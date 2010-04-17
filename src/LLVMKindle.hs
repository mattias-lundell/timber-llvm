{-# LANGUAGE GADTs, NoMonomorphismRestriction #-}

module LLVMKindle where

import LLVM

import Common
import Kindle hiding (unit)
import Name hiding (name)
import Control.Monad.State
import qualified Data.Map as Map
import Data.List hiding (lookup, words)
import Prelude hiding (lookup, words)
import qualified Data.DList as DL

type CodeGen a = StateT CGF (State CGM) a

runCodeGen name = startCgm (cgm0 name) . startCgf (cgf0 "" Map.empty)

startCgf :: Monad m => state -> StateT state m a -> m a
startCgf = flip evalStateT
startCgm :: state -> State state a -> a
startCgm = flip evalState

cgmGets   = lift . gets
cgmModify = lift . modify

cgfGets   = gets
cgfModify = modify
cgfPut    = put

-- | CGM - CodeGenModule
--   State containing module information
data CGM = CGM {
      -- name of the module
      cgmName       :: String,
      -- external variables
      cgmGlobals    :: Map.Map String LLVMValue,
      -- toplevel constants, e.g. strings
      cgmConstants  :: Map.Map String LLVMTopLevelConstant,
      -- type of all functions
      cgmFunEnv     :: Map.Map String LLVMType,
      -- external functions
      cgmEFunctions :: DL.DList LLVMFunctionDecl,
      -- functions already processed
      cgmLFunctions :: DL.DList LLVMFunction,
      -- named types
      cgmTypeDefs   :: Map.Map String LLVMStructDef,
      -- lookup type of a field in a struct
      cgmStructs    :: Map.Map LLVMType (Map.Map String (Int,LLVMType)),
      -- next unique string id
      cgmStringId   :: Integer
    }

cgm0 name = CGM {
      cgmName       = name,
      cgmGlobals    = Map.empty,
      cgmConstants  = Map.empty,
      cgmFunEnv     = Map.empty,
      cgmEFunctions = DL.empty,
      cgmLFunctions = DL.empty,
      cgmTypeDefs   = Map.empty,
      cgmStructs    = Map.empty,
      cgmStringId   = 0
    }

-- | CGF - CodeGenFunction
--   State containing function information
data CGF = CGF {
      -- name of function
      cgfName          :: String,
      -- next fresh register
      cgfNextRegister  :: Integer,
      -- next fresh label
      cgfNextLabel     :: Integer,
      -- handle loop lables, used when encounter continue statement
      cgfContinueLabel :: [LLVMLabel],
      -- handle loop lables, used when encounter break statement
      cgfBreakLabel    :: [LLVMLabel],
      -- current code
      cgfCode          :: DL.DList LLVMInstruction,
      -- map from variable to register
      cgfVarEnv        :: Map.Map String LLVMValue
    }

cgf0 name globals = CGF {
      cgfName          = name,
      cgfNextRegister  = 0,
      cgfNextLabel     = 0,
      cgfContinueLabel = [],
      cgfBreakLabel    = [],
      cgfCode          = DL.empty,
      cgfVarEnv        = globals
    }

-- =============================================================================
-- Getters and setters
-- =============================================================================

getModule :: CodeGen LLVMModule
getModule = do
  name <- cgmGets cgmName
  td   <- cgmGets cgmTypeDefs
  gs   <- cgmGets cgmGlobals
  fds  <- cgmGets cgmEFunctions
  cs   <- cgmGets cgmConstants
  fs   <- cgmGets cgmLFunctions
  return $ LLVMModule name (Map.elems td) (filterLocalGC gs cs) 
            (DL.toList fds) (Map.elems cs) (DL.toList fs)
         where
           -- remove local gcinfo from globals
           filterLocalGC gs cs = 
               Map.elems (Map.filterWithKey 
                          (\x _ -> x `notElem` localGC cs) gs)
           -- get keys of all local gcinfo
           localGC cs = Map.keys . Map.filterWithKey 
                        (\x _ -> "__GC__" `isPrefixOf` x) $ cs

getFunType :: String -> CodeGen LLVMType
getFunType name = cgmGets cgmFunEnv >>= lookup name

addFunType :: String -> LLVMType -> CodeGen ()
addFunType name typ = cgmModify (\s -> s { cgmFunEnv = Map.insert name typ (cgmFunEnv s) })

getString :: String -> CodeGen LLVMValue
getString s = do
  cs <- cgmGets cgmConstants 
  (LLVMTopLevelConstant reg _ _) <- lookup s cs
  getarrayelemptr [intConst 0] reg

getLocalGC :: String -> CodeGen LLVMValue
getLocalGC styp = do 
    cs <- cgmGets cgmConstants
    (LLVMTopLevelConstant reg _ _) <- lookup styp cs
    return reg

addCurrFunction :: LLVMType -> [(String,LLVMType)] -> CodeGen ()
addCurrFunction rettyp paramtypes = do
  name <- cgfGets cgfName
  code <- cgfGets cgfCode
  let fun = LLVMFunction name  Nothing rettyp paramtypes (DL.toList code)
  cgmModify (\s -> s { cgmLFunctions = DL.snoc (cgmLFunctions s) fun })

addGlobalVar :: String -> LLVMValue -> CodeGen ()
addGlobalVar name reg  = 
    cgmModify (\s -> s { cgmGlobals = Map.insert name reg (cgmGlobals s) })

getNextLabel :: CodeGen LLVMLabel
getNextLabel = do
  label <- cgfGets cgfNextLabel
  cgfModify (\s -> s { cgfNextLabel = cgfNextLabel s + 1 })
  return $ Label label

emit :: LLVMInstruction -> CodeGen ()
emit c = cgfModify (\s -> s { cgfCode = DL.snoc (cgfCode s) c }) 

setMname :: String -> CodeGen ()
setMname n = cgmModify (\s -> s {cgmName = n})

setFname :: String -> CodeGen ()
setFname n = cgfModify (\s -> s {cgfName = n})

lookup k m = case Map.lookup k m of
               Just a -> return a
               Nothing -> internalError0 $ "key : " ++ show k ++ "\n not found in:\n" ++ show m

getNewReg :: LLVMType -> CodeGen LLVMValue
getNewReg typ = do
  register <- gets cgfNextRegister
  cgfModify (\s -> s { cgfNextRegister = cgfNextRegister s + 1 })
  fname <- cgfGets cgfName
  return $ LLVMRegister typ ("reg" ++  show register) TagLocal

getNewNamedReg :: String -> LLVMType -> CodeGen LLVMValue
getNewNamedReg name typ = return $ LLVMRegister typ name TagLocal

addCurrFunAndClean :: String -> LLVMType -> [(String,LLVMType)] -> CodeGen ()
addCurrFunAndClean name rettyp paramtypes = do
  code <- cgfGets cgfCode
  let fun = LLVMFunction name Nothing rettyp paramtypes (DL.toList code)
  cgmModify (\s -> s { cgmLFunctions = DL.snoc (cgmLFunctions s) fun })

addExternalFun :: String -> LLVMType -> [LLVMType] -> CodeGen ()
addExternalFun name rettyp argtyps = do
  let funtyp  = Tfun rettyp argtyps
      fundecl = LLVMFunctionDecl [] name funtyp
  cgmModify (\s -> s { cgmFunEnv = Map.insert name funtyp (cgmFunEnv s) }) 
  cgmModify (\s -> s { cgmEFunctions = DL.snoc (cgmEFunctions s) fundecl })

addExternalGC :: String -> LLVMType -> CodeGen ()
addExternalGC name typ = do
  let var = LLVMRegister typ ("__GC__" ++ name) (TagGlobal [External,Global] Nothing)
  cgmModify (\s -> s { cgmGlobals = Map.insert ("__GC__" ++ name) var (cgmGlobals s) })

addLocalGC :: LLVMType -> String -> [LLVMValue] -> CodeGen ()
addLocalGC typ name vals = do
  let reg'  = LLVMRegister typ ("__GC__" ++ name) (TagGlobal [] Nothing)
      reg'' = LLVMRegister (ptr typ) ("__GC__" ++ name) (TagGlobal [] Nothing)
      var'  = LLVMConstant typ (ArrayConst vals)
      con'  = LLVMTopLevelConstant reg' [Global] var'
  cgmModify (\s -> s { cgmGlobals   = Map.insert ("__GC__" ++ name) reg' (cgmGlobals s) })
  cgmModify (\s -> s { cgmConstants = Map.insert ("__GC__" ++ name) con' (cgmConstants s) })

-- add strings to toplevel constants
addString :: String -> CodeGen ()
addString str = do
  constants <- cgmGets cgmConstants
  case Map.lookup str constants of
    Nothing -> do
      sid <- cgmGets cgmStringId
      cgmModify (\s -> s { cgmStringId = cgmStringId s + 1 })
      let reg' = LLVMRegister (ptr (Tarray (length str + 1) char)) ("str" ++ show sid) (TagGlobal [] Nothing)
          var' = LLVMConstant (Tarray (length str + 1) char) (StringConst str)
          con' = LLVMTopLevelConstant reg' [Private,Constant] var'
      cgmModify (\s -> s { cgmConstants = Map.insert str con' (cgmConstants s) })
    Just _ -> return ()

-- add stack allocations, all stack allocations are made in the beginning of each function.
-- this way we can take advantage of the "mem2reg" pass
addAlloca :: LLVMValue -> CodeGen ()
addAlloca reg = do
  let typ = drop1Ptr . getTyp $ reg
      instr = Alloca reg typ
  cgfModify (\s -> s { cgfCode = DL.cons instr (cgfCode s) })

-- get size of a struct, measured in WORD
getStructSize :: LLVMType -> CodeGen Int
getStructSize styp = do
  sdefs <- cgmGets cgmStructs
  sdef <- lookup (dropPtrs styp) sdefs
  calcStructSize (snd . unzip $ Map.toList sdef)

-- get index of a field inside a struct
getStructIndex :: LLVMType -> String -> CodeGen (Int,LLVMType)
getStructIndex name var = do
  structs <- cgmGets cgmStructs
  struct <- lookup (dropPtrs name) structs
  lookup var struct

addTopLevelConstant :: String -> LLVMValue -> [LLVMLinkage] -> LLVMValue -> CodeGen ()
addTopLevelConstant name var linkage val = do
  let con' = LLVMTopLevelConstant var linkage val
  cgmModify (\s -> s { cgmConstants = Map.insert name con' (cgmConstants s) })

-- add a struct definition to env
addStruct :: String -> [(String,LLVMType)] -> CodeGen ()
addStruct name tups = do
  structs <- cgmGets cgmStructs
  let (vars,types) = unzip tups
  cgmModify (\s -> s { cgmStructs = Map.insert (Tstruct name) (Map.fromList (zip vars (zip [0..] types))) structs })
  let sd = LLVMStructDef (LLVMRegister (Tstruct name) name TagLocal) types
  cgmModify (\s -> s { cgmTypeDefs = Map.insert name sd (cgmTypeDefs s) })

-- calculate offset inside a struct, measured in WORD
getStructOffset :: LLVMType -> String -> CodeGen Int
getStructOffset styp fname = do
  structs <- cgmGets cgmStructs
  struct <- lookup styp structs
  (offset,_) <- getStructIndex styp fname
  calcStructSize (take offset (sort ((snd.unzip) (Map.toList struct))))

-- used for "continue" statements
getContinueLabel :: CodeGen LLVMLabel
getContinueLabel = fmap head (cgfGets cgfContinueLabel)

addContinueLabel :: LLVMLabel -> CodeGen ()
addContinueLabel lab = cgfModify (\s -> s { cgfContinueLabel = lab : cgfContinueLabel s })

dropContinueLabel :: CodeGen ()
dropContinueLabel = cgfModify (\s -> s { cgfContinueLabel = tail (cgfContinueLabel s) })

-- used for "break" statements
getBreakLabel :: CodeGen LLVMLabel
getBreakLabel = fmap head (cgfGets cgfBreakLabel)

addBreakLabel :: LLVMLabel -> CodeGen ()
addBreakLabel lab = cgfModify (\s -> s { cgfBreakLabel = lab : cgfBreakLabel s })

dropBreakLabel :: CodeGen ()
dropBreakLabel = cgfModify (\s -> s { cgfBreakLabel = tail (cgfBreakLabel s) })

addVar :: String -> LLVMValue -> CodeGen ()
addVar var reg = 
    modify (\s -> s { cgfVarEnv = Map.insert var reg (cgfVarEnv s) })

getVar :: String -> CodeGen LLVMValue
getVar var = do
  env <- cgfGets cgfVarEnv
  return (fromJust (Map.lookup var env))

lookupVar :: String -> CodeGen (Maybe LLVMValue)
lookupVar var = do
  env <- cgfGets cgfVarEnv
  return (Map.lookup var env)

-- get result type of a function
getResType :: String -> CodeGen LLVMType
getResType name = do
  funs <- cgmGets cgmFunEnv
  ftyp <- lookup name funs
  return (rettyp ftyp)
         where
           rettyp (Tptr (Tfun r _)) = r
           rettyp (Tfun r _) = r

getExternalGC :: String -> CodeGen LLVMValue
getExternalGC styp = cgmGets cgmGlobals >>= lookup styp

-- =============================================================================
-- HELPERS
-- =============================================================================

calcStructSize [] = return 0
calcStructSize ((_,typ):rest) = do 
  size <- typeSize typ 
  rest <- calcStructSize rest 
  return $ size + rest

words :: Int -> Int
words bytes = Prelude.div (bytes+4-1) 4

typeSize :: LLVMType -> CodeGen Int
typeSize (Tint n) = return (n `Prelude.div` 8)
typeSize (Tptr _) = return 4
typeSize Tfloat = return 4
typeSize (Tarray n typ) = do
  typsize <- typeSize typ
  return $ n*typsize
typeSize styp@(Tstruct sname) = getStructSize styp
typeSize _       = return 4

-- =============================================================================
-- Kindle types represented as LLVM types
-- =============================================================================

int :: LLVMType
int = Tint 32

bit32 :: LLVMType
bit32 = Tint 32

bit16 :: LLVMType
bit16 = Tint 16

bit8 :: LLVMType
bit8 = Tint 8

char :: LLVMType
char = Tint 8

float :: LLVMType
float = Tfloat

bool :: LLVMType
bool = Tint 1

word :: LLVMType
word = Tint 32

poly :: LLVMType
poly = ptr word

void :: LLVMType
void = Tvoid

unit :: LLVMType
unit = bit8

ptr :: LLVMType -> LLVMType
ptr = Tptr

fun :: LLVMType -> [LLVMType] -> LLVMType
fun = Tfun

struct :: String -> LLVMType
struct = Tstruct

union :: [LLVMType] -> LLVMType
union = Tunion

array :: Int -> LLVMType -> LLVMType
array = Tarray

opaque :: LLVMType
opaque = Topaque

timestruct   :: LLVMType
timestruct   = ptr $ Tstruct "Time"

liststruct   :: LLVMType
liststruct   = ptr $ Tstruct "LIST"
consstruct   :: LLVMType
consstruct   = ptr $ Tstruct "CONS"

clos1struct  :: LLVMType
clos1struct  = ptr $ Tstruct "CLOS1"
clos2struct  :: LLVMType
clos2struct  = ptr $ Tstruct "CLOS2"
clos3struct  :: LLVMType
clos3struct  = ptr $ Tstruct "CLOS3"
closstruct   :: LLVMType
closstruct   = ptr $ Tstruct "CLOS"

refstruct    :: LLVMType
refstruct    = ptr $ Tstruct "Ref"

msgstruct    :: LLVMType
msgstruct    = ptr $ Tstruct "Msg"

tup0struct   :: LLVMType
tup0struct   = bit8
tup2struct   :: LLVMType
tup2struct   = ptr $ Tstruct "TUP2"
tup3struct   :: LLVMType
tup3struct   = ptr $ Tstruct "TUP3"
tup4struct   :: LLVMType
tup4struct   = ptr $ Tstruct "TUP4"
tuplestruct  :: LLVMType
tuplestruct  = ptr $ Tstruct "TUPLE"

eitherstruct :: LLVMType
eitherstruct = ptr $ Tstruct "EITHER"
leftstruct   :: LLVMType
leftstruct   = ptr $ Tstruct "LEFT"
rightstruct  :: LLVMType
rightstruct  = ptr $ Tstruct "RIGHT"

worldstruct  :: LLVMType
worldstruct  = ptr $ Tstruct "WORLD"
arraystruct  :: LLVMType
arraystruct  = ptr $ Tstruct "Array"
timerstruct  :: LLVMType
timerstruct  = ptr (Tstruct "Timer")

isPrimStruct :: LLVMType -> Bool
isPrimStruct typ = typ `elem` ls
                   where ls = map dropPtrs [timestruct, liststruct, consstruct,
                                            clos1struct, clos2struct, clos3struct,
                                            closstruct, refstruct, msgstruct,
                                            tup0struct, tup2struct, tup3struct,
                                            tup4struct, tuplestruct, eitherstruct,
                                            leftstruct, rightstruct, worldstruct]

isLocalStruct :: LLVMType -> CodeGen Bool
isLocalStruct (Tstruct styp) = do
  env <- cgmGets cgmConstants
  return (Map.member styp env)

isPtr :: LLVMType -> Bool
isPtr (Tptr _) = True
isPtr _        = False

drop1Ptr :: LLVMType -> LLVMType
drop1Ptr (Tptr t) = t
drop1Ptr t       = t

dropPtrs :: LLVMType -> LLVMType
dropPtrs (Tptr t) = dropPtrs t
dropPtrs t       = t

-- =============================================================================
-- Create LLVM constants of common types
-- =============================================================================

true :: LLVMValue
true = LLVMConstant bool (BoolConst True)
false :: LLVMValue
false = LLVMConstant bool (BoolConst False)
floatConst :: Rational -> LLVMValue
floatConst x = LLVMConstant float (FloatConst x) 
intConst :: (Integral a) => a -> LLVMValue
intConst n = LLVMConstant int (IntConst (read (show n)::Int)) 
charConst :: Char -> LLVMValue
charConst c = LLVMConstant char (CharConst c)
voidConst :: LLVMValue
voidConst = LLVMConstant void VoidConst
vectorConst :: [LLVMValue] -> LLVMValue
vectorConst xs = LLVMConstant (Tvector (length xs) (getTyp $ head xs)) (VectorConst xs)
bit32Const :: (Integral a) => a -> LLVMValue
bit32Const n = LLVMConstant bit32 (IntConst (read (show n)::Int))
bit16Const :: (Integral a) => a -> LLVMValue
bit16Const n = LLVMConstant bit16 (IntConst (read (show n)::Int))
bit8Const :: (Integral a) => a -> LLVMValue
bit8Const n = LLVMConstant bit8 (IntConst (read (show n)::Int))
stringConst :: String -> LLVMValue
stringConst s = LLVMConstant (Tarray (length s) char) (StringConst s)

-- =============================================================================
-- INSTRUCTIONS
-- =============================================================================

emitBinaryOp instr op1 op2 = do
  r1 <- getNewReg (getTyp op1)
  emit $ instr r1 op1 op2
  return r1

emitCmpOp instr cmpop op1 op2 = do
  r1 <- getNewReg bool
  emit $ instr r1 cmpop op1 op2
  return r1

emitCast instr typ op1 = do
  r1 <- getNewReg typ
  emit $ instr r1 op1 typ
  return r1

add  = emitBinaryOp Add
sub  = emitBinaryOp Sub
mul  = emitBinaryOp Mul
div  = emitBinaryOp Sdiv
rem  = emitBinaryOp Srem
and  = emitBinaryOp And
or   = emitBinaryOp Or
xor  = emitBinaryOp Xor
shl  = emitBinaryOp Shl
lshr = emitBinaryOp Lshr
ashr = emitBinaryOp Ashr
fadd = emitBinaryOp Fadd
fsub = emitBinaryOp Fsub
fmul = emitBinaryOp Fmul
fdiv = emitBinaryOp Fdiv
frem = emitBinaryOp Frem

icmp = emitCmpOp Icmp
fcmp = emitCmpOp Fcmp

bitcast  = emitCast Bitcast 
ptrtoint = emitCast Ptrtoint
inttoptr = emitCast Inttoptr
sitofp   = emitCast Sitofp
fptosi   = emitCast Fptosi
trunc typ@(Tint n) op1 = do
  let (Tint n') = getTyp op1
  if n == n' 
     then return op1 
     else do
       r1 <- getNewReg typ
       emit $ Trunc r1 op1 typ
       return r1
zext typ@(Tint n) op1 = do
  let (Tint n') = getTyp op1
  if n == n' 
     then return op1 
     else do
       r1 <- getNewReg typ
       emit $ Zext r1 op1 typ
       return r1

load op1 = do
  r1 <- getNewReg ((drop1Ptr.getTyp) op1)
  emit $ Load r1 op1
  return r1
store op1 op2 = emit $ Store op1 op2
label lab = emit $ Lab lab

br lab = emit $ Uncondbr lab
condbr l1 l2 op1 = emit $ Condbr op1 l1 l2

unreachable = emit Unreachable

ret = emit . Ret
retvoid = ret voidConst
alloca typ = do
  r1 <- getNewReg (Tptr typ)
  addAlloca r1
  return r1
call fname exps = do
  resTyp <- getResType fname
  r1 <- getNewReg resTyp
  emit $ Call (Just r1) resTyp fname exps
  return r1
callvoid fname exps = emit $ Call Nothing Tvoid fname exps
callhigher freg ftyp exps = do
  let (Tfun rtyp _) = dropPtrs ftyp
  r2 <- getNewReg rtyp
  emit $ Callhigher r2 ftyp freg exps
  return r2
getelementptr typ offset op1 = do
  r1 <- getNewReg (ptr typ)
  emit $ Getelementptr r1 op1 offset
  return r1
getstructelemptr name op1 = do
  let styp = getTyp op1
  (offset,etyp) <- getStructIndex (dropPtrs styp) name
  r1 <- getNewReg (ptr etyp)
  emit $ Getelementptr r1 op1 [intConst offset]
  return r1
getarrayelemptr index op1 = do
  let (Tarray _ etyp) = dropPtrs $ getTyp op1
  r1 <- getNewReg (ptr etyp)
  emit $ Getelementptr r1 op1 index
  return r1
extractelement op1 op2 = do
  let (Tvector _ typ) = getTyp op1
  r1 <- getNewReg typ
  emit $ Extractelement r1 op1 op2
  return r1           
insertelement  op1 op2 op3 = do
  r1 <- getNewReg (getTyp op1)
  emit $ Insertelement r1 op1 op2 op3
  return r1

-- Transform kindle-types into a llvm representation
k2llvmType :: AType -> LLVMType
k2llvmType typ = case typ of
                   TCon (Prim Int _) _        -> int
                   TCon (Prim Float _) _      -> float
                   TCon (Prim Bool _) _       -> bool
                   TCon (Prim Char _) _       -> char
                   TCon (Prim WORD _) _       -> word
                   TCon (Prim POLY _) _       -> poly
                   TCon (Prim OID _) _        -> ptr int
                   TCon (Prim LIST _) _       -> liststruct
                   TCon (Prim CONS _) _       -> consstruct
                   TCon (Prim NIL _) _        -> liststruct
                   TCon (Prim BITS32 _) _     -> bit32
                   TCon (Prim BITS16 _) _     -> bit16
                   TCon (Prim BITS8 _) _      -> bit8
                   TCon (Prim CLOS1 _) _      -> clos1struct
                   TCon (Prim CLOS2 _) _      -> clos2struct
                   TCon (Prim CLOS3 _) _      -> clos3struct
                   TCon (Prim CLOS _) _       -> closstruct
                   TCon (Prim Ref _) _        -> refstruct
                   TCon (Prim Msg _) _        -> msgstruct
                   TCon (Prim Time _) _       -> timestruct
                   TCon (Prim TIMERTYPE _) _  -> timerstruct
                   TCon (Prim AbsTime _) _    -> struct "AbsTime"
                   TCon (Prim World _) _      -> worldstruct 
                   TCon (Prim Array _) _      -> arraystruct
                   TCon (Tuple 0 _) _         -> tup0struct
                   TCon (Tuple 2 _) _         -> tup2struct
                   TCon (Tuple 3 _) _         -> tup3struct
                   TCon (Tuple 4 _) _         -> tup4struct
                   TCon (Tuple n _) _         -> tuplestruct
                   TCon (Prim EITHER _)_      -> eitherstruct
                   TCon (Prim LEFT _)_        -> leftstruct
                   TCon (Prim RIGHT _)_       -> rightstruct
                   TCon name@(Name _ _ _ _) _ -> ptr $ struct (k2llvmName name)
                   _                          -> error $ "ERROR IN k2llvmType: " ++ show typ   

-- Transform Name data to string
k2llvmName :: Name -> String
k2llvmName (Name s t (Just _) a)
  | not (public a)            = s ++ '_':show t
k2llvmName (Name s t _ _)
  | t == 0 = s
k2llvmName (Name s t m a)       = id ++ tag ++ mod
  where 
    id                          = if okForC s then s else "_sym"
    tag                         = if mod=="" || generated a || id=="_sym" then '_':show t else ""
    mod                         = maybe "" (('_' :) . modToundSc) m
k2llvmName (Prim CONS _)        = "CONS"
k2llvmName n                    = show n
