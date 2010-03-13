{-# LANGUAGE GADTs #-}

module LLVMKindle where

import LLVM

import Common 
import Kindle hiding (unit)
import Name hiding (name)
import Control.Monad.State hiding (get, put)
import qualified Data.Map as Map
import Data.List hiding (lookup, words)
import Prelude hiding (lookup, words)
import qualified Data.DList as DL

data CodeGenState = CodeGenState {
      cgNextRegister :: Integer,
      cgNextLabel :: Integer,
      cgTypeEnv :: [Map.Map String LLVMValue],
      cgCurrModuleName :: String,
      cgCurrFunName :: String,
      cgCurrLoopLabel :: [LLVMLabel],

      cgGlobals :: Map.Map String LLVMValue,
      cgGs      :: [LLVMGlobal], 
      cgFuns    :: Map.Map String LLVMType,
      cgFDs      :: [LLVMFunctionDecl],
      cgConsts  :: Map.Map String LLVMValue,
      cgCs      :: [LLVMTopLevelConstant],
      cgStructs :: Map.Map LLVMType (Map.Map String (Int,LLVMType)),
      cgSs      :: [LLVMStructDef],
      cgFs      :: [LLVMFunction],
      cgCurrF   :: DL.DList LLVMInstruction
}

type CGState a = State CodeGenState a
type Selector a = (CGState a, a -> CGState ())

-- =============================================================================
-- SELECTORS
-- =============================================================================

globals :: Selector (Map.Map String LLVMValue)
globals = (gets cgGlobals, \x -> 
                  modify (\vs -> vs {cgGlobals = x}))

gs :: Selector [LLVMGlobal]
gs = (gets cgGs, \x -> 
          modify (\vs -> vs {cgGs = x}))

funs :: Selector (Map.Map String LLVMType)
funs = (gets cgFuns, \x -> 
                  modify (\vs -> vs {cgFuns = x}))

fds :: Selector [LLVMFunctionDecl]
fds = (gets cgFDs, \x -> 
          modify (\vs -> vs {cgFDs = x}))

fs :: Selector [LLVMFunction]
fs = (gets cgFs, \x -> 
          modify (\vs -> vs {cgFs = x}))

consts :: Selector (Map.Map String LLVMValue)
consts = (gets cgConsts, \x -> 
                  modify (\vs -> vs {cgConsts = x}))

cs :: Selector [LLVMTopLevelConstant]
cs = (gets cgCs, \x -> 
          modify (\vs -> vs {cgCs = x}))

ss :: Selector [LLVMStructDef]
ss = (gets cgSs, \x -> 
          modify (\vs -> vs {cgSs = x}))

currf :: Selector (DL.DList LLVMInstruction)
currf = (gets cgCurrF, \x -> 
                  modify (\vs -> vs {cgCurrF = x}))

currLoopLabel :: Selector [LLVMLabel]
currLoopLabel = (gets cgCurrLoopLabel, \x -> 
                  modify (\vs -> vs {cgCurrLoopLabel = x}))

nextRegister :: Selector Integer
nextRegister = (gets cgNextRegister, \x -> 
                    modify (\vs -> vs {cgNextRegister = x}))

nextLabel :: Selector Integer
nextLabel = (gets cgNextLabel, \x -> 
                 modify (\vs -> vs {cgNextLabel = x}))

structs :: Selector (Map.Map LLVMType (Map.Map String (Int,LLVMType)))
structs = (gets cgStructs, \x -> 
               modify (\vs -> vs {cgStructs = x}))

typeEnv :: Selector [Map.Map String LLVMValue]            
typeEnv = (gets cgTypeEnv, \x -> 
               modify (\vs -> vs {cgTypeEnv = x}))

currModuleName :: Selector String
currModuleName = (gets cgCurrModuleName, \x -> 
                      modify (\vs -> vs {cgCurrModuleName = x}))

currFunName :: Selector String
currFunName = (gets cgCurrFunName, \x -> 
                      modify (\vs -> vs {cgCurrFunName = x}))

-- =============================================================================
-- GETTERS AND SETTERS
-- =============================================================================

-- used for "continue"-call in a while loop
getLoopLabel :: CGState LLVMLabel
getLoopLabel = get currLoopLabel >>= return . head

addLoopLabel :: LLVMLabel -> CGState ()
addLoopLabel lab = modify $ \s -> s { cgCurrLoopLabel = lab : cgCurrLoopLabel s }

dropLoopLabel :: CGState ()
dropLoopLabel = modify $ \s -> s { cgCurrLoopLabel = tail (cgCurrLoopLabel s) }

-- add strings to toplevel constants
addString :: String -> CGState ()
addString s = do
  r <- get nextRegister
  let reg' = LLVMRegister (ptr (Tarray (length s + 1) char)) ("str" ++ show r) True
  let var' = LLVMConstant (Tarray (length s + 1) char) (StringConst s)
  let con' = LLVMTopLevelConstant reg' [Private,Constant] var'
  consts' <- get consts
  cs' <- get cs
  put consts (const $ Map.insert s reg' consts')
  put cs (const (con':cs'))

-- get address to a stored string
getString :: String -> CGState LLVMValue
getString s = do
  cs <- get consts
  reg <- lookup "getString" s cs
  getarrayelemptr [intConst 0] reg

-- add stack allocations, all stack allocations are made in the beginning of each function.
-- this way we can take advantage of the "mem2reg" pass
addAlloca :: LLVMValue -> CGState ()
addAlloca reg = do
  let typ = drop1Ptr . getTyp $ reg
      instr = Alloca reg typ
  f <- get currf
  put currf (const (DL.cons instr f))
  
addGlobalVar :: String -> LLVMValue -> [LLVMLinkage] -> Maybe LLVMGlobalInitializer -> CGState ()
addGlobalVar name arg linkage init = do
  globals' <- get globals
  gs' <- get gs
  let var' = LLVMRegister (drop1Ptr $ getTyp arg) name True
      glo' = LLVMGlobal var' linkage init
  put globals (const $ Map.insert name arg globals')
  put gs (const (glo':gs'))

getFunType :: String -> CGState LLVMType
getFunType name = get funs >>= lookup "getFunType" name

-- get result type of a function
getResType :: String -> CGState LLVMType
getResType name = do
  ftyp <- getFunType name
  return (rettyp ftyp)
         where
           rettyp (Tptr (Tfun r _)) = r
           rettyp (Tfun r _) = r

addFunType :: String -> LLVMType -> CGState ()
addFunType name typ = do
  funs' <- get funs
  put funs (const $ Map.insert name typ funs')

addExternalFun :: String -> LLVMType -> [LLVMType] -> CGState ()
addExternalFun name rettyp argtyps = do
  funs' <- get funs
  fds'   <- get fds
  let funtyp'  = Tfun rettyp argtyps
  let fundecl' = LLVMFunctionDecl [] name funtyp'
  put funs (const $ Map.insert name funtyp' funs')
  put fds (const $ fundecl':fds')

addExternalGC :: String -> LLVMType -> CGState ()
addExternalGC name typ = do
  globals' <- get globals
  gs' <- get gs
  let var = LLVMRegister (ptr typ) ("__GC__" ++ name) True
      var' = LLVMRegister typ ("__GC__" ++ name) True
      glo' = LLVMGlobal var' [External,Global] Nothing
  put globals (const $ Map.insert name var globals')
  put gs (const $ glo':gs')

addGCArray :: LLVMType -> String -> [LLVMValue] -> CGState ()
addGCArray typ name vals = do
  consts' <- get consts
  cs' <- get cs
  let reg' = LLVMRegister typ ("__GC__" ++ name) True
      reg'' = LLVMRegister (ptr typ) ("__GC__" ++ name) True
      var' = LLVMConstant typ (ArrayConst vals)
      con' = LLVMTopLevelConstant reg' [Global] var'
  put consts (const $ Map.insert name reg'' consts')
  put cs (const $ con':cs')

addTopLevelConstant :: LLVMValue -> LLVMValue -> [LLVMLinkage] -> CGState ()
addTopLevelConstant var val linkage = do
  cs' <- get cs
  let con' = LLVMTopLevelConstant var linkage val
  put cs (const $ con':cs')

getLocalGC :: String -> CGState LLVMValue
getLocalGC styp = get consts >>= lookup "getLocalGC" styp

getExternalGC :: String -> CGState LLVMValue
getExternalGC styp = get globals >>= lookup "getGloabalGC" styp

setModuleName :: String -> CGState ()
setModuleName name = put currModuleName (const name)

setFunName :: String -> CGState ()
setFunName name = put currFunName (const name)

getFunName :: CGState String
getFunName = get currFunName

getNewOpReg :: LLVMType -> CGState LLVMValue
getNewOpReg typ = do
  register <- get nextRegister
  put nextRegister (+1)
  return $ LLVMRegister typ ("reg" ++ show register) False

getNamedOpReg :: String -> LLVMType -> CGState LLVMValue
getNamedOpReg rname typ = do
  fname <- getFunName
  return $ LLVMRegister typ rname False
            
getNewResReg :: LLVMType -> CGState LLVMValue
getNewResReg typ = do
  register <- get nextRegister
  put nextRegister (+1)
  fname <- getFunName
  return $ LLVMRegister typ ("reg" ++  show register) False

getNextLabel :: CGState LLVMLabel
getNextLabel = do
  label <- get nextLabel
  put nextLabel (+1)
  return $ Label label

enterNewScope :: CGState ()
enterNewScope = do
  env <- get typeEnv
  vars <- get globals
  put typeEnv (const $ vars:env)

leaveScope :: CGState ()
leaveScope = do
  env <- get typeEnv
  put typeEnv (const $ tail env) 

addVarToEnv :: String -> LLVMType -> CGState LLVMValue
addVarToEnv var typ = do
  r1 <- getNewResReg (ptr typ)
  (scope:rest) <- get typeEnv
  put typeEnv (const $ Map.insert var r1 scope : rest)
  return r1  

addVariableToScope :: String -> LLVMValue -> CGState ()
addVariableToScope var reg = do
  (scope:rest) <- get typeEnv
  put typeEnv (const $ Map.insert var reg scope : rest)
 
addParamToScope :: (String,LLVMValue) -> CGState ()
addParamToScope (var,reg) = do
  (scope:rest) <- get typeEnv
  put typeEnv (const (Map.insert var reg scope:rest))

getVariable :: String -> CGState LLVMValue
getVariable var = do
  (scope:rest) <- get typeEnv
  lookup "getVariable" var scope

lookupVar :: String -> CGState (Maybe LLVMValue)
lookupVar var = do
  (scope:rest) <- get typeEnv
  return $ Map.lookup var scope

getModule :: CGState LLVMModule
getModule = do
  name <- get currModuleName
  td'  <- get ss
  gs'  <- get gs
  fds' <- get fds
  cs'  <- get cs
  fs'  <- get fs
  return $ LLVMModule name td' gs' fds' cs' fs'

emits :: [LLVMInstruction] -> CGState ()
emits = mapM_ emit

emit :: LLVMInstruction -> CGState ()
emit s = do
  f <- get currf
  put currf (const (DL.snoc f s))

addCurrFunction :: String -> LLVMType -> [(String,LLVMType)] -> CGState ()
addCurrFunction name rettyp paramtypes = do
  code <- get currf
  fs' <- get fs
  let fun = LLVMFunction name rettyp paramtypes (DL.toList code)
  put fs (const $ fun:fs')
  put currf (const DL.empty)

-- get index of a field inside a struct
getStructIndex :: LLVMType -> String -> CGState (Int,LLVMType)
getStructIndex name var = 
  get structs >>= lookup "getStructIndex" (dropPtrs name) >>= lookup "getStructIndex"  var

-- get size of a struct, measured in WORD
getStructSize :: LLVMType -> CGState Int
getStructSize styp = do
  ss <- get structs
  s <- lookup "getStructSize" (dropPtrs styp) ss
  calcStructSize ((snd.unzip) (Map.toList s))

-- calculate offset inside a struct, measured in WORD
getStructOffset :: LLVMType -> String -> CGState Int
getStructOffset styp fname = do
  ss <- get structs
  s <- lookup "getStructOffset" styp ss
  (offset,_) <- getStructIndex styp fname
  calcStructSize (take offset (sort ((snd.unzip) (Map.toList s))))

-- add a struct definition to env
addStruct :: String -> [(String,LLVMType)] -> CGState ()
addStruct name tups = do
  s <- get structs
  let (vars,types) = unzip tups
  put structs (const $ Map.insert (Tstruct name) (Map.fromList (zip vars (zip [0..] types))) s)
  let sd = LLVMStructDef (LLVMRegister (Tstruct name) name False) types
  ss' <- get ss
  put ss (const (sd:ss'))

lookup s k m = case Map.lookup k m of
    Just v -> return v
    _      -> fail $ "source: " ++ s ++ "\nget: " ++ show k ++ " from: " ++ show m

-- =============================================================================
-- HELPERS
-- =============================================================================

--calcStructSize :: forall a . [(a,LLVMType)] -> CGState Int
calcStructSize [] = return 0
calcStructSize ((_,typ):rest) = do 
  size <- typeSize typ 
  rest <- calcStructSize rest 
  return $ size + rest

words :: Int -> Int
words bytes = Prelude.div (bytes+4-1) 4

typeSize :: LLVMType -> CGState Int
typeSize (Tint n) = return (n `Prelude.div` 8)
typeSize (Tptr _) = return 4
typeSize Tfloat = return 4
typeSize styp@(Tstruct sname) = getStructSize styp
typeSize _       = return 4

-- =============================================================================
-- MONAD GET AND PUT
-- =============================================================================

get :: Selector a -> CGState a
get = fst

put :: Selector a -> (a -> a) -> CGState ()
put (gf,uf) f = do
  st <- gf
  uf (f st)

-- =============================================================================
-- INITIAL STATE
-- =============================================================================

s0 :: CodeGenState
s0 = CodeGenState {
       cgNextRegister = 0,
       cgNextLabel = 0,
       cgTypeEnv = [Map.empty],
       cgCurrModuleName = "",
       cgCurrFunName = "",
       cgCurrLoopLabel = [],

       cgGlobals = Map.empty,
       cgGs      = [],
       cgFuns    = Map.empty,
       cgFDs     = [],
       cgConsts  = Map.empty,
       cgCs      = [],
       cgStructs = Map.empty,
       cgSs      = [],
       cgFs      = [],
       cgCurrF   = DL.empty
     }


----------------
---------------
--------------------

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

isPtr :: LLVMType -> Bool
isPtr (Tptr _) = True
isPtr _        = False

fun :: LLVMType -> [LLVMType] -> LLVMType
fun = Tfun

struct :: String -> LLVMType
struct = Tstruct

drop1Ptr :: LLVMType -> LLVMType
drop1Ptr (Tptr t) = t
drop1Ptr t       = t

dropPtrs :: LLVMType -> LLVMType
dropPtrs (Tptr t) = dropPtrs t
dropPtrs t       = t

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

isLocalStruct :: LLVMType -> CGState Bool
isLocalStruct (Tstruct styp) = do
  env <- get consts
  return (Map.member styp env)

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
  r1 <- getNewResReg (getTyp op1)
  emit $ instr r1 op1 op2
  return r1

add = emitBinaryOp Add
sub = emitBinaryOp Sub
mul = emitBinaryOp Mul
div = emitBinaryOp Sdiv
rem = emitBinaryOp Srem
and = emitBinaryOp And
or  = emitBinaryOp Or
xor = emitBinaryOp Xor
shl op1 op2 = do
  r1 <- getNewResReg (getTyp op1)
  emit $ Shl r1 op1 op2
  return r1
lshr op1 op2 = do
  r1 <- getNewResReg (getTyp op1)
  emit $ Lshr r1 op1 op2
  return r1
ashr op1 op2 = do
  r1 <- getNewResReg (getTyp op1)
  emit $ Ashr r1 op1 op2
  return r1
load' typ op1 = do
  r1 <- getNewResReg typ
  emit $ Load r1 op1
  return r1
load op1 = do
  r1 <- getNewResReg ((drop1Ptr.getTyp) op1)
  emit $ Load r1 op1
  return r1
store op1 op2 = emit $ Store op1 op2
label lab = emit $ Lab lab
icmp cmpop op1 op2 = do
  let typ = getTyp op1
  r1 <- getNewResReg bool
  emit $ Icmp r1 cmpop op1 op2
  return r1
fcmp cmpop op1 op2 = do
  let typ = getTyp op1
  r1 <- getNewResReg bool
  emit $ Fcmp r1 cmpop op1 op2
  return r1
br lab = emit $ Uncondbr lab
condbr l1 l2 op1 = emit $ Condbr op1 l1 l2
switch op l1 ls = emit $ Switch op l1 ls
unreachable = emit Unreachable
bitcast typ op1 = do
  r1 <- getNewResReg typ
  emit $ Bitcast r1 op1 typ
  return r1
ptrtoint typ op1 = do
  r1 <- getNewResReg typ
  emit $ Ptrtoint r1 op1 typ
  return r1
inttoptr typ op1 = do
  r1 <- getNewResReg typ
  emit $ Inttoptr r1 op1 typ
  return r1
trunc typ@(Tint n) op1 = do
  let (Tint n') = getTyp op1
  if n == n' 
     then return op1 
     else do
       r1 <- getNewResReg typ
       emit $ Trunc r1 op1 typ
       return r1
zext typ@(Tint n) op1 = do
  let (Tint n') = getTyp op1
  if n == n' 
     then return op1 
     else do
       r1 <- getNewResReg typ
       emit $ Zext r1 op1 typ
       return r1
ret = emit . Ret
retvoid = ret voidConst
alloca typ = do
  r1 <- getNewResReg (Tptr typ)
  addAlloca r1
  return r1
call fname exps = do
  resTyp <- getResType fname
  r1 <- getNewResReg resTyp
  emit $ Call (Just r1) resTyp fname exps
  return r1
callvoid fname exps = emit $ Call Nothing Tvoid fname exps
callhigher freg ftyp exps = do
  let (Tfun rtyp _) = dropPtrs ftyp
  r2 <- getNewResReg rtyp
  emit $ Callhigher r2 ftyp freg exps
  return r2
getelementptr typ offset op1 = do
  r1 <- getNewResReg (ptr typ)
  emit $ Getelementptr r1 op1 offset
  return r1
getstructelemptr name op1 = do
  let styp = getTyp op1
  (offset,etyp) <- getStructIndex (dropPtrs styp) name
  r1 <- getNewResReg (ptr etyp)
  emit $ Getelementptr r1 op1 [intConst offset]
  return r1

getarrayelemptr index op1 = do
  let (Tarray _ etyp) = dropPtrs $ getTyp op1
  r1 <- getNewResReg (ptr etyp)
  emit $ Getelementptr r1 op1 index
  return r1

sitofp typ op1 = do
    r1 <- getNewResReg typ
    emit $ Sitofp r1 op1 typ
    return r1
fptosi typ op1 = do
    r1 <- getNewResReg typ
    emit $ Fptosi r1 op1 typ
    return r1

fadd op1 op2 = do
  r1 <- getNewResReg (getTyp op1)
  emit $ Fadd r1 op1 op2
  return r1
fsub op1 op2 = do
  r1 <- getNewResReg (getTyp op1)
  emit $ Fsub r1 op1 op2
  return r1
fmul op1 op2 = do
  r1 <- getNewResReg (getTyp op1)
  emit $ Fmul r1 op1 op2
  return r1
fdiv op1 op2 = do
  r1 <- getNewResReg (getTyp op1)
  emit $ Fdiv r1 op1 op2
  return r1
frem op1 op2 = do
  r1 <- getNewResReg (getTyp op1)
  emit $ Frem r1 op1 op2
  return r1

extractelement op1 op2 = do
  let (Tvector _ typ) = getTyp op1
  r1 <- getNewResReg typ
  emit $ Extractelement r1 op1 op2
  return r1           
insertelement  op1 op2 op3 = do
  r1 <- getNewResReg (getTyp op1)
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
