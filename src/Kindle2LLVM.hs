

module Kindle2LLVM where

import Kindle hiding (unit)
import Common hiding (name, Array)
import Name hiding (name)
import qualified Core
import qualified Core2Kindle
import qualified Prepare4C as P
import Control.Monad.State
import LLVMPP
import Prelude hiding (and,or,div,rem)
import LLVMPrim
import Env
import qualified Data.Map as Map

import Data.Int
import Data.Char
import LLVM
import LLVMKindle
import Depend (groupMap)

-- dsi contains structdefs Map Name Kindle.Decl
kindle2llvm e2 e3 m@(Module name _ _ _ _) = do
  let mod = runCodeGen (show name) (codeGenModule e2 e3 m) 
      source = ppLLVMModule mod
--  tr $ show m
--  tr $ source
  return source

codeGenModule (Core.Module _ _ _ es' _ _ [bs']) dsi (Module moduleName importNames es decls binds) = do
  -- add structs from imported files
  let te2 = Core.tsigsOf bs' ++ extsMap es'
  tei <- Core2Kindle.c2kTEnv dsi te2 
  let env1 = P.addTEnv (primTEnv++tei++es) (P.addDecls (primDecls++dsi) P.env0)
      env  = P.addTEnv (mapSnd typeOf binds) (P.addDecls decls env1)
      ktypedEs = P.pDecls env dsi
      -- add functions declarations from imported files
      ktypedEf = mapSnd P.pType tei
  -- gen primitive functions
  genPrimitives
  -- struct declarations
  k2llvmStructDecls (decls ++ ktypedEs)
  mapM_ (\(sname,_) -> 
             addExternalGC (k2llvmName sname) (Tarray 0 int)) ktypedEs 
  k2llvmAddExternals (ktypedEf ++ es)
  k2llvmAddGlobalVars binds
  k2llvmHarvestFunTypes binds
  k2llvmTopBinds binds
  -- create _init_ module function
  initModule moduleName importNames binds  
  getModule

-- | Add external functions and global variable bindings
k2llvmAddExternals binds = mapM_ f binds
    where 
      f (fname, FunT _ intyps outtyp) = do
        let outtyp' = k2llvmType outtyp
            intyps' = map k2llvmType intyps
            fname'  = k2llvmName fname
        addFunType fname' (Tptr (Tfun outtyp' intyps'))
        addExternalFun fname' outtyp' intyps'
      f (vname, ValT vtyp) = do
        let vname' = k2llvmName vname 
            vtyp'  = k2llvmType vtyp
            reg    = LLVMRegister (ptr vtyp') vname' 
                     (TagGlobal [External,Global] Nothing)
        addGlobalVar vname' reg

-- Add struct binds
k2llvmStructDecls sdecls = mapM_ f sdecls
    where 
      f (sname, Struct _ vars _) = do 
        let sname' = k2llvmName sname
            vars' = fixvars sname' vars
        addStruct sname' vars'
      fixvars _ [] = []
      fixvars sname ((name, ValT typ) : rest) = 
          (k2llvmName name, k2llvmType typ) : fixvars sname rest
      fixvars sname ((name, FunT _ argtyps restyp) : rest) = 
          (k2llvmName name, Tptr (Tfun (k2llvmType restyp) (Tptr (Tstruct sname) : map k2llvmType argtyps))) : fixvars sname rest

-- | Harvest all functions types from the current file
k2llvmHarvestFunTypes binds = mapM_ f binds
    where f (fname, Fun _ atype atenv _) = addFunType (k2llvmName fname) (ptr (Tfun (k2llvmType atype) (map k2llvmType (snd (unzip atenv)))))
          f _ = return ()

-- Bind struct fields
k2llvmStructBinds var ntype bind = mapM_ f bind
    where
      f (Prim GCINFO _, Val atype (ECall _ _ exp)) = do
        r0 <- k2llvmExp var
        bindGCINFO r0 ntype exp
        return ()
      f (vname, Val atype exp) = do
        r0 <- k2llvmExp var
        let typ = getTyp r0
            typ_noptr = dropPtrs typ
            name = k2llvmName vname
        r2 <- k2llvmExp exp
        (offset,typ) <- getStructIndex typ_noptr name
        getelementptr typ [intConst offset] r0 >>= store r2
      f (vname, Fun _ t te (CRet (ECall fname [] es))) = do
        r0 <- k2llvmExp var
        let typ = getTyp r0
            typ_noptr = dropPtrs typ
            name = k2llvmName vname
        (offset,typ) <- getStructIndex typ_noptr name
        ftyp <- getFunType (k2llvmName fname)
        let freg = LLVMRegister ftyp (k2llvmName fname) (TagGlobal [] Nothing)
        r1 <- getelementptr typ [intConst offset] r0 
        if k2llvmName ntype == "CLOS" 
            then do
              freg' <- bitcast (ptr (Tfun void [])) freg 
              store freg' r1
            else store freg r1
      f (x,_) = internalError0 ("k2llvmStructBinds " ++ show x)

gcArray v (EVar x) 
    | x == v = do
         size <- getStructSize (Tstruct (k2llvmName v))
         return $ intConst (LLVMKindle.words size)
    | otherwise = do
         offset <- getStructOffset (Tstruct (k2llvmName v)) (k2llvmName x)
         return $ intConst (LLVMKindle.words offset)
gcArray v (ELit lit) = return $ lit2const lit

-- Add global varaibles from the current module
k2llvmAddGlobalVars binds = mapM_ f binds
    where
      f (vname, Val atype exp) = do
        let name = k2llvmName vname
            typ = k2llvmType atype
        if isPtr typ 
          then
              addGlobalVar name (LLVMRegister (ptr typ) name 
                                 (TagGlobal [Common,Global] 
                                  (Just Null)))
          else
              addGlobalVar name (LLVMRegister (ptr typ) name 
                                 (TagGlobal [Common,Global] 
                                  (Just Zeroinitializer)))
      f _ = return ()

-- Process toplevel bindings
k2llvmTopBinds binds = mapM_ f binds
    where f b@(fname, Fun [] funtyp typenv cmds) = do
            globals <- cgmGets cgmGlobals 
            cgfPut (cgf0 (k2llvmName fname) globals)
            params <- atenv2params typenv
            mapM_ addParams params
            k2llvmCmd cmds
            unreachable
            addCurrFunction (k2llvmType funtyp) params
          f b@(vname, Val _ (ECall (Prim GCINFO _) [] vs@(EVar v : _))) = do
            vals <- mapM (gcArray v) vs
            addLocalGC (Tarray (length vals) int) (k2llvmName vname) vals
          f _ = return ()

k2llvmValBinds (_,binds) = mapM_ f binds >> mapM_ g binds
    where f (vname, Val vtyp (ENew ntyp [] binds)) = do
            let typ = k2llvmType vtyp
                name = k2llvmName vname
            size <- getStructSize (dropPtrs typ)            
            var <- lookupVar name           
            r1 <- codeGenNew var typ size
            addVar name r1
          f (vname, Val vtyp (ECast _ (ENew ntyp [] binds))) = do
            let objtyp = k2llvmType vtyp
                castyp = Tstruct (k2llvmName ntyp)
                sname  = k2llvmName ntyp
                vname' = k2llvmName vname
            var <- lookupVar vname'
            case var of
              Just reg -> do
                       size <- getStructSize castyp
                       r1 <- codeGenNew (Just reg) (ptr castyp) size
                       return ()
              Nothing -> do
                       size <- getStructSize castyp
                       r1 <- codeGenNew Nothing (ptr castyp) size
                       r2 <- bitcast (ptr objtyp) r1
                       addVar vname' r2
          f (vname, Val vtyp exp0) = do
            let typ = k2llvmType vtyp
                name = k2llvmName vname
            r1 <- k2llvmExp exp0
            var <- lookupVar name
            case var of
              Just reg -> store r1 reg
              Nothing -> do
                       r2 <- alloca typ
                       store r1 r2
                       addVar name r2
          f _ = return ()
          g (vname, Val vtyp (ENew ntyp [] binds)) = 
              k2llvmStructBinds (EVar vname) ntyp binds
          g (vname, Val vtyp (ECast _ (ENew ntyp [] binds))) = 
              k2llvmStructBinds (ECast (tCon ntyp) (EVar vname)) ntyp binds
          g _ = return ()

addParams (var,typ) = do
  reg <- getNewNamedReg var typ
  case typ of
    (Tptr (Tstruct _)) -> do
               r1 <- alloca typ
               store reg r1
               addVar var r1
    _         -> do
               r1 <- alloca typ
               store reg r1
               addVar var r1

bindGCINFO r0 typ exp = do
  let typ = getTyp r0
      typ_noptr@(Tstruct sname) = dropPtrs typ  
  local <- isLocalStruct typ_noptr
  if null exp then do
                  LLVMRegister typ name tag <- getVar ("__GC__" ++ sname)
                  let r1 = LLVMRegister (ptr (dropPtrs typ)) name tag
                  r2 <- getarrayelemptr [intConst 0] r1
                  r3 <- getstructelemptr "GCINFO" r0
                  store r2 r3                   
              else do
                  LLVMRegister typ name tag <- getVar ("__GC__" ++ sname)
                  let r1 = LLVMRegister (ptr (dropPtrs typ)) name tag
                  r2 <- k2llvmExp (head exp)
                  r3 <- getelementptr int [intConst 0] r1
                  r4 <- ptrtoint int r3
                  r5 <- add r4 r2
                  r6 <- inttoptr poly r5
                  r7 <- getstructelemptr "GCINFO" r0
                  store r6 r7                   

atenv2params :: ATEnv -> CodeGen [(String,LLVMType)]
atenv2params ps = return [(k2llvmName v, k2llvmType typ) | (v,typ) <- ps]
    
lit2const (LInt _ n) = intConst n
lit2const (LChr _ c) = charConst c
lit2const (LRat _ r) = floatConst r
--lit2const (LStr _ s) = genStr s

{-
genStringSwitch _ _ end [] = br end
genStringSwitch m typ end (ALit (LStr _ lit) cmd : alts) = do
  l1 <- getNextLabel
  l2 <- getNextLabel
  str <- genStr lit
  call "strEq" [m, str] >>= trunc bool >>= condbr l1 l2
  label l1
  k2llvmCmd cmd
  br end
  label l2
  genStringSwitch m typ end alts
genStringSwitch m typ end (AWild cmd : alts) = do
  k2llvmCmd cmd
  genStringSwitch m typ end alts

genIntSwitch _ _ end [] = br end
genIntSwitch m typ end (ALit lit cmd : alts) = do
  l1 <- getNextLabel
  l2 <- getNextLabel
  icmp IcmpEQ m (lit2const lit) >>= condbr l1 l2
  label l1
  k2llvmCmd cmd
  br end
  label l2
  genIntSwitch m typ end alts
genIntSwitch m typ end (AWild cmd : alts) = do
  k2llvmCmd cmd
  genIntSwitch m typ end alts

genFloatSwitch _ _ end [] = br end
genFloatSwitch m typ end (ALit lit cmd : alts) = do
  l1 <- getNextLabel
  l2 <- getNextLabel
  fcmp FcmpUEQ m (lit2const lit) >>= condbr l1 l2
  label l1
  k2llvmCmd cmd
  br end
  label l2
  genFloatSwitch m typ end alts
genFloatSwitch m typ end (AWild cmd : alts) = do
  k2llvmCmd cmd
  genFloatSwitch m typ end alts
-}

k2llvmCmd :: Cmd -> CodeGen () 
k2llvmCmd (CRet exp1) = k2llvmExp exp1 >>= ret
k2llvmCmd (CRun exp1 cmd1) = do
  k2llvmExp exp1
  k2llvmCmd cmd1
k2llvmCmd (CBind False [(x,Val t (ENew n [] bs))] (CBind False [(y,Val tref (ENew (Prim Ref _) [] bs'))] c)) = do
  let objtyp = (struct (k2llvmName n))
  refsize <- getStructSize refstruct
  objsize <- getStructSize objtyp
  r1 <- codeGenNew Nothing refstruct (refsize+objsize)
  addVar (k2llvmName y) r1
  r2 <- load r1
  callvoid "INITREF" [r2]
  k2llvmStructBinds (ECast t (ESel (EVar y) (prim STATE))) n bs
  k2llvmCmd c
k2llvmCmd (CBind False [(_,Val t e)] (CRet (ECast t' _)))
    | t == tUNIT && t' == tUNIT = k2llvmExp e >> return ()
k2llvmCmd (CBind False [(_,Val t e)] CBreak)
  | t == tUNIT = getBreakLabel >>= br
k2llvmCmd (CBind False binds cmd1) = do
  k2llvmValBinds (False,binds)
  k2llvmCmd cmd1
k2llvmCmd (CBind True binds cmd1) = do
  mapM_ k2llvmValBinds (groupMap binds)
  k2llvmCmd cmd1
k2llvmCmd (CUpd name exp1 cmd1) = do
  var <- getVar (k2llvmName name)
  exp <- k2llvmExp exp1
  store exp var
  k2llvmCmd cmd1
k2llvmCmd (CUpdS sname sfield newval cmd1) = do
  r1 <- k2llvmExp sname
  let typ@(Tptr typ_noptr) = getTyp r1                              
  r3 <- k2llvmExp newval
  (offset,typ) <- getStructIndex typ_noptr (k2llvmName sfield)
  getelementptr typ [intConst offset] r1 >>= store r3
  k2llvmCmd cmd1
k2llvmCmd (CUpdA array index newval cmd1) = do
  r1 <- k2llvmExp array
  r2 <- k2llvmExp index
  r3 <- k2llvmExp newval
  let etyp = getTyp r3
  (offset,typ) <- getStructIndex (struct "Array") "elems"
  r4 <- getelementptr typ [intConst offset] r1
  getelementptr etyp [r2] r4 >>= store r3
  k2llvmCmd cmd1
k2llvmCmd (CSwitch e alts) = 
    let firstLit (ALit l _ : _) = l
        firstLit (_ : as)       = firstLit as
    in case litType (firstLit alts) of
         TCon (Prim LIST _) [TCon (Prim Char _) []] -> do
              e' <- k2llvmExp e
              k2llvmStringSwitch e' alts
         TCon (Prim Float _) [] -> do
              e' <- k2llvmExp e
              k2llvmFloatSwitch e' alts
         _ -> do
              e' <- k2llvmExp e
              k2llvmIntSwitch e' alts

k2llvmCmd (CSeq cmd1 cmd2) = k2llvmCmd cmd1 >> k2llvmCmd cmd2
k2llvmCmd CBreak = getBreakLabel >>= br
k2llvmCmd (CRaise exp1) = do
  exp <- k2llvmExp exp1
  callvoid "RAISE" [exp]                             
k2llvmCmd (CWhile exp1 cmd1 cmd2) = do
  loopHead <- getNextLabel
  loopBody <- getNextLabel
  loopExit <- getNextLabel
  addContinueLabel loopHead
  addBreakLabel loopExit
  br loopHead
  label loopHead
  exp <- k2llvmExp exp1
  icmp IcmpEQ exp true >>= condbr loopBody loopExit
  label loopBody
  k2llvmCmd cmd1
  br loopHead
  label loopExit
  dropBreakLabel
  dropContinueLabel
  k2llvmCmd cmd2                             
k2llvmCmd CCont = getContinueLabel >>= br

genStr s = do 
  addString s
  r <- getString s
  call "getStr" [r]

k2llvmFloatSwitch e (ALit lit cmd : alts) = do
  trueBlock  <- getNextLabel
  falseBlock <- getNextLabel
  addBreakLabel falseBlock
  fcmp FcmpUEQ e (lit2const lit) >>= condbr trueBlock falseBlock
  label trueBlock
  k2llvmNestIfCmd cmd
  br falseBlock
  label falseBlock
  dropBreakLabel
  k2llvmFloatSwitch e alts
k2llvmFloatSwitch e [AWild cmd] = do k2llvmNestIfCmd cmd

k2llvmStringSwitch e (ALit (LStr _ lit) cmd : alts) = do
  trueBlock  <- getNextLabel
  falseBlock <- getNextLabel
  addBreakLabel falseBlock
  str <- genStr lit
  call "strEq" [e, str] >>= trunc bool >>= condbr trueBlock falseBlock
  label trueBlock
  k2llvmNestIfCmd cmd
  br falseBlock
  label falseBlock
  dropBreakLabel
  k2llvmStringSwitch e alts
k2llvmStringSwitch _ [AWild cmd] = do k2llvmNestIfCmd cmd

k2llvmIntSwitch e (ALit lit cmd : alts) = do
  trueBlock  <- getNextLabel
  falseBlock <- getNextLabel
  addBreakLabel falseBlock
  icmp IcmpEQ e (lit2const lit) >>= condbr trueBlock falseBlock
  label trueBlock
  k2llvmNestIfCmd cmd
  br falseBlock
  label falseBlock
  dropBreakLabel
  k2llvmIntSwitch e alts
k2llvmIntSwitch _ [AWild c] = k2llvmNestIfCmd c
k2llvmIntSwitch _ []        = return ()

k2llvmNestIfCmd CBreak             = return ()
k2llvmNestIfCmd CCont              = getContinueLabel >>= br  
k2llvmNestIfCmd (CRet e)           = do
  e' <- k2llvmExp e
  ret e'
k2llvmNestIfCmd c                  = do k2llvmCmd c

k2llvmExp :: Exp -> CodeGen LLVMValue
k2llvmExp (EVar (Prim Inherit _)) = return $ LLVMConstant timestruct NullConst
k2llvmExp (EVar name) = do
                             getVar (k2llvmName name) >>= load
k2llvmExp EThis = internalError0 "k2llvmExp EThis"      
k2llvmExp (ELit (LStr _ s)) = genStr s
k2llvmExp (ELit lit) = return $ lit2const lit 
-- select in a n-tuple with n > 4
k2llvmExp (ESel (ECast (TCon n _) exp) name) 
    | isBigTuple n = do
        r1 <- k2llvmExp exp
        -- only possible to handle select from 'a' to 'z'
        -- i.e. only up to 26-tuples
        let offset = ord (head (show name)) - 96
        -- the "2" is used to direct into the elem
        -- part of the struct
        r2 <- getelementptr poly [intConst 2,intConst offset] r1
        load r2
        -- select on casted struct
k2llvmExp (ESel (ECast atype exp) name) = do
  -- perform cast from struct to struct
  let totype@(Tptr totype_noptr) = k2llvmType atype
  -- cast 
  r2 <- (k2llvmExp exp >>= bitcast totype)
  -- get type and offset
  (offset,typ) <- getStructIndex totype_noptr (k2llvmName name)
  getelementptr typ [intConst offset] r2 >>= load
  -- select on ordinary structs
k2llvmExp (ESel exp1 name) = do
  r1 <- k2llvmExp exp1
  let typ@(Tptr typ_noptr) = getTyp r1
  (offset,typ) <- getStructIndex typ_noptr (k2llvmName name)
  getelementptr typ [intConst offset] r1 >>= load
k2llvmExp (ECall name atypes exps) = codeGenECall name exps
k2llvmExp (EEnter (ECast clos@(TCon (Prim CLOS _) (rettyp:typs)) fun) fname atypes exps) = do
  let rettyp' = k2llvmType rettyp
      (Tptr typ_noptr) = k2llvmType clos
      funtyp = Tptr (Tfun rettyp' (map k2llvmType (clos:typs)))
  funAddr <- k2llvmExp fun
  (offset,typ) <- getStructIndex typ_noptr (k2llvmName fname)
  r1 <- getelementptr typ [intConst offset] funAddr 
  r2 <- load r1
  r3 <- bitcast funtyp r2
  exps' <- mapM k2llvmExp exps
  callhigher r3 funtyp (funAddr:exps')
k2llvmExp (EEnter exp fname atypes exps) = do
  r1 <- k2llvmExp exp                              
  let typ@(Tptr typ_noptr) = getTyp r1                              
  (offset,typ) <- getStructIndex typ_noptr (k2llvmName fname)
  r3 <- (getelementptr typ [intConst offset] r1 >>= load)
  exps' <- mapM k2llvmExp (exp:exps)                              
  callhigher r3 typ exps'
  -- many forms of casts, must keep track of types
  -- for example zext can only be applied to 
  -- shorter to longer integer, must have some
  -- logic that chooses the right cast
  -- boolean "optimizations" casts
k2llvmExp (ECast (TCon (Prim Bool _) _) (ECast _ (ELit (LInt _ 1)))) = return true
k2llvmExp (ECast (TCon (Prim Bool _) _) (ECast _ (ELit (LInt _ 0)))) = return false
k2llvmExp (ECast (TCon (Prim Bool _) _) (ELit (LInt _ 1))) = return true 
k2llvmExp (ECast (TCon (Prim Bool _) _) (ELit (LInt _ 0))) = return false
k2llvmExp (ECast (TCon (Prim BITS32 _) _) (ELit (LInt _ n))) = return (bit32Const n)
k2llvmExp (ECast (TCon (Prim BITS16 _) _) (ELit (LInt _ n))) = return (bit16Const n)
k2llvmExp (ECast (TCon (Prim BITS8  _) _) (ELit (LInt _ n))) = return (bit8Const  n)                                
k2llvmExp (ECast ktotype exp1) = do
  r1 <- k2llvmExp exp1
  let totype   = k2llvmType ktotype        
      fromtype = getTyp r1
  case fromtype of
    Tptr _ -> case totype of
                Tint _ -> ptrtoint totype r1
                Tptr _  -> bitcast totype r1
                Tvector _ _ -> ptrtoint int r1 >>= bitcast totype
    Tint n -> case totype of
                Tptr _   -> inttoptr totype r1
                Tint n' -> cast n n' totype r1
                    where 
                      cast n n' totyp reg
                        | n < n'  = zext totyp reg
                        | n == n' = return reg
                        | n > n'  = trunc totyp reg
                Tvector _ _ -> bitcast totype r1
    Tvector _ _ -> case totype of
                     Tint _ -> bitcast totype r1
                     Tptr _ -> bitcast int r1 >>= inttoptr totype 

primBin :: (LLVMValue -> LLVMValue -> CodeGen LLVMValue) -> [Exp] -> CodeGen LLVMValue
primBin f exps = do
  [op1,op2] <- mapM k2llvmExp exps
  f op1 op2

primIcmp :: IcmpArg -> [Exp] -> CodeGen LLVMValue
primIcmp cmp exps = do
  [op1,op2] <- mapM k2llvmExp exps
  case (getTyp op1) of
    Tint _ -> icmp cmp op1 op2
    Tptr _ -> do
      r1 <- ptrtoint word op1
      r2 <- ptrtoint word op1
      icmp cmp r1 r2

primFcmp :: FcmpArg -> [Exp] -> CodeGen LLVMValue
primFcmp cmp exps = do
  [op1,op2] <- mapM k2llvmExp exps
  fcmp cmp op1 op2

bitSet exps = do
  [op1,op2] <- mapM k2llvmExp exps
  let (Tint n) = getTyp op1
  r1 <- bitcast (Tvector n bool) op1
  r2 <- insertelement r1 true op2
  bitcast (Tint n) r2

bitClr exps = do
  [op1,op2] <- mapM k2llvmExp exps
  let (Tint n) = getTyp op1
  r1 <- bitcast (Tvector n bool) op1
  r2 <- insertelement r1 false op2
  bitcast (Tint n) r2

bitTst exps = do
  [op1,op2] <- mapM k2llvmExp exps
  let (Tint n) = getTyp op1
  r1 <- bitcast (Tvector n bool) op1
  r2 <- extractelement r1 op2
  and true r2

bitNot exps = do
  [op1] <- mapM k2llvmExp exps
  let (Tint n) = getTyp op1
  bitcast (Tvector n bool) op1 >>= xor (vectorConst (replicate n true)) >>= bitcast (Tint n)

codeGenECall :: Name -> [Exp] -> CodeGen LLVMValue
codeGenECall (Prim name _) exps 
    | name `elem` [AND8,AND16,AND32]             = primBin and exps
    | name `elem` [OR8,OR16,OR32]                = primBin or exps
    | name `elem` [EXOR8,EXOR16,EXOR32]          = primBin xor exps
    | name `elem` [SHIFTL8,SHIFTL16,SHIFTL32]    = do
          [op1,op2] <- mapM k2llvmExp exps
          let bittyp = getTyp op1
          r1 <- zext int op1
          r2 <- shl r1 op2
          trunc bittyp r2
    | name `elem` [SHIFTR8,SHIFTR16,SHIFTR32]    = do
          [op1,op2] <- mapM k2llvmExp exps
          let bittyp = getTyp op1
          r1 <- zext int op1
          r2 <- lshr r1 op2
          trunc bittyp r2
    | name `elem` [SHIFTRA8,SHIFTRA16,SHIFTRA32] = do
          [op1,op2] <- mapM k2llvmExp exps
          let bittyp = getTyp op1
          r1 <- zext int op1
          r2 <- ashr r1 op2
          trunc bittyp r2
    | name `elem` [SET8,SET16,SET32]             = bitSet exps
    | name `elem` [CLR8,CLR16,CLR32]             = bitClr exps
    | name `elem` [TST8,TST16,TST32]             = bitTst exps
    | name `elem` [NOT8,NOT16,NOT32]             = bitNot exps
    | otherwise = case name of
                    IntPlus ->  primBin add exps
                    IntMinus -> primBin sub exps
                    IntTimes -> primBin mul exps
                    IntDiv   -> primBin div exps
                    IntMod   -> primBin rem exps
                    IntNeg   -> do
                              [exp] <- mapM k2llvmExp exps
                              sub (intConst 0) exp
                    IntEQ -> primIcmp IcmpEQ exps
                    IntNE -> primIcmp IcmpNE exps                             
                    IntGT -> primIcmp IcmpSGT exps                             
                    IntGE -> primIcmp IcmpSGE exps                             
                    IntLT -> primIcmp IcmpSLT exps                    
                    IntLE -> primIcmp IcmpSLE exps                                      
                    FloatPlus -> primBin fadd exps
                    FloatMinus -> primBin fsub exps
                    FloatTimes -> primBin fmul exps
                    FloatDiv   -> primBin fdiv exps                            
                    FloatNeg  -> do
                              [exp] <- mapM k2llvmExp exps
                              fsub (floatConst 0) exp
                    FloatEQ -> primFcmp FcmpUEQ exps
                    FloatNE -> primFcmp FcmpUNE exps                              
                    FloatGT -> primFcmp FcmpUGT exps                              
                    FloatGE -> primFcmp FcmpUGE exps                              
                    FloatLT -> primFcmp FcmpULT exps                              
                    FloatLE -> primFcmp FcmpULE exps                              
                    LOCK   -> mapM k2llvmExp exps >>= call "LOCK"
                    UNLOCK -> mapM k2llvmExp exps >>= call "UNLOCK"
                    ASYNC ->  mapM k2llvmExp exps >>= call "ASYNC"
                    IntToFloat -> do
                              [exp] <- mapM k2llvmExp exps
                              sitofp float exp
                    FloatToInt -> do
                              [exp] <- mapM k2llvmExp exps
                              fptosi int exp
                    Float2POLY -> do 
                              [exp] <- mapM k2llvmExp exps
                              r1 <- bitcast int exp
                              inttoptr poly r1
                    POLY2Float -> do
                              [exp] <- mapM k2llvmExp exps
                              r1 <- ptrtoint int exp
                              bitcast float r1
                    LazyAnd -> do                              
                              [op1,op2] <- mapM k2llvmExp exps                              
                              r1 <- alloca bool
                              l1 <- getNextLabel
                              l2 <- getNextLabel
                              l3 <- getNextLabel
                              icmp IcmpEQ op1 true >>=
                                   condbr l1 l2                             
                              label l1
                              r2 <- icmp IcmpEQ op2 true 
                              store r2 r1
                              br l3
                              label l2
                              store false r1
                              br l3
                              label l3
                              load r1
                    LazyOr -> do
                              [op1,op2] <- mapM k2llvmExp exps
                              r1 <- alloca bool
                              l1 <- getNextLabel
                              l2 <- getNextLabel
                              l3 <- getNextLabel
                              icmp IcmpEQ op1 true >>=
                                   condbr l1 l2
                              label l1
                              store true r1
                              br l3
                              label l2
                              r2 <- icmp IcmpEQ op2 true 
                              store r2 r1
                              br l3
                              label l3
                              load r1
                    TimeMinus  -> mapM k2llvmExp exps >>= call "primTimeMinus"
                    TimePlus   -> mapM k2llvmExp exps >>= call "primTimePlus"
                    TimeEQ     -> mapM k2llvmExp exps >>= call "primTimeEQ"
                    TimeNE     -> mapM k2llvmExp exps >>= call "primTimeNE"
                    TimeGE     -> mapM k2llvmExp exps >>= call "primTimeGE"
                    TimeGT     -> mapM k2llvmExp exps >>= call "primTimeGT"
                    TimeLT     -> mapM k2llvmExp exps >>= call "primTimeLT"
                    TimeLE     -> mapM k2llvmExp exps >>= call "primTimeLE"
                    TimeMin    -> mapM k2llvmExp exps >>= call "primTimeMin"
                    Refl       -> mapM k2llvmExp exps >>= call "primRefl"
                    Sec        -> mapM k2llvmExp exps >>= call "sec"
                    PidEQ      -> primIcmp IcmpEQ exps
                    PidNE      -> primIcmp IcmpNE exps
                    ListArray  -> mapM k2llvmExp exps >>= call "primListArray"
                    IndexArray -> do
                      [_,arr,i] <- mapM k2llvmExp exps
                      getelementptr poly [intConst 2,i] arr >>= load
                    ShowFloat  -> mapM k2llvmExp exps >>= call "primShowFloat"
                    SizeArray  -> do
                      [_,arr] <- mapM k2llvmExp exps 
                      getstructelemptr "size" arr >>= load
                    Raise      -> mapM k2llvmExp exps >>= call "Raise"
                    EmptyArray -> mapM k2llvmExp exps >>= call "EmptyArray"
                    Millisec   -> mapM k2llvmExp exps >>= call "millisec"
                    UniArray   -> mapM k2llvmExp exps >>= call "primUniArray"
                    CloneArray -> mapM k2llvmExp exps >>= call "CloneArray"
                    MicrosecOf -> mapM k2llvmExp exps >>= call "microsecOf"
                    SecOf      -> mapM k2llvmExp exps >>= call "secOf"
                    Abort      -> mapM k2llvmExp exps >>= call "ABORT"
                    TIMERTERM  -> mapM k2llvmExp exps >>= call "primTIMERTERM"
                    _ -> fail $ show name ++ " " ++ show exps

codeGenECall name exps = do
  exps' <- mapM k2llvmExp exps
  let fname = k2llvmName name
  call fname exps'

codeGenClosCall :: LLVMValue -> [AType] -> [Exp] -> CodeGen LLVMValue
codeGenClosCall funAddr typs exps = mapM k2llvmExp exps >>= callhigher funAddr poly

codeGenCall :: String -> [Exp] -> CodeGen LLVMValue
codeGenCall fname exps = mapM k2llvmExp exps >>= call fname

codeGenNew :: Maybe LLVMValue -> LLVMType -> Int -> CodeGen LLVMValue
codeGenNew (Just r1) typ size = do
  r2 <- bitcast (ptr $ ptr int) r1
  let size' = intConst (LLVMKindle.words size)
  callvoid "new" [r2,size']
  return r1
codeGenNew Nothing typ size = do
  r1 <- alloca typ
  r2 <- bitcast (ptr (ptr int)) r1
  let size' = intConst (LLVMKindle.words size)
  callvoid "new" [r2,size']
  return r1

isBigTuple n = isTuple n && width n > 4

initModule n ns bs = do
  let fname = "_init_" ++ modToundSc (str n)
      var    = LLVMRegister bool "INITIALIZED" (TagGlobal [Internal,Global] Nothing)
      varptr = LLVMRegister (ptr bool) "INITIALIZED" (TagGlobal [Internal,Global] Nothing)
  globals <- cgmGets cgmGlobals 
  cgfPut (cgf0 fname globals)
  addTopLevelConstant "INITIALIZED" var [Internal,Global] false
  l1 <- getNextLabel
  l2 <- getNextLabel
  load varptr >>= icmp IcmpEQ false >>= condbr l1 l2
  label l1
  initImports ns
  initStructs bs
  store true varptr
  br l2
  label l2
  retvoid
  addCurrFunction void []

initImports ns = mapM f ns
    where 
      f n = addExternalFun (fname n) void [] >> callvoid (fname n) []
      fname n = "_init_" ++ modToundSc (str n)

initStructs binds = mapM_ k2llvmValBinds' (groupMap binds) 
    where     
      k2llvmValBinds' (r,binds) = k2llvmValBinds (r, filter isInitVal binds)
      isInitVal (_,Val _ (ECall (Prim GCINFO _) _ _)) = False
      isInitVal (_,Val _ _) = True
      isInitVal _ = False

