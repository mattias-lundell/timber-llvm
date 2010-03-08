module Kindle2LLVM where

import Kindle hiding (unit)
import Common hiding (name, Array)
import Name hiding (name)
import qualified Core
import qualified Core2Kindle
import Prepare4C hiding (isPtr)
import Control.Monad.State
import LLVMPP
import Prelude hiding (and,or,div,rem)

import Data.Int
import Data.Char
import LLVM
import LLVMKindle
import Depend (groupMap)

-- TODO
-- 
-- structs: fix name, ex fieldname
-- clean state, to many fields

-- dsi contains structdefs Map Name Kindle.Decl
kindle2llvm e2 e3 m = do
  let (mod,_) = runState (codeGenModule e2 e3 m) s0
      source = ppLLVMModule mod
--  tr $ show m
--  tr $ source
  return source

codeGenModule (Core.Module _ _ _ es' _ _ [bs']) dsi (Module moduleName importNames es decls binds) = do
  -- add structs from imported files
  let te2 = Core.tsigsOf bs' ++ extsMap es'
  tei <- Core2Kindle.c2kTEnv dsi te2 
  let env1 = addTEnv (primTEnv++tei++es) (addDecls (primDecls++dsi) env0)
      env  = addTEnv (mapSnd typeOf binds) (addDecls decls env1)
      ktypedEs = pDecls env dsi
      -- add functions declarations from imported files
      ktypedEf = mapSnd pType tei
  -- gen code
  setModuleName (show moduleName)
  codeGenPrelude
  -- struct declarations
  k2llvmStructDecls (decls ++ ktypedEs) --mapM_ codeGenDecl (decls ++ ktypedEs)
  mapM_ (\(sname,_) -> addExternalGC (k2llvmName sname) (Tarray 0 int) {-(ptr word)-}) ktypedEs 
  k2llvmAddExternals (ktypedEf ++ es)
  k2llvmAddGlobalVars binds
  k2llvmHarvestFunTypes binds
  k2llvmTopBinds binds
  initModule moduleName importNames binds
  getModule
  --return ((show moduleName),code) 

-- Add external functions and global variable bindings
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
            reg    = LLVMRegister (ptr vtyp') ('@' : vname')
        addGlobalVar vname' reg [External,Global] Nothing

-- Add struct binds
k2llvmStructDecls sdecls = mapM_ f sdecls
    where 
      f (sname, (Struct _ vars _)) = do 
        let sname' = k2llvmName sname
            vars' = fixvars sname' vars
        addStruct sname' vars'
      fixvars _ [] = []
      fixvars sname ((name,ValT typ):rest) = 
          (k2llvmName name,k2llvmType typ) : fixvars sname rest
      fixvars sname ((name,FunT _ argtyps restyp):rest) = 
          (k2llvmName name, (Tptr (Tfun (k2llvmType restyp) (Tptr (Tstruct sname):map k2llvmType argtyps)))):fixvars sname rest

-- Harvest all functions from the current file
k2llvmHarvestFunTypes binds = mapM_ f binds
    where f (fname, Fun _ atype atenv _) = addFunType (k2llvmName fname) (ptr (Tfun (k2llvmType atype) (map k2llvmType (snd (unzip atenv)))))
          f _ = return ()

-- Bind struct fields
k2llvmStructBinds var ntype bind = mapM_ f bind
    where
      f (Prim GCINFO _, Val atype (ECall _ _ exp)) = do
        r0 <- codeGenExp var
        bindGCINFO r0 ntype exp
        return ()
      f (vname, Val atype exp) = do
        r0 <- codeGenExp var
        let typ = getTyp r0
            typ_noptr = dropPtrs typ
            name = k2llvmName vname
        --r1 <- getNewResReg typ
        r2 <- codeGenExp exp
        (offset,typ) <- getStructIndex typ_noptr name
        getelementptr typ [intConst offset] r0 >>= store r2
        --getstructelem (k2llvmName vname) r0 >>= store r2
      f (vname, Fun _ t te (CRet (ECall fname [] es))) = do
        r0 <- codeGenExp var
        let typ = getTyp r0
            typ_noptr = dropPtrs typ
            name = k2llvmName vname
        (offset,typ) <- getStructIndex typ_noptr name
        ftyp <- getFunType (k2llvmName fname)
        let freg = LLVMRegister ftyp ('@' : k2llvmName fname)
        r1 <- getelementptr typ [intConst offset] r0 
        if k2llvmName ntype == "CLOS" 
            then 
                do
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
gcArray v (ELit l) = lit2value l

-- Add global varaibles from the current module
k2llvmAddGlobalVars binds = mapM_ f binds
    where
      f (vname, Val atype exp) = do
        let name = k2llvmName vname
            typ = k2llvmType atype
            reg = LLVMRegister (ptr typ) ('@' : name)
        if isPtr typ 
            then
                addGlobalVar name reg [Common,Global] (Just Null)
            else
                addGlobalVar name reg [Common,Global] (Just Zeroinitializer)
            
      f _ = return ()

-- Process toplevel bindings
k2llvmTopBinds binds = mapM_ f binds
  where f b@(fname, Fun [] funtyp typenv cmds) = do
          setFunName (k2llvmName fname)
          enterNewScope
          params <- atenv2params typenv
          mapM_ addParams params
          codeGenCmd cmds
          unreachable
          leaveScope
          addCurrFunction (k2llvmName fname) (k2llvmType funtyp) params
        f b@(vname, Val _ (ECall (Prim GCINFO _) [] vs@(EVar v : _))) = do
          vals <- mapM (gcArray v) vs
          addGCArray (Tarray (length vals) int) (k2llvmName vname) vals
          return ()
        f _                    = return ()

k2llvmValBinds (_,binds) = mapM_ f binds >> mapM_ g binds
    where f (vname, Val vtyp (ENew ntyp [] binds)) = do
            let typ = k2llvmType vtyp
                name = k2llvmName vname
            size <- getStructSize (dropPtrs typ)            
            var <- lookupVar name           
            r1 <- codeGenNew'' var typ size
            addVariableToScope name r1
          f (vname, Val vtyp (ECast _ (ENew ntyp [] binds))) = do
            let objtyp = k2llvmType vtyp
                castyp = Tstruct (k2llvmName ntyp)
                sname  = k2llvmName ntyp
                vname' = k2llvmName vname
            var <- lookupVar vname'
            case var of
              Just reg -> do
                       size <- getStructSize castyp
                       r1 <- codeGenNew'' (Just reg) (ptr castyp) size
                       return ()
              Nothing -> do
                size <- getStructSize castyp
                r1 <- codeGenNew'' Nothing (ptr castyp) size
                r2 <- bitcast (ptr objtyp) r1
                addVariableToScope vname' r2
          f (vname, Val vtyp exp0) = do
            let typ = k2llvmType vtyp
                name = k2llvmName vname
            r1 <- codeGenExp exp0
            var <- lookupVar name
            case var of
              Just reg -> store r1 reg
              Nothing -> do
                       r2 <- alloca typ
                       store r1 r2
                       addVariableToScope name r2
          f _                                                = return ()
          g (vname, Val vtyp (ENew ntyp [] binds))           = k2llvmStructBinds (EVar vname) ntyp binds
          g (vname, Val vtyp (ECast _ (ENew ntyp [] binds))) = k2llvmStructBinds (ECast (tCon ntyp) (EVar vname)) ntyp binds
          g _                                                = return ()

addParams (var,typ) = do
  --let var' = "%" ++ var 
  reg <- getNamedOpReg var typ
  case typ of
    (Tptr (Tstruct _)) -> do
               --let reg' = Register typ reg Operand
               r1 <- alloca typ
               store reg r1
               addParamToScope (var,r1)
    _         -> do
               r1 <- alloca typ
               store reg r1
               addParamToScope (var,r1)

bindGCINFO r0 typ exp = do
  let typ = getTyp r0
      typ_noptr@(Tstruct sname) = dropPtrs typ  
  local <- isLocalStruct typ_noptr
  if isPrimStruct typ_noptr 
    then
      if null exp
        then
          do
            --tr $ "prim without arithmetic " ++ show typ_noptr
            r1 <- getExternalGC sname
            --r4 <- load r1
            r2 <- getarrayelemptr [intConst 0] r1
            r3 <- getstructelemptr "GCINFO" r0
            store r2 r3 
        else
          do
            --tr $ "prim with arithmetic " ++ show typ_noptr
            r1 <- codeGenExp (head exp)
            r2 <- getExternalGC sname
            r3 <- getarrayelemptr [intConst 0] r2
            --r3 <- load r2
            r4 <- ptrtoint int r3
            r5 <- add r4 r1
            r6 <- inttoptr poly r5
            r7 <- getstructelemptr "GCINFO" r0
            store r6 r7 
    else
      if not local
        then
          do
            --tr $ "external " ++ show typ_noptr
            r1 <- getExternalGC sname
            --r4 <- load r1
            r2 <- getarrayelemptr [intConst 0] r1
            r3 <- getstructelemptr "GCINFO" r0
            store r2 r3 
        else
          do
            --tr $ "internal " ++ show typ_noptr
            r1 <- getLocalGC sname
            r2 <- getarrayelemptr [intConst 0] r1
            r3 <- getstructelemptr "GCINFO" r0
            store r2 r3 

atenv2params :: ATEnv -> CGState [(String,LLVMType)]
atenv2params ps = return [((k2llvmName v),k2llvmType typ) | (v,typ) <- ps]
               
lit2value (LInt _ n) = return $ intConst n
lit2value (LChr _ c) = return $ LLVMConstant char (CharConst c)
lit2value (LRat _ r) = return $ floatConst r
lit2value (LStr _ s) = genStr s

genStringSwitch _ _ end [] = br end
genStringSwitch m typ end ((ALit lit cmd):alts) = do
  n <- lit2value lit
  l1 <- getNextLabel
  l2 <- getNextLabel
  call "strEq" [m,n] >>= trunc bool >>= condbr l1 l2
  label l1
  codeGenCmd cmd
  br end
  label l2
  genStringSwitch m typ end alts
genStringSwitch m typ end ((AWild cmd):alts) = do
  codeGenCmd cmd
  genStringSwitch m typ end alts


genIntSwitch _ _ end [] = br end
genIntSwitch m typ end ((ALit lit cmd):alts) = do
  n <- lit2value lit
  l1 <- getNextLabel
  l2 <- getNextLabel
  icmp IcmpEQ m n >>= condbr l1 l2
  label l1
  codeGenCmd cmd
  br end
  label l2
  genIntSwitch m typ end alts
genIntSwitch m typ end ((AWild cmd):alts) = do
  codeGenCmd cmd
  genIntSwitch m typ end alts

genFloatSwitch _ _ end [] = br end
genFloatSwitch m typ end ((ALit lit cmd):alts) = do
  n <- lit2value lit
  l1 <- getNextLabel
  l2 <- getNextLabel
  fcmp FcmpUEQ m n >>= condbr l1 l2
  label l1
  codeGenCmd cmd
  br end
  label l2
  genFloatSwitch m typ end alts
genFloatSwitch m typ end ((AWild cmd):alts) = do
  codeGenCmd cmd
  genFloatSwitch m typ end alts

codeGenCmd :: Cmd -> CGState () 
codeGenCmd cmd0 = case cmd0 of
                    CRet exp1                 -> codeGenExp exp1 >>= ret
                    CRun exp1 cmd1            -> do
                              codeGenExp exp1
                              codeGenCmd cmd1
--                    (CBind False [(_,Val (TCon (Tuple 0 _) _) e)] (CRet (ECast (TCon (Tuple 0 _) _) _))) -> do
--                              codeGenExp e
--                              return ()
                    (CBind False [(x,Val t (ENew n [] bs))] (CBind False [(y,Val tref (ENew (Prim Ref _) [] bs'))] c)) -> do
                              --let reftyp = (Tstruct "REF")
                              let objtyp = (struct (k2llvmName n))
                              refsize <- getStructSize refstruct
                              objsize <- getStructSize objtyp
                              r1 <- codeGenNew'' Nothing refstruct (refsize+objsize)
                              addVariableToScope (k2llvmName y) r1
                              r2 <- load r1
                              callvoid "INITREF" [r2]
                              --tr $ show (ECast t (ESel (EVar y) (prim STATE)))
                              --tr $ show bs
                              --tr $ show bs'
                              k2llvmStructBinds (ECast t (ESel (EVar y) (prim STATE))) n bs
                              --(offset,typ) <- getStructIndex refstruct "STATE"
                              codeGenCmd c
                    CBind False binds cmd1    -> do
                              -- mapM_ codeGenBind binds
                              k2llvmValBinds (False,binds)
                              codeGenCmd cmd1
                    CBind True binds cmd1     -> do --fail $ "undefined CBind"
                              mapM_ k2llvmValBinds (groupMap binds)
                              codeGenCmd cmd1
                    CUpd name exp1 cmd1       -> do
                              var <- getVariable (k2llvmName name)
                              exp <- codeGenExp exp1
                              store exp var
                              codeGenCmd cmd1
                    CUpdS sname sfield newval cmd1 -> do
                              r1 <- codeGenExp sname
                              let typ@(Tptr typ_noptr) = getTyp r1                              
                              r3 <- codeGenExp newval
                              (offset,typ) <- getStructIndex typ_noptr (k2llvmName sfield)
                              getelementptr typ [intConst offset] r1 >>= store r3
                              --getstructelem (k2llvmName sfield) r1 >>= store r3
                              codeGenCmd cmd1
                    CUpdA array index newval cmd1 -> do --fail $ "undefined CUpdA"
                              r1 <- codeGenExp array
                              r2 <- codeGenExp index
                              r3 <- codeGenExp newval
                              let etyp = getTyp r3
                              (offset,typ) <- getStructIndex (struct "Array") "elems"
                              r4 <- getelementptr typ [intConst offset] r1
                              getelementptr etyp [r2] r4 >>= store r3
                              codeGenCmd cmd1
--                              fail $ "\n" ++ show r1 ++ "\n" ++ show r2 ++ "\n" ++ show r3
                              --getelementptr 
                    -- switch can be of different forms, on a "complete"
                    -- form that covers all cases and ends when the right 
                    -- case is executed, it can also be on the form 
                    -- with a default case after the switch statement
                    -- if there are two cases, use the "select" instruction
{-                  CSwitch exp1 alts         -> do 
                              end <- getNextLabel
                              e <- codeGenExp exp1
                              let typ' = getTyp e 
                              genSwitch e typ' end alts
                              br end
                              label end-}

                    CSwitch exp1 alts -> let firstLit (ALit l _ : _) = l
                                             firstLit (_ : as)       = firstLit as
                                         in case litType (firstLit alts) of
                                              TCon (Prim LIST _) [TCon (Prim Char _) []] -> do --return () -- k2llvmStringAlts False e alts
                                                                           end <- getNextLabel
                                                                           exp1' <- codeGenExp exp1
                                                                           let typ = getTyp exp1'
                                                                           genStringSwitch exp1' typ end alts
                                                                           br end
                                                                           label end
                                              TCon (Prim Float _) []                     -> do --return () -- k2llvmFloatAlts False e alts
                                                                           end <- getNextLabel
                                                                           exp1' <- codeGenExp exp1
                                                                           let typ = getTyp exp1'
                                                                           genFloatSwitch exp1' typ end alts
                                                                           br end
                                                                           label end
                                              _                                          -> do --return () -- k2llvmIntAlts 
                                                                           end <- getNextLabel
                                                                           exp1' <- codeGenExp exp1
                                                                           let typ = getTyp exp1'
                                                                           genIntSwitch exp1' typ end alts
                                                                           br end
                                                                           label end
                    CSeq cmd1 cmd2            -> do
                              codeGenCmd cmd1
                              codeGenCmd cmd2
                    CBreak -> return () 
                    CRaise exp1               -> do
                              exp <- codeGenExp exp1
                              callvoid "RAISE" [exp]                             
                    CWhile exp1 cmd1 cmd2 -> do
                              l1 <- getNextLabel
                              l2 <- getNextLabel
                              l3 <- getNextLabel
                              addLoopLabel l1
                              br l1
                              label l1
                              exp <- codeGenExp exp1
                              icmp IcmpEQ exp true >>= condbr l2 l3
                              label l2
                              codeGenCmd cmd1
                              br l1
                              label l3
                              dropLoopLabel
                              codeGenCmd cmd2                             
                    CCont -> do
                              label <- getLoopLabel
                              br label
                              return ()

genStr s = do 
  addString s
  r <- getString s
  call "getStr" [r]

codeGenExp :: Exp -> CGState LLVMValue
codeGenExp exp0 = case exp0 of
                    EVar (Prim Inherit _) -> return $ LLVMConstant timestruct NullConst
                    EVar name -> getVariable (k2llvmName name) >>= load
                    EThis -> fail "undefined EThis"      
                    ELit (LInt _ i) -> return $ intConst i
                    ELit (LRat _ r) -> return $ floatConst r
                    ELit (LChr _ c) -> return $ charConst c
                    ELit (LStr _ s) -> genStr s
                    -- select in a n-tuple with n > 4
                    ESel (ECast (TCon n _) exp) name 
                        | isBigTuple n -> do
                              r1 <- codeGenExp exp
                              -- only possible to handle select from 'a' to 'z'
                              -- i.e. only up to 26-tuples
                              let offset = ord (head (show name)) - 96
                              -- the "2" is used to direct into the elem
                              -- part of the struct
                              r2 <- getelementptr poly [intConst 2,intConst offset] r1
                              load r2
                    -- select on casted struct
                    ESel (ECast atype exp) name  -> do
                              -- perform cast from struct to struct
                              let totype@(Tptr totype_noptr) = k2llvmType atype
                              -- codegen exp
                              -- cast 
                              r2 <- (codeGenExp exp >>= bitcast totype)
                              -- get type and offset
                              (offset,typ) <- getStructIndex totype_noptr (k2llvmName name)
                              getelementptr typ [intConst offset] r2 >>= load
                              --getstructelem (k2llvmName name) r2 >>= load
                              -- select on ordinary structs
                    ESel   exp1 name             -> do
                              r1 <- codeGenExp exp1
                              let typ@(Tptr typ_noptr) = getTyp r1
                              (offset,typ) <- getStructIndex typ_noptr (k2llvmName name)
                              getelementptr typ [intConst offset] r1 >>= load
                              --getstructelem (k2llvmName name) r1 >>= load
                    ENew   name atypes binds     -> fail "undefined ENew"
                    ECall  name atypes exps      -> codeGenECall name exps
                    EEnter (ECast clos@(TCon (Prim CLOS _) (rettyp:typs)) fun) fname atypes exps -> do
                              let rettyp' = k2llvmType rettyp
                                  (Tptr typ_noptr) = k2llvmType clos
                                  funtyp = Tptr (Tfun rettyp' (map k2llvmType (clos:typs)))
                              funAddr <- codeGenExp fun
                              (offset,typ) <- getStructIndex typ_noptr (k2llvmName fname)
                              r1 <- getelementptr typ [intConst offset] funAddr 
                              r2 <- load r1
                              r3 <- bitcast funtyp r2
                              exps' <- mapM codeGenExp exps
                              callhigher r3 funtyp (funAddr:exps')
                    EEnter exp fname atypes exps -> do
                              r1 <- codeGenExp exp                              
                              let typ@(Tptr typ_noptr) = getTyp r1                              
                              --r3 <- (getstructelem (k2llvmName fname) r1 >>= load)
                              (offset,typ) <- getStructIndex typ_noptr (k2llvmName fname)
                              r3 <- (getelementptr typ [intConst offset] r1 >>= load)
                              exps' <- mapM codeGenExp (exp:exps)                              
                              callhigher r3 typ exps' --(exp:exps) 
                              --codeGenClosCall r3 atypes (exp:exps)
                    -- many forms of casts, must keep track of types
                    -- for example zext can only be applied to 
                    -- shorter to longer integer, must have some
                    -- logic that chooses the right cast
                    -- boolean "optimizations" casts
                    ECast (TCon (Prim Bool _) _) (ECast _ (ELit (LInt _ 1))) -> return true
                    ECast (TCon (Prim Bool _) _) (ECast _ (ELit (LInt _ 0))) -> return false
                    ECast (TCon (Prim Bool _) _) (ELit (LInt _ 1)) -> return true 
                    ECast (TCon (Prim Bool _) _) (ELit (LInt _ 0)) -> return false
                    ECast (TCon (Prim BITS32 _) _) (ELit (LInt _ n)) -> return (bit32Const n)
                    ECast (TCon (Prim BITS16 _) _) (ELit (LInt _ n)) -> return (bit16Const n)
                    ECast (TCon (Prim BITS8  _) _) (ELit (LInt _ n)) -> return (bit8Const  n)                                
                    --  (ECast (TCon Int []) (ECall ! [] [ECast (TCon BITS32 []) (ELit LInt 1),EVar e,ELit LInt 0])))]
                    --ECast elemtyp (ECall (Prim IndexArray _) [] exps) -> do --fail $ "no array index"
                             
                    ECast ktotype exp1 -> do
                              r1 <- codeGenExp exp1
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

primBin :: (LLVMValue -> LLVMValue -> CGState LLVMValue) -> [Exp] -> CGState LLVMValue
primBin f exps = do
  [op1,op2] <- mapM codeGenExp exps
  f op1 op2

primIcmp :: IcmpArg -> [Exp] -> CGState LLVMValue
primIcmp cmp exps = do
  [op1,op2] <- mapM codeGenExp exps
  case (getTyp op1) of
    Tint _ -> icmp cmp op1 op2
    Tptr _ -> do
      r1 <- ptrtoint word op1
      r2 <- ptrtoint word op1
      icmp cmp r1 r2

primFcmp :: FcmpArg -> [Exp] -> CGState LLVMValue
primFcmp cmp exps = do
  [op1,op2] <- mapM codeGenExp exps
  fcmp cmp op1 op2

bitSet exps = do
  [op1,op2] <- mapM codeGenExp exps
  let (Tint n) = getTyp op1
  r1 <- bitcast (Tvector n bool) op1
  r2 <- insertelement r1 true op2
  bitcast (Tint n) r2

bitClr exps = do
  [op1,op2] <- mapM codeGenExp exps
  let (Tint n) = getTyp op1
  r1 <- bitcast (Tvector n bool) op1
  r2 <- insertelement r1 false op2
  bitcast (Tint n) r2

bitTst exps = do
  [op1,op2] <- mapM codeGenExp exps
  let (Tint n) = getTyp op1
  r1 <- bitcast (Tvector n bool) op1
  r2 <- extractelement r1 op2
  and true r2

bitNot exps = do
  [op1] <- mapM codeGenExp exps
  let (Tint n) = getTyp op1
  bitcast (Tvector n bool) op1 >>= xor (vectorConst (replicate n true)) >>= bitcast (Tint n)

codeGenECall :: Name -> [Exp] -> CGState LLVMValue
codeGenECall (Prim name _) exps 
    | elem name [AND8,AND16,AND32]             = primBin and exps
    | elem name [OR8,OR16,OR32]                = primBin or exps
    | elem name [EXOR8,EXOR16,EXOR32]          = primBin xor exps
    | elem name [SHIFTL8,SHIFTL16,SHIFTL32]    = do
          [op1,op2] <- mapM codeGenExp exps
          let bittyp = getTyp op1
          r1 <- zext int op1
          r2 <- shl r1 op2
          trunc bittyp r2
    | elem name [SHIFTR8,SHIFTR16,SHIFTR32]    = do
          [op1,op2] <- mapM codeGenExp exps
          let bittyp = getTyp op1
          r1 <- zext int op1
          r2 <- lshr r1 op2
          trunc bittyp r2
    | elem name [SHIFTRA8,SHIFTRA16,SHIFTRA32] = do
          [op1,op2] <- mapM codeGenExp exps
          let bittyp = getTyp op1
          r1 <- zext int op1
          r2 <- ashr r1 op2
          trunc bittyp r2
    | elem name [SET8,SET16,SET32]             = bitSet exps
    | elem name [CLR8,CLR16,CLR32]             = bitClr exps
    | elem name [TST8,TST16,TST32]             = bitTst exps
    | elem name [NOT8,NOT16,NOT32]             = bitNot exps
    | otherwise = case name of
                    IntPlus ->  primBin add exps
                    IntMinus -> primBin sub exps
                    IntTimes -> primBin mul exps
                    IntDiv   -> primBin div exps
                    IntMod   -> primBin rem exps
                    IntNeg   -> do
                              [exp] <- mapM codeGenExp exps
                              --let zero = Value {typ=int, value=(Vint32 0), regType=Operand}
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
                              [exp] <- mapM codeGenExp exps
                              --let zero = Value {typ=float, value=(Vfloat 0), regType=Operand}
                              fsub (floatConst 0) exp
                    FloatEQ -> primFcmp FcmpUEQ exps
                    FloatNE -> primFcmp FcmpUNE exps                              
                    FloatGT -> primFcmp FcmpUGT exps                              
                    FloatGE -> primFcmp FcmpUGE exps                              
                    FloatLT -> primFcmp FcmpULT exps                              
                    FloatLE -> primFcmp FcmpULE exps                              
                    GCINFO -> return $ intConst 0
                    LOCK   -> mapM codeGenExp exps >>= call "LOCK"
                    UNLOCK -> mapM codeGenExp exps >>= call "UNLOCK"
                    ASYNC ->  mapM codeGenExp exps >>= call "ASYNC"
                    IntToFloat -> do
                              [exp] <- mapM codeGenExp exps
                              sitofp float exp
                    FloatToInt -> do
                              [exp] <- mapM codeGenExp exps
                              fptosi int exp
                    Float2POLY -> do
                              [exp] <- mapM codeGenExp exps
                              r1 <- alloca (struct "FloatCast") 
                              r2 <- getelementptr float [intConst 0] r1 
                              store exp r2
                              r3 <- getelementptr float [intConst 0] r1
                              r4 <- bitcast (ptr (ptr int)) r3
                              load r4
                              --return r5
                    POLY2Float -> do
                              [exp] <- mapM codeGenExp exps
                              r1 <- alloca (struct "FloatCast")
                              r2 <- getelementptr float [intConst 0] r1
                              r3 <-bitcast (ptr (ptr int)) r2
                              store exp r3
                              r4 <- getelementptr float [intConst 0] r1
                              load r4
                              --return r5
                              --store exp r3
--                              bitcast (ptr (struct "FloatCast")) exp >>=    
--                                      getelementptr float [intConst 0] >>= load
                    LazyAnd -> do                              
                              [op1,op2] <- mapM codeGenExp exps                              
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
                              [op1,op2] <- mapM codeGenExp exps
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
                    TimeMinus  -> mapM codeGenExp exps >>= call "primTimeMinus"
                    TimePlus   -> mapM codeGenExp exps >>= call "primTimePlus"
                    TimeEQ     -> mapM codeGenExp exps >>= call "primTimeEQ"
                    TimeNE     -> mapM codeGenExp exps >>= call "primTimeNE"
                    TimeGE     -> mapM codeGenExp exps >>= call "primTimeGE"
                    TimeGT     -> mapM codeGenExp exps >>= call "primTimeGT"
                    TimeLT     -> mapM codeGenExp exps >>= call "primTimeLT"
                    TimeLE     -> mapM codeGenExp exps >>= call "primTimeLE"
                    TimeMin    -> mapM codeGenExp exps >>= call "primTimeMin"
                    Refl       -> mapM codeGenExp exps >>= call "primRefl"
                    Sec        -> mapM codeGenExp exps >>= call "sec"
                    PidEQ      -> primIcmp IcmpEQ exps
                    PidNE      -> primIcmp IcmpNE exps
                    ListArray  -> mapM codeGenExp exps >>= call "primListArray"
                    IndexArray -> do
                      [_,arr,i] <- mapM codeGenExp exps
                      getelementptr' poly [intConst 2,i] arr >>= load
                    ShowFloat  -> mapM codeGenExp exps >>= call "primShowFloat"
                    SizeArray  -> do
                      [_,arr] <- mapM codeGenExp exps 
                      getstructelemptr "size" arr >>= load
                    Raise      -> mapM codeGenExp exps >>= call "Raise"
                    EmptyArray -> mapM codeGenExp exps >>= call "EmptyArray"
                    Millisec   -> mapM codeGenExp exps >>= call "millisec"
                    UniArray   -> mapM codeGenExp exps >>= call "primUniArray"
                    CloneArray -> mapM codeGenExp exps >>= call "CloneArray"
                    MicrosecOf -> mapM codeGenExp exps >>= call "microsecOf"
                    SecOf      -> mapM codeGenExp exps >>= call "secOf"
                    Abort      -> mapM codeGenExp exps >>= call "ABORT"
                    TIMERTERM  -> mapM codeGenExp exps >>= call "primTIMERTERM"
                    _ -> fail $ show name ++ " " ++ show exps

codeGenECall name exps = do
  exps' <- mapM codeGenExp exps
  let fname = k2llvmName name
  call fname exps'

codeGenClosCall :: LLVMValue -> [AType] -> [Exp] -> CGState LLVMValue
codeGenClosCall funAddr typs exps = mapM codeGenExp exps >>= callhigher funAddr poly

codeGenCall :: String -> [Exp] -> CGState LLVMValue
codeGenCall fname exps = mapM codeGenExp exps >>= call fname
{-
codeGenNew :: LLVMType -> Int -> CGState LLVMValue
codeGenNew typ size = do
  r1 <- alloca (ptr int)
  let size' = intConst (LLVMKindle.words size)
  callvoid "new" [r1,size']
  bitcast (ptr typ) r1
-}
codeGenNew'' :: Maybe LLVMValue -> LLVMType -> Int -> CGState LLVMValue
codeGenNew'' (Just r1) typ size = do
  r2 <- bitcast (ptr $ ptr int) r1
  let size' = intConst (LLVMKindle.words size)
  callvoid "new" [r2,size']
  return r1
codeGenNew'' Nothing typ size = do
{-  r1 <- alloca (ptr int)
  let size' = intConst (LLVMKindle.words size)
  callvoid "new" [r1,size']
  bitcast (ptr typ) r1-}
  r1 <- alloca typ
  r2 <- bitcast (ptr (ptr int)) r1
  let size' = intConst (LLVMKindle.words size)
  callvoid "new" [r2,size']
  return r1
--  load r1

isBigTuple n = isTuple n && width n > 4

initModule n ns bs = do
  enterNewScope
  let fname = "_init_" ++ modToundSc (str n)
      var    = LLVMRegister bool "@INITIALIZED"
      varptr = LLVMRegister (ptr bool) "@INITIALIZED"
  addTopLevelConstant var false [Internal,Global]
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
  leaveScope
  addCurrFunction fname void []

initImports ns = mapM f ns
    where 
      f n = addExternalFun (fname n) void [] >> callvoid (fname n) []
      fname n = "_init_" ++ modToundSc (str n)

initStructs binds = do mapM_ k2llvmValBinds' (groupMap binds) 
--                       fail $ show (groupMap (filter isInitVal binds))
    where     
      k2llvmValBinds' (r,binds) = k2llvmValBinds (r, filter isInitVal binds)
      isInitVal (_,Val _ (ECall (Prim GCINFO _) _ _)) = False
      isInitVal (_,Val _ _) = True
      isInitVal _ = False

-- primitive "prelude" with list struct types
codeGenPrelude = do
  addStruct "LIST"  [("GCINFO",poly)]
  addStruct "CONS"  [("GCINFO",poly), 
                     ("a", poly), 
                     ("b", liststruct)]
  addStruct "TUP2"  [("GCINFO",poly), 
                     ("a", poly), 
                     ("b", poly)] 
  addStruct "TUP3"  [("GCINFO",poly), 
                     ("a", poly), 
                     ("b", poly), 
                     ("c", poly)] 
  addStruct "TUP4"  [("GCINFO",poly), 
                     ("a", poly), 
                     ("b", poly), 
                     ("c", poly), 
                     ("d", poly)]
  addStruct "TUPLE" [("GCINFO",poly), 
                     ("size", int), 
                     ("elems",array 0 poly)]
  addStruct "CLOS1" [("GCINFO",poly), 
                     ("Code", (ptr (fun poly [clos1struct, 
                                               poly])))]
  addStruct "CLOS2" [("GCINFO",poly), 
                     ("Code", (ptr (fun poly [clos2struct, 
                                               poly, 
                                               poly])))]
  addStruct "CLOS3" [("GCINFO",poly), 
                     ("Code", (ptr (fun poly [clos3struct, 
                                               poly, 
                                               poly, 
                                               poly])))]
  addStruct "CLOS"  [("GCINFO",poly),
                     ("Code", (ptr (fun void [])))]
  
  addStruct "PTHREAD_DESCR_STRUCT" [("a",opaque)]
  addStruct "PTHREAD_FASTLOCK" [("a",int),
                                ("b",int)]
  addStruct "PTHREAD_MUTEX_T" [("a",int),
                               ("b",int),
                               ("c",ptr (struct "PTHREAD_DESCR_STRUCT")),
                               ("d",int),
                               ("e",struct "PTHREAD_FASTLOCK")]
  addStruct "Ref"   [("GCINFO",   poly),
                     ("mut",      struct "PTHREAD_MUTEX_T"),
                     ("STATE",    poly)]
  addStruct "AbsTime" [("a",     int),
                       ("b",     int)]
  addStruct "Msg"   [("GCINFO",   (poly)),
                     ("Code",     (ptr (fun int [msgstruct]))),
                     ("baseline", struct "AbsTime"),
                     ("deadline", struct "AbsTime"),
                     ("next",     msgstruct)]
  addStruct "Time"  [("GCINFO",   (poly)),
                     ("sec",      int),
                     ("usec",     int)]
  addStruct "EITHER" [("GCINFO", poly),
                      ("Tag", int)]
  addStruct "LEFT"   [("GCINFO", poly),
                      ("Tag", int),
                      ("a", poly)]
  addStruct "RIGHT"  [("GCINFO", poly),
                      ("Tag", int),
                      ("a", poly)]
  addStruct "FloatCast" [("float", float)]
  addStruct "WORLD" [("",opaque)] 
  addStruct "Array" [("GCINFO",poly),
                     ("size", int),
                     ("elems", array 0 poly)]
  addStruct "Timer" [("GCINFO",poly),
                     ("reset",ptr (fun unit [timerstruct,int])),
                     ("sample",ptr (fun timestruct [timerstruct,int]))]

  addExternalFun "new"     void [ptr (ptr int), int]
  addExternalFun "LOCK"    (ptr int) [ptr int]
  addExternalFun "UNLOCK"  bit8 [ptr int]
  addExternalFun "INITREF" void [refstruct]
  addExternalFun "ASYNC"   bit8 [msgstruct,timestruct,timestruct]
  addExternalFun "primTimeMin" timestruct [timestruct,timestruct];
  addExternalFun "primTimePlus" timestruct [timestruct,timestruct];
  addExternalFun "primTimeMinus" timestruct [timestruct,timestruct];
  addExternalFun "primTimeEQ" bool [timestruct,timestruct];
  addExternalFun "primTimeNE" bool [timestruct,timestruct];
  addExternalFun "primTimeLT" bool [timestruct,timestruct];
  addExternalFun "primTimeLE" bool [timestruct,timestruct];
  addExternalFun "primTimeGT" bool [timestruct,timestruct];
  addExternalFun "primTimeGE" bool [timestruct,timestruct];
  addExternalFun "sec" timestruct [int]
  addExternalFun "millisec" timestruct [int]
  addExternalFun "microsec" timestruct [int]
  addExternalFun "secOf" int [timestruct]
  addExternalFun "microsecOf" int [timestruct]
  addExternalFun "RAISE"  void [int]
  addExternalFun "Raise" poly [bit32,int]
  addExternalFun "primRefl" poly [bit32,poly]
  addExternalFun "getStr" liststruct [ptr char]
  addExternalFun "strEq" int [liststruct,liststruct]
  addExternalFun "primListArray" arraystruct [bit32,liststruct]
  addExternalFun "primUniArray" arraystruct [bit32,int,poly]
  addExternalFun "EmptyArray" arraystruct [bit32,int]
  addExternalFun "CloneArray" arraystruct [bit32,arraystruct,int]
  addExternalFun "UpdateArray" arraystruct [bit32,arraystruct,int,poly]
  addExternalFun "primShowFloat" liststruct [float]
  addExternalFun "ABORT" bit8 [bit32,msgstruct,refstruct]
  addExternalFun "primTIMERTERM" timerstruct [int]

  addExternalGC "TUP2"   (array 0 int) -- (ptr word)
  addExternalGC "TUP3"   (array 0 int) -- (ptr word)
  addExternalGC "TUP4"   (array 0 int) -- (ptr word)
  addExternalGC "TUPLE"  (array 0 int) -- (ptr word)
  addExternalGC "CLOS1"  (array 0 int) -- (ptr word)
  addExternalGC "CLOS2"  (array 0 int) -- (ptr word)
  addExternalGC "CLOS3"  (array 0 int) -- (ptr word)
  addExternalGC "CLOS"   (array 0 int) -- (ptr word)
  addExternalGC "CONS"   (array 0 int) -- (ptr word)
  addExternalGC "EITHER" (array 0 int) -- (ptr word)
  addExternalGC "LEFT"   (array 0 int) -- (ptr word)
  addExternalGC "RIGHT"  (array 0 int) -- (ptr word)
  addExternalGC "Msg"    (array 0 int) -- (ptr word)
  addExternalGC "Timer"  (array 0 int) -- (ptr word)
  addExternalGC "Ref"    (array 0 int) -- (ptr word)
  addExternalGC "Time"   (array 0 int) -- (ptr word)
  addExternalGC "Array"  (array 0 int) -- (ptr word)
