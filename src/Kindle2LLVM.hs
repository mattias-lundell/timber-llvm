module Kindle2LLVM (kindle2llvm) where

import Kindle hiding (unit)
import Common hiding (name, Array)
import Name hiding (name)
import qualified Core
import qualified Core2Kindle
import qualified Prepare4C
import LLVMPP
import Prelude hiding (and,or,div,rem)
import LLVMPrim
import Env

import Data.Char (ord)
import LLVM
import LLVMKindle
import Depend (groupMap)

-- dsi contains structdefs Map Name Kindle.Decl
kindle2llvm e2 e3 m@(Module name _ _ _ _) = do
  let mod = runCodeGen (show name) (k2llvmModule e2 e3 m)
  -- tr $ show m
  return . render $ ppLLVMModule mod

k2llvmModule (Core.Module _ _ _ es' _ _ [bs']) dsi (Module moduleName importNames es decls binds) = do
  -- add structs from imported files
  let te2 = Core.tsigsOf bs' ++ extsMap es'
  tei <- Core2Kindle.c2kTEnv dsi te2
  let env1 = Prepare4C.addTEnv (primTEnv++tei++es) (Prepare4C.addDecls (primDecls++dsi) Prepare4C.env0)
      env  = Prepare4C.addTEnv (mapSnd typeOf binds) (Prepare4C.addDecls decls env1)
      ktypedEs = Prepare4C.pDecls env dsi
      -- add functions declarations from imported files
      ktypedEf = mapSnd Prepare4C.pType tei
  -- gen primitive functions
  genPrimitives
  -- struct declarations
  k2llvmStructDecls (decls ++ ktypedEs)
  mapM_ (\(sname,_) ->
             addExternalGC (k2llvmName sname) (array 0 int)) ktypedEs
  k2llvmAddExternals (ktypedEf ++ es)
  k2llvmAddGlobalVars binds
  k2llvmHarvestFunTypes binds
  k2llvmTopBinds binds
  -- create _init_ module function
  k2llvmInitModule moduleName importNames binds
  getModule

-- | Add external functions and global variable bindings
k2llvmAddExternals binds = mapM_ f binds
    where
      f (fname, FunT _ intyps outtyp) = do
        let outtyp' = k2llvmType outtyp
            intyps' = map k2llvmType intyps
            fname'  = k2llvmName fname
        addFunType fname' (ptr (fun outtyp' intyps'))
        addExternalFun fname' outtyp' intyps'
      f (vname, ValT vtyp) = do
        let vname' = k2llvmName vname
            vtyp'  = k2llvmType vtyp
            reg    = LLVMRegister (ptr vtyp') vname'
                     (TagGlobal [External,Global] Nothing)
        addGlobalVar vname' reg

-- | Generate type aliases for all struct declarations
k2llvmStructDecls sdecls = mapM_ f sdecls
    where
      f (sname, Struct _ vars _) = do
        let sname' = k2llvmName sname
            vars' = map (fixvars sname') vars
        addStruct sname' vars'
      fixvars sname (name, ValT typ) = (k2llvmName name, k2llvmType typ)
      fixvars sname (name, FunT _ argtyps restyp) =
          (k2llvmName name, ptr (fun (k2llvmType restyp)
                                (ptr (struct sname) : map k2llvmType argtyps)))

-- | Harvest all functions types from the current file, llvm needs type
--   information when generating function calls
k2llvmHarvestFunTypes binds = mapM_ f binds
    where f (fname, Fun _ atype atenv _) =
              addFunType (k2llvmName fname)
                         (ptr (fun (k2llvmType atype)
                                   (map k2llvmType (snd (unzip atenv)))))
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
              freg' <- bitcast (ptr (fun void [])) freg
              store freg' r1
            else store freg r1
      f (x,_) = internalError0 ("k2llvmStructBinds " ++ show x)

-- | Add global variables from the current module, they must be avilible
--   when generating function code.
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

-- | Process toplevel bindings
k2llvmTopBinds binds = mapM_ f binds
          -- Generate code for a function
    where f b@(fname, Fun [] funtyp typenv cmds) = do
            -- Initiate local variable context with all globals
            globals <- cgmGets cgmGlobals
            cgfPut (cgf0 (k2llvmName fname) globals)
            -- Add function parameters
            -- CLEAN create registers, not list with tuples
            params <- atenv2params typenv
            mapM_ addParams params
            -- Generate code
            k2llvmCmd cmds
            -- Add unreachable (if function ends with a function call)
            unreachable
            addCurrFunction (k2llvmType funtyp) params
          -- Create GCINFO array
          f b@(vname, Val _ (ECall (Prim GCINFO _) [] vs@(EVar v : _))) = do
            vals <- mapM (gcArray v) vs
            addLocalGC (array (length vals) int) (k2llvmName vname) vals
          f _ = return ()

atenv2params :: ATEnv -> CodeGen [(String,LLVMType)]
atenv2params ps = return [(k2llvmName v, k2llvmType typ) | (v,typ) <- ps]

-- | Add function parameters, all parameters are allocated in the toplevel
--   basic block, this way llvm handles conversation from memory to register.
--   when using -mem2reg optimization pass.
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

-- | Generate llvm representation of a GC array
gcArray v (EVar x)
    | x == v = do
         size <- getStructSize (struct (k2llvmName v))
         return $ intConst (LLVMKindle.words size)
    | otherwise = do
         offset <- getStructOffset (struct (k2llvmName v)) (k2llvmName x)
         return $ intConst (LLVMKindle.words offset)
gcArray v (ELit lit) = return $ lit2const lit


k2llvmValBinds (_,binds) = mapM_ f binds >> mapM_ g binds
    where f (vname, Val vtyp (ENew ntyp [] binds)) = do
            let typ = k2llvmType vtyp
                name = k2llvmName vname
            size <- getStructSize (dropPtrs typ)
            var <- lookupVar name
            r1 <- k2llvmNew var typ size
            addVar name r1
          f (vname, Val vtyp (ECast _ (ENew ntyp [] binds))) = do
            let objtyp = k2llvmType vtyp
                castyp = struct (k2llvmName ntyp)
                sname  = k2llvmName ntyp
                vname' = k2llvmName vname
            var <- lookupVar vname'
            case var of
              Just reg -> do
                       size <- getStructSize castyp
                       r1 <- k2llvmNew (Just reg) (ptr castyp) size
                       return ()
              Nothing -> do
                       size <- getStructSize castyp
                       r1 <- k2llvmNew Nothing (ptr castyp) size
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
                  r3 <- getelementptr int [r2] r1
                  r5 <- getstructelemptr "GCINFO" r0
                  store r3 r5

lit2const (LInt _ n) = intConst n
lit2const (LChr _ c) = charConst c
lit2const (LRat _ r) = floatConst r

genStr s = do
  addString s
  r <- getString s
  call "getStr" [r]

k2llvmCmd :: Cmd -> CodeGen ()
k2llvmCmd (CRet exp1) = k2llvmExp exp1 >>= ret
k2llvmCmd (CRun exp1 cmd1) = do
  k2llvmExp exp1
  k2llvmCmd cmd1
k2llvmCmd (CBind False [(x,Val t (ENew n [] bs))] (CBind False [(y,Val tref (ENew (Prim Ref _) [] bs'))] c)) = do
  let objtyp = (struct (k2llvmName n))
  refsize <- getStructSize refstruct
  objsize <- getStructSize objtyp
  r1 <- k2llvmNew Nothing refstruct (refsize+objsize)
  addVar (k2llvmName y) r1
  r2 <- load r1
  callvoid "INITREF" [r2]
  k2llvmStructBinds (ECast t (ESel (EVar y) (prim STATE))) n bs
  k2llvmCmd c
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
k2llvmCmd (CSwitch e alts) = do
  e' <- k2llvmExp e
  switchEnd <- getNextLabel
  addBreakLabel switchEnd
  k2llvmSwitch e' alts switchEnd
  br switchEnd
  label switchEnd
  dropBreakLabel
k2llvmCmd (CSeq cmd1 cmd2) = k2llvmCmd cmd1 >> k2llvmCmd cmd2
k2llvmCmd CBreak = getBreakLabel >>= br
k2llvmCmd (CRaise exp1) = do
  exp <- k2llvmExp exp1
  callvoid "RAISE" [exp]
k2llvmCmd (CWhile exp1 cmd1 cmd2) = do
  loopCond <- getNextLabel
  loopBody <- getNextLabel
  loopEnd  <- getNextLabel
  addContinueLabel loopCond
  addBreakLabel loopEnd
  br loopCond
  -- loop body
  label loopBody
  k2llvmCmd cmd1
  br loopCond
  -- loop condition
  label loopCond
  exp <- k2llvmExp exp1
  icmp IcmpEQ exp true >>= condbr loopBody loopEnd
  -- loop end
  label loopEnd
  dropBreakLabel
  dropContinueLabel
  k2llvmCmd cmd2
k2llvmCmd CCont = getContinueLabel >>= br

-- | Generate code for switch command
k2llvmSwitch e (ALit lit cmd : alts) end = do
  trueBlock  <- getNextLabel
  falseBlock <- getNextLabel
  cmp lit >>= condbr trueBlock falseBlock
  label trueBlock
  k2llvmCmd cmd
  br falseBlock
  label falseBlock
  k2llvmSwitch e alts end
      where cmp (LStr _ lit) = do
              str <- genStr lit
              call "strEq" [e, str] >>= trunc bool
            cmp lit@(LRat _ _) = fcmp FcmpOEQ e (lit2const lit)
            cmp _              = icmp IcmpEQ e (lit2const lit)
k2llvmSwitch _ [AWild c] end = k2llvmCmd c
k2llvmSwitch _ []        end = br end

k2llvmExp :: Exp -> CodeGen LLVMValue
k2llvmExp (EVar (Prim Inherit _)) = return $ LLVMConstant timestruct NullConst
k2llvmExp (EVar name) = getVar (k2llvmName name) >>= load
k2llvmExp EThis = internalError0 "k2llvmExp EThis"
k2llvmExp (ELit (LStr _ s)) = genStr s
k2llvmExp (ELit lit) = return $ lit2const lit
-- select in a n-tuple with n > 4
k2llvmExp (ESel (ECast (TCon n _) exp) name) | isBigTuple n = do
  r1 <- k2llvmExp exp
  -- only possible to handle select from 'a' to 'z'
  -- i.e. only up to 26-tuples
  let offset = ord (head (show name)) - 96
  -- the "2" is used to direct into the elem part of the struct
  getelementptr poly [intConst 2,intConst offset] r1 >>= load
    where
      isBigTuple n = isTuple n && width n > 4
-- select on casted struct
k2llvmExp (ESel (ECast atype exp) name) = do
  -- perform cast from struct to struct
  let totype@(Tptr totype_noptr) = k2llvmType atype
  -- cast
  r2 <- bitcast totype =<< k2llvmExp exp
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
      funtyp = ptr (Tfun rettyp' (map k2llvmType (clos:typs)))
  funAddr <- k2llvmExp fun
  (offset,typ) <- getStructIndex typ_noptr (k2llvmName fname)
  r1 <- bitcast funtyp =<< load =<< getelementptr typ [intConst offset] funAddr
  exps' <- mapM k2llvmExp exps
  callhigher r1 funtyp (funAddr:exps')
k2llvmExp (EEnter exp fname atypes exps) = do
  r1 <- k2llvmExp exp
  let typ@(Tptr typ_noptr) = getTyp r1
  (offset,typ) <- getStructIndex typ_noptr (k2llvmName fname)
  r3 <- load =<< getelementptr typ [intConst offset] r1
  exps' <- mapM k2llvmExp (exp:exps)
  callhigher r3 typ exps'
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

primBitShift f exps = do
  [op1, op2] <- mapM k2llvmExp exps
  let bittyp = getTyp op1
  r1 <- zext int op1
  r2 <- f r1 op2
  trunc bittyp r2

codeGenECall :: Name -> [Exp] -> CodeGen LLVMValue
codeGenECall (Prim name _) exps
    | name `elem` [AND8,AND16,AND32]             = primBin and exps
    | name `elem` [OR8,OR16,OR32]                = primBin or exps
    | name `elem` [EXOR8,EXOR16,EXOR32]          = primBin xor exps
    | name `elem` [SHIFTL8,SHIFTL16,SHIFTL32]    = primBitShift shl exps
    | name `elem` [SHIFTR8,SHIFTR16,SHIFTR32]    = primBitShift lshr exps
    | name `elem` [SHIFTRA8,SHIFTRA16,SHIFTRA32] = primBitShift ashr exps
    | name `elem` [SET8,SET16,SET32]             = do
          [op1,op2] <- mapM k2llvmExp exps
          let (Tint n) = getTyp op1
          r1 <- bitcast (Tvector n bool) op1
          r2 <- insertelement r1 true op2
          bitcast (Tint n) r2
    | name `elem` [CLR8,CLR16,CLR32]             = do
          [op1,op2] <- mapM k2llvmExp exps
          let (Tint n) = getTyp op1
          r1 <- bitcast (Tvector n bool) op1
          r2 <- insertelement r1 false op2
          bitcast (Tint n) r2
    | name `elem` [TST8,TST16,TST32]             = do
          [op1,op2] <- mapM k2llvmExp exps
          let (Tint n) = getTyp op1
          r1 <- bitcast (Tvector n bool) op1
          r2 <- extractelement r1 op2
          and true r2
    | name `elem` [NOT8,NOT16,NOT32]             = do
          [op1] <- mapM k2llvmExp exps
          let (Tint n) = getTyp op1
          bitcast (Tvector n bool) op1 >>= xor (vectorConst (replicate n true)) >>= bitcast (Tint n)
    | otherwise = case name of
                    -- Integer arithmetic
                    IntPlus  -> primBin add exps
                    IntMinus -> primBin sub exps
                    IntTimes -> primBin mul exps
                    IntDiv   -> primBin div exps
                    IntMod   -> primBin rem exps
                    IntNeg   -> do
                              [exp] <- mapM k2llvmExp exps
                              sub (intConst 0) exp
                    -- Integer comparison
                    IntEQ -> primIcmp IcmpEQ exps
                    IntNE -> primIcmp IcmpNE exps
                    IntGT -> primIcmp IcmpSGT exps
                    IntGE -> primIcmp IcmpSGE exps
                    IntLT -> primIcmp IcmpSLT exps
                    IntLE -> primIcmp IcmpSLE exps
                    -- Float arithmetic
                    FloatPlus -> primBin fadd exps
                    FloatMinus -> primBin fsub exps
                    FloatTimes -> primBin fmul exps
                    FloatDiv   -> primBin fdiv exps
                    FloatNeg  -> do
                              [exp] <- mapM k2llvmExp exps
                              fsub (floatConst 0) exp
                    -- Float comparison
                    FloatEQ -> primFcmp FcmpUEQ exps
                    FloatNE -> primFcmp FcmpUNE exps
                    FloatGT -> primFcmp FcmpUGT exps
                    FloatGE -> primFcmp FcmpUGE exps
                    FloatLT -> primFcmp FcmpULT exps
                    FloatLE -> primFcmp FcmpULE exps
                    -- Float show
                    ShowFloat  -> mapM k2llvmExp exps >>= call "primShowFloat"
                    -- Typecasts
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
                    -- Short-cut AND
                    LazyAnd -> do
                              let [e1,e2] = exps
                              r1 <- alloca bool
                              firstTrue  <- getNextLabel
                              firstFalse <- getNextLabel
                              end <- getNextLabel
                              op1 <- k2llvmExp e1
                              icmp IcmpEQ op1 true >>=
                                   condbr firstTrue firstFalse
                              label firstTrue
                              op2 <- k2llvmExp e2
                              r2 <- icmp IcmpEQ op2 true
                              store r2 r1
                              br end
                              label firstFalse
                              store false r1
                              br end
                              label end
                              load r1
                    -- Short-cut OR
                    LazyOr -> do
                              let [e1,e2] = exps
                              r1 <- alloca bool
                              firstTrue <- getNextLabel
                              firstFalse <- getNextLabel
                              end <- getNextLabel
                              op1 <- k2llvmExp e1
                              icmp IcmpEQ op1 true >>=
                                   condbr firstTrue firstFalse
                              label firstTrue
                              store true r1
                              br end
                              label firstFalse
                              op2 <- k2llvmExp e2
                              r2 <- icmp IcmpEQ op2 true
                              store r2 r1
                              br end
                              label end
                              load r1
                    -- Time arithmetic
                    TimeMinus  -> mapM k2llvmExp exps >>= call "primTimeMinus"
                    TimePlus   -> mapM k2llvmExp exps >>= call "primTimePlus"
                    -- Time comparison
                    TimeEQ     -> mapM k2llvmExp exps >>= call "primTimeEQ"
                    TimeNE     -> mapM k2llvmExp exps >>= call "primTimeNE"
                    TimeGE     -> mapM k2llvmExp exps >>= call "primTimeGE"
                    TimeGT     -> mapM k2llvmExp exps >>= call "primTimeGT"
                    TimeLT     -> mapM k2llvmExp exps >>= call "primTimeLT"
                    TimeLE     -> mapM k2llvmExp exps >>= call "primTimeLE"
                    TimeMin    -> mapM k2llvmExp exps >>= call "primTimeMin"
                    -- Time constructors
                    Sec        -> mapM k2llvmExp exps >>= call "sec"
                    Millisec   -> mapM k2llvmExp exps >>= call "millisec"
                    MicrosecOf -> mapM k2llvmExp exps >>= call "microsecOf"
                    SecOf      -> mapM k2llvmExp exps >>= call "secOf"
                    Refl       -> mapM k2llvmExp exps >>= call "primRefl"
                    -- Get new timer
                    TIMERTERM  -> mapM k2llvmExp exps >>= call "primTIMERTERM"
                    -- Pid comparison
                    PidEQ      -> primIcmp IcmpEQ exps
                    PidNE      -> primIcmp IcmpNE exps
                    -- Array functions
                    ListArray  -> mapM k2llvmExp exps >>= call "primListArray"
                    UniArray   -> mapM k2llvmExp exps >>= call "primUniArray"
                    CloneArray -> mapM k2llvmExp exps >>= call "CloneArray"
                    EmptyArray -> mapM k2llvmExp exps >>= call "EmptyArray"
                    IndexArray -> do
                      [_,arr,i] <- mapM k2llvmExp exps
                      getelementptr poly [intConst 2,i] arr >>= load
                    SizeArray  -> do
                      [_,arr] <- mapM k2llvmExp exps
                      getstructelemptr "size" arr >>= load
                    -- Message functions
                    LOCK   -> mapM k2llvmExp exps >>= call "LOCK"
                    UNLOCK -> mapM k2llvmExp exps >>= call "UNLOCK"
                    ASYNC ->  mapM k2llvmExp exps >>= call "ASYNC"
                    -- Various functions
                    Raise      -> mapM k2llvmExp exps >>= call "Raise"
                    Abort      -> mapM k2llvmExp exps >>= call "ABORT"
                    _ -> fail $ show name ++ " " ++ show exps
codeGenECall name exps = do
  exps' <- mapM k2llvmExp exps
  let fname = k2llvmName name
  call fname exps'

k2llvmNew :: Maybe LLVMValue -> LLVMType -> Int -> CodeGen LLVMValue
k2llvmNew (Just r1) typ size = do
  r2 <- bitcast (ptr $ ptr int) r1
  let size' = intConst (LLVMKindle.words size)
  callvoid "new" [r2,size']
  return r1
k2llvmNew Nothing typ size = do
  r1 <- alloca typ
  r2 <- bitcast (ptr (ptr int)) r1
  let size' = intConst (LLVMKindle.words size)
  callvoid "new" [r2,size']
  return r1

-- | Generate "_init_"-function
k2llvmInitModule n ns bs = do
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
