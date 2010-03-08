{-# LANGUAGE ParallelListComp #-}

-- The Timber compiler <timber-lang.org>
--
-- Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the names of the copyright holder and any identified
--    contributors, nor the names of their affiliations, may be used to 
--    endorse or promote products derived from this software without 
--    specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
-- OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

module Prepare4C where

import Monad
import Common
import Kindle
import PP
import qualified Core
import qualified Core2Kindle

-- Establishes that:
--   every struct function simply relays all work to a global function with an explicit "this" parameter
--   every EEnter expression has a variable as its body
--   every ENew expression occurs at the rhs of a Val binding

-- Removes the type arguments from all type constructors except Array
-- Replaces type variables with the special type constructor POLY
-- Adds type casts wherever necessary

-- Replaces type abstraction and application with BITSET parameters indicating pointer/non-pointer status

-- Replaces nullary structs with out-of-band pointers (casts from the corresponding tag value)
-- Replaces type constructor switching by switching on integer tags (embedded as pointer values or as explicit tag fields)
-- Makes field selection from matched struct variants explicit

-- Removes CBreak commands that appear directly under a CSeq (without any intervening CSwitch)
-- Removes redundant defaults in CSwitch commands

-- (Not yet):
-- Flattens the struct subtyping graph by inlining the first coercion field
-- Ditto for enums...


prepare4c e2 e3 m             = localStore (pModule e2 e3 m)


-- ===========================
-- Local environment
-- ===========================

data Env                        = Env { decls   :: Decls,                 -- all structs in scope
                                        tenv    :: TEnv,                  -- all fun and val variables in scope
                                        strinfo :: Map Name (Int,[Bool]), -- number of ptr fields + 1 / relevance flags of tvars for each struct
                                        polyenv :: Map Name (Exp,Int),    -- status location (refexp,bit-number) for type variables in scope
                                        conval  :: Map Name Int,          -- numerical values of all variant constructor names
                                        nulls   :: [Name],                -- lists all nullary struct variants
                                        tagged  :: [Name],                -- lists all struct variants that use an explicit tag field
                                        this    :: Maybe Name }

env0                            = Env { decls = [], tenv = [], strinfo = [], polyenv = [], conval = [], nulls = [], tagged = [], this = Nothing }

addDecls ds env                 = env { decls = ds ++ decls env, strinfo = info ++ strinfo env, conval = convals ++ conval env, 
                                        nulls = nullcons ++ nulls env, tagged = taggedcons ++ taggedunions ++ tagged env }
  where unioncons               = unions ds
        allvariants             = map (variants ds) unioncons
        convals                 = concat (map (`zip` [0..]) allvariants)
        nullcons                = [ n | (n, Struct _ [] (Extends n' _ _)) <- ds, n' `elem` unioncons ]
        singlecons              = map head . filter ((==1) . length) . map (\\nullcons) $ allvariants
        taggedcons              = dom convals \\ (singlecons++nullcons)
        taggedunions            = [ n | (n,ns) <- unioncons `zip` allvariants, any (`elem` taggedcons) ns ]
        info                    = mapSnd structInfo ds
        
structInfo (Struct vs te _)     = (length (ptrFields te []), map (`elem` relevant) vs)
  where relevant                = rng (varFields te)

addTEnv te env                  = env { tenv = te ++ tenv env }

addVals te env                  = env { tenv = mapSnd ValT te ++ tenv env }

addPolyEnv vs xs env            = env { polyenv = vs `zip` map f [0..] ++ polyenv env }
  where f n                     = let (i,j) = bitSplit n in (EVar (xs !! i), j)

bitSplit i                      = (i `div` 32, i `mod` 32)

setThis x env                   = env { this = Just x }

findValT te x                   = t
  where ValT t                  = lookup' te x
  
findFunT te x ts                = (vs `zip` ts, ts', t)
  where FunT vs ts' t           = lookup' te x

findStructTEnv env (TCon n ts)
  | isTuple n                   = (abcSupply `zip` ts, abcSupply `zip` map (ValT . tVar) (take (width n) abcSupply))
  | n == prim CLOS              = ([], closBind ts)
  | otherwise                   = (vs `zip` ts, te)
  where Struct vs te _          = lookup' (decls env) n

findStructEQuant env n ts
  | isTuple n                   = []
  | n == prim CLOS              = []
  | otherwise                   = [ t | (v,t) <- vs `zip` ts, v `elem` vs' ]
  where Struct vs _ l           = lookup' (decls env) n
        vs'                     = equants l

findStructInfo env n
  | isTuple n                   = (width n + 1, take (width n) (repeat True))
  | n == prim CLOS              = (1, [])
  | otherwise                   = lookup' (strinfo env) n

findPolyTag xx env v            = lookup' (polyenv env) v

conLit env n                    = lInt (lookup' (conval env) n)

allCons env (ACon n _ _ _ : _)
  | isTuple n                   = [n]
  | otherwise                   = variants (decls env) n0
  where Struct _ _ (Extends n0 _ _) = lookup' (decls env) n
allCons env _                   = []


-- =====================================
-- Replacing polymorphism with polyTags
-- =====================================
    
-- Create a list of polyTag types on basis of polymorphic arity
polyTagTypes 0                  = []
polyTagTypes a | a <= 32        = [tBITS32]
               | otherwise      = tBITS32 : polyTagTypes (a-32)


-- Create a polyTag TEnv on basis of polymorphic arity (the externally visible part)
polyTagEnv0 a | a <= 4          = [(prim GCINFO, ValT tPOLY)]
              | otherwise       = (prim GCINFO, ValT tPOLY) : 
                                  _ABCSupply `zip` map ValT (polyTagTypes a)


-- Create a polyTag TEnv on basis of polymorphic arity (the existentially quantified part)
polyTagEnv1 a                   = _abcSupply `zip` map ValT (polyTagTypes a)


-- Create a polyTag struct binding from a list of type arguments
polyTagBinds env n ts           = bs0 ++ bs1
  where mkBind (x,ValT t) e     = (x, Val t e)
        ts0                     = zipFilter vflags ts
        ts1                     = findStructEQuant env n ts
        l_ts0                   = length ts0
        te0                     = polyTagEnv0 l_ts0
        es0                     = polyTagArgs env ts0
        bs0 | l_ts0 <= 4        = zipWith mkBind te0 [ECall n [] (map offset es0)]
            | otherwise         = zipWith mkBind te0 (ECall n [] [] : es0)
        bs1                     = zipWith mkBind (polyTagEnv1 (length ts1)) (polyTagArgs env ts1)
        offset e                = ECall (prim IntTimes) [] [ELit (lInt (d + 2)),e]
        (d,vflags)              = findStructInfo env n


-- Create a list of polyTag Exp arguments from a list of type arguments
polyTagArgs env []              = []
polyTagArgs env ts 
  | vars && ordered && total    = [e0]
  where vars                    = l_ts == length vs && length (nub es) == 1
        l_ts                    = length ts
        vs                      = [ n | TVar n _ <- ts ]
        (es,is)                 = unzip (map (findPolyTag "XX" env) vs)
        e0                      = head es
        ordered                 = is == [0..l_ts-1]
        total                   = length [ v | (v,(e,_)) <- polyenv env, e == e0 ] == l_ts
polyTagArgs env ts              = args (length ts) ts
  where 
    args 0 []                   = []
    args a ts | a <= 32         = [arg 0 ts]
              | otherwise       = arg 0 ts0 : args (a-32) ts1
      where (ts0,ts1)           = splitAt a ts

    arg k []                    = intlit 0
    arg k (TThis n : ts)        = bor (arg (k+1) ts) (shift (band e (mask j)) j k)
      where (i,j)               = bitSplit n
            e                   = ESel (EVar (fromJust (this env))) (_abcSupply !! i)
    arg k (TVar n _ : ts)       = bor (arg (k+1) ts) (shift (band e (mask j)) j k)
      where (e,j)               = findPolyTag "YY" env n
    arg k (TCon n _ : ts)
      | n `elem` scalars        = bor (mask k) (arg (k+1) ts)
    arg k (_ : ts)              = arg (k+1) ts


shift e i k | i < k             = shiftL e (ELit (lInt (k-i)))
            | i > k             = shiftR e (ELit (lInt (i-k)))
            | otherwise         = e

shiftL e1 e2                    = ECall (prim SHIFTL32) [] [e1,e2]
         
shiftR e1 e2                    = ECall (prim SHIFTR32) [] [e1,e2]

mask i                          = intlit (2^i)

bor e1 e2 | e1 == intlit 0      = e2
          | e2 == intlit 0      = e1
          | otherwise           = ECall (prim OR32) [] [e1,e2]
             
band e1 e2                      = ECall (prim AND32) [] [e1,e2]

intlit i                        = ECast tBITS32 (ELit (lInt i))


-- =============================
-- Prepare modules and types
-- =============================
    
pModule (Core.Module _ _ _ es' _ _ [bs']) dsi (Module m ns es ds bs)      
                                = do let te2 = Core.tsigsOf bs' ++ extsMap es'
                                     tei <- Core2Kindle.c2kTEnv dsi te2 
                                     let env1 = addTEnv (primTEnv++tei++es) (addDecls (primDecls++dsi) env0)
                                         env  = addTEnv (mapSnd typeOf bs) (addDecls ds env1)
                                     (bf,bs) <- pBinds pBind env bs
                                     bs' <- currentStore
                                     return (Module m ns es (pDecls env ds) (gcinfo env ds ++ flatBinds bf ++ bs ++ reverse bs'))


-- Prepare structs declarations
pDecls env ds                   = map f ds
  where f (n,Struct vs te l)    = (n, Struct [] (polyTagEnv0 l_vs0 ++ tagSig ++ mapSnd pType te ++ polyTagEnv1 l_vs1) Top)
          where tagSig          = if n `elem` tagged env then [(prim Tag, ValT tInt)] else []
                l_vs0           = length (zipFilter vflags vs)
                l_vs1           = length (equants l)
                (_,vflags)      = findStructInfo env n


-- gcinfo types: these must match corresponding defines in gc.c
gcSTD                           = ELit (lInt 0)
gcARRAY                         = ELit (lInt 1)
gcTUPLE                         = ELit (lInt 2)
gcBIG                           = ELit (lInt 3)
gcMUT                           = ELit (lInt 4)

-- Generate gcinfo for structs
gcinfo env ds                   = map f (prune ds (nulls env))
  where f (n,Struct vs te _)    = (n, Val tPOLY (ECall (prim GCINFO) [] es))
          where es | l_vs1 <= 4 = concat [ EVar n : gcSTD : pad l_es0 (ptrFields te vs) | vs <- sampleSpaces vs1 ]
                   | otherwise  = EVar n : gcBIG : es0 ++ concat (map bitRef (varFields te)) ++ [ELit (lInt 0)]
                es0             = ptrFields te []
                l_es0           = length es0
                (d,vflags)      = findStructInfo env n
                vs1             = zipFilter vflags vs
                l_vs1           = length vs1
                idx             = vs1 `zip` [0..]
                bitRef (n,v)    = [ EVar n, ELit (lInt (i `div` 32 + 1)), ELit (lInt (i `mod` 32)) ]
                  where i       = lookup' idx v

pad n es                        = es ++ replicate (n - length es) (ELit (lInt 0))

ptrFields te vs                 = map EVar (dom (filter (isPtr vs) te)) ++ [ELit (lInt 0)]

sampleSpaces []                 = [[]]
sampleSpaces (v:vs)             = [ vs1 ++ vs2 | vs1 <- [[],[v]], vs2 <- sampleSpaces vs ]

isPtr vs (n,FunT _ _ _)         = False
isPtr vs (n,ValT (TVar v _))    = v `notElem` vs
isPtr vs (Prim Next _, _)       = False         -- the next field in Msg and its subtypes is custom handled during timerQ scanning.
isPtr vs (n,ValT (TCon k _))    = k `notElem` scalars


varFields te                    = [ (n,v) | (n,ValT (TVar v _)) <- te ]


-- Prepare types
pType (ValT t)                  = ValT (pAType t)
pType (FunT vs ts t)            = FunT [] (polyTagTypes (length vs) ++ map pAType ts) (pAType t)


-- Prepare atomic types: erase polymorphism and uniformly represent big tuples
pAType (TCon n _)               = TCon n []
pAType (TVar _ _)               = tPOLY
pAType (TThis _)                = tPOLY

pATEnv te                       = mapSnd pAType te


-- =============================
-- Prepare bindings and commands
-- =============================

-- Prepare bindings
pBinds f env xs                 = do (bfs,xs) <- fmap unzip (mapM (f env) xs)
                                     return (foldr (.) id bfs, xs)

-- Prepare top-level & cmd bindings (assume code is lambda-lifted)
pBind env (x, Val t e)          = do (bf,t',e) <- pRhsExp env e 
                                     return (bf, (x, Val (pAType t) (cast t t' e)))
pBind env (x, Fun vs t te c)    = do te' <- newEnv paramSym (polyTagTypes (length vs))
                                     c <- pCmd (addVals te (addPolyEnv vs (dom te') env)) t c
                                     return (id, (x, Fun [] (pAType t) (te' ++ pATEnv te) c))


-- Prepare struct bindings (assume code is lambda-lifted)
pSBind _ te0 env (x,Val t e)    = do (bf,e) <- pExpT env t e 
                                     return (bf, (x, Val (pAType t0) (cast t0 t e)))
  where t0                      = findValT te0 x
pSBind _ te0 env (x,Fun [] t te c@(CRet (ECall f [] (EThis:es))))
  | okAlready                   = return (id, (x, Fun [] t te c))
  where (_,ts0,t0)              = findFunT te0 x []
        okAlready               = t == pAType t0 && rng te == map pAType ts0 && es == map EVar (dom te)
pSBind ty te0 env (x,Fun vs t te c)
                                = do y <- newName thisSym
                                     te0 <- newEnv paramSym ts0
                                     te' <- newEnv paramSym (polyTagTypes (length vs))
                                     let bs0  = [ (x, Val t (cast t t0 (EVar x0))) | (x0,t0) <- te0 | (x,t) <- te ]
                                         te1  = [ if isEVar e then (x,t) else xt0 | (x, Val t e) <- bs0 | xt0 <- te0 ]
                                         bs1  = [ b | b@(_,Val _ e) <- bs0, not (isEVar e) ]
                                         te1' = te' ++ pATEnv te1
                                         env' = addPolyEnv vs (dom te') env
                                     c <- pCmd (setThis y (addVals ((y,ty):te) env')) t0 c
                                     f <- newName functionSym
                                     addToStore (f, Fun [] t0' ((y,pAType ty):te1') (cBind bs1 c))
                                     return (id, (x, Fun [] t0' te1' (CRet (ECall f [] (EThis : map EVar (dom te1'))))))
  where (_,ts0,t0)              = findFunT te0 x []
        t0'                     = pAType t0


-- Prepare commands
pCmd env t0 (CRet e)            = do (bf,e) <- pExpT env t0 e
                                     return (bf (CRet e))
pCmd env t0 (CRun e c)          = do (bf,_,e) <- pExp env e
                                     liftM (bf . CRun e) (pCmd env t0 c)
pCmd env t0 (CBind _ [] c)      = pCmd env t0 c
pCmd env t0 (CBind False bs c)  = do (bf,bs) <- pBinds pBind env bs
                                     liftM (bf . CBind False bs) (pCmd env' t0 c)
  where env'                    = addTEnv (mapSnd typeOf bs) env
pCmd env t0 (CBind True bs c)   = do (bf,bs) <- pBinds pBind env' bs
                                     liftM (CBind True (flatBinds bf ++ bs)) (pCmd env' t0 c)
  where env'                    = addTEnv (mapSnd typeOf bs) env
pCmd env t0 (CUpd x e c)        = do (bf,e) <- pExpT env (findValT (tenv env) x) e
                                     liftM (bf . CUpd x e) (pCmd env t0 c)
pCmd env t0 (CUpdS e x e' c)    = do (bf,t1,e) <- pExp env e
                                     let (s,te) = findStructTEnv env t1
                                     (bf',e') <- pExpT env (findValT te x) e'
                                     liftM (bf . bf' . CUpdS e x e') (pCmd env t0 c)
pCmd env t0 (CUpdA e i e' c)    = do (bf,TCon (Prim Array _) [t],e) <- pExp env e
                                     (bf',i) <- pExpT env tInt i
                                     (bf'',e') <- pExpT env tPOLY e'
                                     liftM (bf . bf' . bf'' . CUpdA e i e') (pCmd env t0 c)
pCmd env t0 (CSeq c c')         = liftM2 mkSeq (pCmd env t0 c) (pCmd env t0 c')
pCmd env t0 (CBreak)            = return CBreak
pCmd env t0 (CRaise e)          = do (bf,e) <- pExpT env tInt e
                                     return (bf (CRaise e))
pCmd env t0 (CWhile e c c')     = do (bf,e) <- pExpT env tBool e
                                     c <- pCmd env t0 c
                                     liftM (bf . CWhile e c) (pCmd env t0 c')
pCmd env t0 (CCont)             = return CCont
pCmd env t0 (CSwitch e alts)    
  | any litA alts               = if simple (litType (firstLit alts)) then
                                     do (bf,e) <- pExpT env tInt e
                                        alts <- mapM (pAlt env e tInt t0) alts
                                        return (bf (CSwitch e alts))
                                    else mkVarSwitch env t0 e alts
  | isEVar e || all nullA alts  = do (bf,t,e) <- pExp env e
                                     alts <- mapM (pAlt env e t t0) alts
                                     let (alts0,alts1) = partition nullA [ a | a@(ACon _ _ _ _) <- alts ]
                                         altsW         = [ a | a@(AWild _) <- alts ]
                                     return (bf (mkSwitch env e (alts0++absent0 altsW) (alts1++absent1 altsW)))
  | otherwise                   = mkVarSwitch env t0 e alts
  where nullA (ACon k _ _ _)    = k `elem` nulls env
        nullA _                 = False
        absent                  = allCons env alts \\ [ k | ACon k _ _ _ <- alts ]
        (abs0,abs1)             = partition (`elem` nulls env) absent
        absent0 []              = [ ACon k [] [] CBreak | k <- abs0 ]
        absent0 (AWild d : _)   = [ ACon k [] [] d | k <- abs0 ]
        absent1 altsW           = [ a | a <- altsW, not (null abs1) ]
        litA (ALit _ _)         = True
        litA _                  = False
        firstLit (ALit l _ : _) = l
        firstLit (_ : as)       = firstLit as
        simple (TCon (Prim Int _) []) = True
        simple (TCon (Prim Char _) []) = True
        simple _                = False


mkSwitch env (EVar _) [] []     = CBreak
mkSwitch env e [] []            = CSwitch e [AWild CBreak]
mkSwitch env e [] [ACon n _ _ c]
  | n `notElem` tagged env      = c

mkSwitch env e [] [AWild c]     = c
mkSwitch env e [] alts1         = CSwitch (ESel e (prim Tag)) (map (mkLitAlt env) alts1)
mkSwitch env e alts0@[ACon n _ _ c] []
  | allCons env alts0 == [n]    = c
mkSwitch env e alts0 []         = CSwitch (ECast tWORD e) (map (mkLitAlt env) alts0)
mkSwitch env e alts0 alts1      = mkSwitch env e (alts0++[AWild d]) []
  where d                       = mkSwitch env e [] alts1


mkVarSwitch env t0 e alts
  | isEVar e                    = do (bf,t,e) <- pExp env e
                                     alts <- mapM (pAlt env e t t0) alts
                                     return (bf (CSwitch e alts))
  | otherwise                   = do (bf,t,e) <- pExp env e
                                     x <- newName tempSym
                                     c <- pCmd (addVals [(x,t)] env) t0 (CSwitch (EVar x) alts)
                                     return (bf (cBind [(x,Val t e)] c))


mkLitAlt env (ACon n [] [] c)   = ALit (conLit env n) c
mkLitAlt env a                  = a


-- Prepare switch alternatives
pAlt env _ _ t0 (AWild c)       = liftM AWild (pCmd env t0 c)
pAlt env _ _ t0 (ALit l c)      = liftM (ALit l) (pCmd env t0 c)
pAlt env e (TCon _ ts) t0 (ACon k vs te c)
                                = do te' <- newEnv paramSym (polyTagTypes (length vs))
                                     c <- pCmd (addPolyEnv vs (dom te') (addVals te env)) t0 c
                                     return (ACon k [] [] (cBind (bs0 te' ++ bs1) c))
  where bs0 te'                 = zipWith mkBind te' (_abcSupply `zip` repeat (ValT tBITS32))
        (_,te0)                 = findStructTEnv env (TCon k (ts ++ map tVar vs))
        bs1                     = filter (not . isDummy . fst) (zipWith mkBind te te0)
        mkBind (x,t) (y,ValT u) = (x, Val (pAType t) (cast t u (ESel (ECast (tCon k) e) y)))
        
        

mkSeq c1 c2                     = case anchor c1 of
                                    (bf,CBreak) -> bf c2
                                    (bf,CCont)  -> c1
                                    _           -> CSeq c1 c2
  where anchor (CBind r bs c)   = (CBind r bs . bf, c')
          where (bf,c')         = anchor c
        anchor (CRun e c)       = (CRun e . bf, c')
          where (bf,c')         = anchor c
        anchor (CUpd x e c)     = (CUpd x e . bf, c')
          where (bf,c')         = anchor c
        anchor (CUpdS e x e' c) = (CUpdS e x e' . bf, c')
          where (bf,c')         = anchor c
        anchor (CUpdA e i e' c) = (CUpdA e i e' . bf, c')
          where (bf,c')         = anchor c
        anchor c                = (id, c)



-- =============================
-- Prepare expressions
-- =============================

-- Prepare a right-hand-side expression
pRhsExp env (ENew n ts bs)      = pNewExp env n ts bs
pRhsExp env (ECast t (ENew n ts bs))
                                = do (bf,t',e) <- pNewExp env n ts bs
                                     return (bf, t, cast t t' e)
pRhsExp env e                   = pExp env e


pNewExp env n ts bs
  | n == tuple 0                = return (id, t0, cast t0 tWORD (ELit (lInt 0)))
  | n `elem` nulls env          = return (id, t0, cast t0 tWORD (ELit (conLit env n)))
  | otherwise                   = do (bf,bs) <- pBinds (pSBind t0 te0) env bs
                                     return (bf, t0, ENew n [] (bs''++bs'++bs))
  where bs'                     = if n `elem` tagged env then [(prim Tag, Val tInt (ELit (conLit env n)))] else []
        bs''                    = polyTagBinds env n ts
        t0                      = TCon n ts
        (_,te0)                 = findStructTEnv env t0


pRefBind te0 env (x,Val _ e)    = do (bf,t,e) <- pRhsExp env e 
                                     return (bf, (x, Val (pAType t0) (cast t0 t e)))
  where t0                      = findValT te0 x


-- Prepare an expression in an arbitrary position and match its type with the expected one
pExpT env t0 e                  = do (bf,t,e) <- pExp env e
                                     return (bf, cast t0 t e)


cast t0 t1 e
  | u0 == u1                    = e
  | u0 == tPOLY && smallPrim u1 = ECast tPOLY (ECast tWORD e)
  | smallPrim u0 && u1 == tPOLY = ECast u0 (ECast tWORD e)
  | u0 == tPOLY && u1 == tFloat = ECall (prim Float2POLY) [] [e]
  | u0 == tFloat && u1 == tPOLY = ECall (prim POLY2Float) [] [e]
  | otherwise                   = ECast u0 e
  where u0                      = pAType t0
        u1                      = pAType t1

smallPrim (TCon n _)            = n `elem` smallTypes
smallPrim _                     = False


pExpTs env [] []                = return (id, [])
pExpTs env (t:ts) (e:es)        = do (bf1,e) <- pExpT env t e
                                     (bf2,es) <- pExpTs env ts es
                                     return (bf1 . bf2, e:es)


-- Prepare an expression in an arbitrary position and compute its type
pExp env (EVar x)                   = return (id, findValT (tenv env) x, EVar x)
pExp env (ELit l)                   = return (id, litType l, ELit l)
pExp env (EThis)                    = return (id, findValT (tenv env) x, EVar x)
  where x                           = fromJust (this env)
pExp env (ESel e l)                 = do (bf,t1,e) <- pExp env e
                                         let (s,te) = findStructTEnv env t1
                                             t = findValT te l
                                         specialize s t bf (ESel (castIfBigTuple t1 e) l)
pExp env (ECall f ts es)            = do (bf,es) <- pExpTs env ts0 es
                                         specialize s t bf (ECall f [] (polyTagArgs env ts ++ es))
  where (s,ts0,t)                   = findFunT (tenv env) f ts
pExp env (EEnter (EVar x) f ts es)  = do let t1 = findValT (tenv env) x
                                         let (s,te) = findStructTEnv env t1
                                             (s',ts0,t) = findFunT te f ts
                                         (bf,es) <- pExpTs env ts0 es
                                         specialize (s'@@s) t bf (EEnter (castIfClos t1 x) f [] (polyTagArgs env ts ++ es))
pExp env (EEnter e f ts es)         = do (bf1,t1,e) <- pRhsExp env e
                                         let (s,te) = findStructTEnv env t1
                                             (s',ts0,t) = findFunT te f ts
                                         (bf2,es) <- pExpTs env ts0 es
                                         x <- newName tempSym
                                         specialize (s'@@s) t (bf1 . bf2 . cBind [(x, Val (pAType t1) e)]) 
                                                    (EEnter (castIfClos t1 x) f [] (polyTagArgs env ts ++ es))
pExp env (ECast t e)                = do (bf,t',e) <- pExp env e
                                         return (bf, t, cast t t' e)
pExp env (ENew n ts bs)
  | n == tuple 0                    = return (id, tWORD, ELit (lInt 0))
  | n `elem` nulls env              = return (id, tWORD, ELit (conLit env n))
  | otherwise                       = do (bf,t,e) <- pNewExp env n ts bs
                                         x <- newName tempSym
                                         return (bf . cBind [(x, Val (pAType t) e)], t, EVar x)

specialize s t bf e                 = return (bf, t', cast t' t e)
  where t'                          = subst s t

castIfBigTuple t@(TCon (Tuple n _) _) e
  | n > 4                           = ECast (pAType t) e
castIfBigTuple _ e                  = e

castIfClos (TCon (Prim CLOS _) ts0) x
                                    = ECast (TCon (prim CLOS) (polyTagTypes (length vs) ++ map pAType (t:ts))) (EVar x)
  where (vs,ts,t)                   = decodeCLOS ts0
castIfClos t x                      = EVar x
