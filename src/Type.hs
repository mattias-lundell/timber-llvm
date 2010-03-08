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

module Type(typecheck) where
      
import Common
import Core
import Env
import Depend
import List(unzip4, zipWith3)
import Monad
import Kind
import Decls
import Reduce
import Termred
import PP
import Derive

--typecheck                       :: Module -> M s Module
typecheck e2 m                     = tiModule e2 m


{-

Example 1:
==========

-- Initially:
let f :: P => S -> T
    f = \x -> e (f 7)

-- After exp inference:
    \x -> e w (f v 7)                                ::X->T'   \\ w::W, v::V

-- After generalization:
    \w v x -> e w (f v 7)                            ::W->V->X->T'  \\

-- After mu:
let f = c (\w v x -> e w (f v 7))                  ::P->S->T   \\ c::(W->V->X->T')->(P->S->T)

-- After improvement (T' = T, X = S, V = P):
let f = c (\w v x -> e w (f v 7))                  ::P->S->T   \\ c::(W->P->S->T)->(P->S->T)

-- After reduce:
let f = (\c -> c (\w v x -> e w (f v 7))) (\i -> i w0)             ::P->S->T  \\ w0 :: W

-- After generalization:
let f = \w0 -> (\f -> ((\c -> c (\w v x -> e w (f v 7))) (\i -> i w0))) (f w0)   ::W->P->S->T  \\

-- After inlining:
let f = \w0 -> \f -> ((\c -> c (\w v x -> e w (f v 7))) (\i -> i w0)) (f w0)  ::W->P->S->T  \\
let f = \w0 -> (\c -> c (\w v x -> e w (f w0 v 7))) (\i -> i w0)              ::W->P->S->T  \\
let f = \w0 -> (\i -> i w0) (\w v x -> e w (f w0 v 7))                        ::W->P->S->T  \\
let f = \w0 -> (\w v x -> e w (f w0 v 7)) w0                                  ::W->P->S->T  \\
let f = \w0 v x -> e w0 (f w0 v 7)                                            ::W->P->S->T  \\

---------------
\i -> i w0 :: (W->P->S->T)->P->S->T                \\ w0 :: W
---------------
-}

tiModule (Module _ _ xs' es' ds' ws' [bs']) (Module v ns xs es ds ws bss)
                                = do env0' <- impPreds env0 pe'
                                     (env1,ds1,bs1) <- typeDecls env0' ps ds
                                     let env1' = addTEnv0 (tenvSelsCons ds') env1
                                     (env2,bs2) <- instancePreds env1' pe
                                     -- Here it should be checked that any coercions in weqs follow the
                                     -- restricted rules for coercions, and that the equalities collected 
                                     -- in env2 are actually met by the equations in bs1, bs2 and bss
                                     let env3 = insertDefaults (addTEnv0 (extsMap (es' ++ es)) env2) (pe'++pe) (xs'++xs)
                                         env4 = addCoercions (weqs1 ++ weqs) env3
                                         weqs1 = eqnsOf bs1 ++ eqnsOf bs2
                                     (ss0,pe0,bs0) <- tiBindsList env4 bss
                                     -- tr ("Top-level: \n" ++ render (nest 4 (vpr (tsigsOf bs0) $$ vpr pe0)))
                                     bs3 <- topresolve env4 ss0 pe0 bs0
                                     -- tr ("Top-level after resolve: \n" ++ render (nest 4 (vpr (tsigsOf bs3))))
                                     return (Module v ns xs es ds1 (dom weqs1 ++ ws) (groupBinds (concatBinds [bs1,bs2,bs3])))
    where bs                    = concatBinds bss
          weqs                  = restrict (eqnsOf bs') ws' ++ restrict (eqnsOf bs) ws
          pe                    = restrict (tsigsOf bs) ws
          pe'                   = restrict (tsigsOf bs') ws'
          env0                  = addTEnv0 (tsigsOf bs') (impDecls (initEnv v) ds')
          ps                    = filter (isGenerated . fst) (tsigsOf bs')

tiBindsList env []              = return ([], [], nullBinds)
tiBindsList env (bs:bss)        = do (ss1, pe1, bs1) <- tiBinds env bs
                                     (ss2, pe2, bs2) <- tiBindsList (addTEnv (tsigsOf bs1) env) bss
                                     return (ss1++ss2, pe1++pe2, bs1 `catBinds` bs2)

                                     
tiBinds env (Binds _ [] [])     = return ([], [], nullBinds)
tiBinds env (Binds rec te eqs)  = do -- tr ("TYPE-CHECKING " ++ showids xs ++ ", at line: " ++ show (pos (fst(head te))))
                                     -- tr ("rec: " ++ show rec ++ ", mono: " ++ show mono)
                                     -- tr (render (nest 8 (vpr te)))
                                     -- tr ("assuming\n" ++ render (nest 4 (vpr (typeEnv env))))
                                     (s,pe,es1)   <- tiRhs0 (addTEnv te env) explWits ts es
                                     -- tr ("RESULT (" ++ showids xs ++ "):\n" ++ render (nest 8 (vpr pe)))
                                     -- tr ("EXPS:\n" ++ render (nest 8 (vpr es1)))
                                     (s',qe,f) <- fullreduce (target te (setErrPos (posInfo es) env)) s pe `handle` (fail . tail)
                                     -- tr ("PREDICATES OBTAINED (" ++ showids xs ++ "):\n" ++ render (nest 8 (vpr qe)))
                                     let env1      = subst s' env
                                         ts1       = subst s' ts
                                         tvs0      = concat [ tvars t | (t,e) <- ts1 `zip` es, monoRestrict rec t e ]
                                         (qe1,qe2) = if mono then (qe,[]) else partition (isFixed (tevars env1 ++ tvs0)) qe
                                         (vs,qs)   = unzip qe2
                                         es2       = map f es1
                                         es3       = if mono || not rec then es2 else map (subst (satSubst vs)) es2
                                         (es',ts') = if mono then (es3,ts1) else unzip (zipWith (qual qe2) es3 ts1)
                                     -- tr ("BEFORE GEN:\n" ++ render (nest 8 (vpr (xs `zip` ts') $$ vpr qe2)))
                                     -- Note: using genL below instead of mapM gen allows us to take a simplifying 
                                     -- shortcut later on in Type2
                                     ts'' <- genL (tevars env1 ++ tvs0 ++ tvars qe1) ts'
                                     -- tr ("DONE\n" ++ render (nest 8 (vpr (xs `zip` ts''))))
                                     -- tr ("EXPS " ++ render (nest 8 (vpr (xs `zip` es'))))
                                     return (mkEqns env s', qe1, Binds rec (xs `zip` ts'') (xs `zip` es'))
  where ts                      = map (lookup' te) xs
        (xs,es)                 = unzip eqs
        explWits                = map (explicit . annot) xs
        satSubst []             = []
        satSubst vs             = zipWith f xs ts
          where es              = map EVar vs
                f x t           = (x, eLam te (EAp (EVar x) (es ++ map EVar (dom te))))
                  where te      = abcSupply `zip` ctxt t
        mono                    = or [ monoRestrict True t e | (e,t) <- es `zip` ts ]


tiRhs0 env explWits ts es       = do (ss,pes,es') <- fmap unzip3 (mapM (tiExpT' env) (zip3 explWits ts es))
                                     return (concat ss, concat pes, es')

tiRhs env ts es                 = tiRhs0 env (repeat False) ts es


tiExpT env t e                  = tiExpT' env (False, t, e)


tiExpT' env (explWit, sc@(Scheme t0 ps ke), e)
  | monoRestrict False sc e     = do (ss,pe,t,e') <- tiExp env e
                                     c            <- newNamePos coercionSym e'
                                     return (ss, (c, Scheme (F [scheme' t] t0) ps ke) : pe, EAp (EVar c) [e'])
  | null ke && not explWit      = do (ss,qe,t,e)  <- tiExp env e
                                     c            <- newNamePos coercionSym e
                                     pe0          <- newEnvPos assumptionSym ps e
                                     let ws        = map EVar (dom pe0)
                                         e1        = eLam pe0 (EAp (eAp (EVar c) ws) [e])
                                     return (ss, (c, Scheme (F [scheme' t] t0) ps ke) : qe, e1)
  | otherwise                   = do (ss,qe,t,e)  <- tiExp env e
                                     -- tr ("REQUIRED: " ++ render (pr (Scheme t0 ps ke)))
                                     -- tr ("INFERRED: " ++ render (pr t) ++ "\n" ++ render (nest 4 (vpr qe)))
                                     (s,qe,f)     <- normalize (target t (setErrPos (posInfo e) env)) ss qe `handle` (fail . tail)
                                     c            <- newNamePos coercionSym e
                                     pe0          <- newEnvPos assumptionSym ps e >>= wildify ke
                                     let env1      = subst s env
                                         (qe1,qe2) = partition (isFixed (tevars env1)) (subst s qe)
                                         (e',t')   = qual qe2 (f e) (scheme' (subst s t))
                                         ws        = map EVar (dom pe0)
                                         (ws',ps') = if explWit then (ws, ps) else ([], [])
                                         e1        = eLam pe0 (eAp (EAp (eAp (EVar c) ws) [e']) ws')
                                     sc           <- gen (tevars env1 ++ tvars qe1) t'
                                     return (mkEqns env s, (c, Scheme (F [sc] (tFun ps' t0)) ps ke) : qe1, e1)

mkEqns env s                    = mapFst Tvar (restrict s (tevars env))

isFixed tvs (w,p)               = isDummy w || all (`elem` tvs) (tvars p)


tiAp env s pe (F scs rho) e es
  | len_scs >= len_es           = do (s',pe',es') <- tiRhs env scs1 es
                                     te <- newEnv paramSym scs2
                                     return (s++s', pe++pe', tFun scs2 rho, eLam te (EAp e (es'++map EVar (dom te))))
  | otherwise                   = do (s',pe',es') <- tiRhs env scs es1
                                     tiAp env (s++s') (pe++pe') rho (EAp e es') es2
  where len_scs                 = length scs
        len_es                  = length es
        (scs1,scs2)             = splitAt len_es scs
        (es1,es2)               = splitAt len_scs es
tiAp env s pe (R (TFun ts t)) e es
                                = tiAp env s pe (F (map scheme ts) (R t)) e es
tiAp env s pe rho e es          = do t <- newTvar Star
                                     c <- newNamePos coercionSym e
                                     (ss,pes,ts,es) <- fmap unzip4 (mapM (tiExp env) es)
                                     let p = Scheme (F [scheme' rho] (F (map scheme' ts) (R t))) [] []
                                     return (s++concat ss, (c,p):pe++concat pes, R t, EAp (EAp (EVar c) [e]) es)

etaExpand pe (R (TFun ts t)) e  = etaExpand pe (F (map scheme ts) (R t)) e
etaExpand pe (F scs t) e        = do te <- newEnv etaExpSym scs
                                     return ([], pe, F scs t, ELam te (EAp e (map EVar (dom te))))
etaExpand pe t e                = return ([], pe, t, e)


tiExp env (ELit l)              = return ([], [], R (litType l), ELit l)
tiExp env (EVar x)
  | doEtaExpand x               = do (pe,t,e) <- instantiate (findExplType env x) (EVar (annotExplicit x))
                                     etaExpand pe t e
  | otherwise                   = do (pe,t,e) <- instantiate (findExplType env x) (EVar (annotExplicit x))
                                     return ([], pe, t, e)
tiExp env (ECon k)              = do (pe,t,e) <- instantiate (findExplType env k) (ECon (annotExplicit k))
                                     etaExpand pe t e
tiExp env (ESel e l)            = do (t, t0:ps) <- inst (findType env l)
                                     (s,pe,e) <- tiExpT env t0 e
                                     let r = if explicit (annot l) then (tFun ps t,[]) else (t,ps)
                                     (pe',t',e') <- saturate r (ESel e (annotExplicit l))
                                     return (s, pe++pe', t', e')
tiExp env (ELam te e)           = do (s,pe,t,e) <- tiExp (addTEnv te env) e
                                     return (s, pe, F (rng te) t, ELam te e)
tiExp env (EAp e es)            = do (s,pe,t,e) <- tiExp env e
                                     tiAp env s pe t e es
tiExp env (ELet bs e)           = do (s,pe,bs) <- tiBinds env bs
                                     (s',pe',t,e) <- tiExp (addTEnv (tsigsOf bs) env) e
                                     return (s++s',pe++pe', t, ELet bs e)
tiExp env (ERec c eqs)          = do alphas <- mapM newTvar (kArgs (findKind env c))
                                     (t,ts,_) <- tiLhs env (foldl TAp (TId c) alphas) tiSel sels
                                     (s,pe,es') <- tiRhs env ts es
                                     -- tr ("RECORD " ++ render (pr t))
                                     -- tr (render (nest 4 (vpr (sels `zip` ts))))
                                     -- tr ("    pe :\n" ++ render (nest 4 (vpr pe)))
                                     e <- mkRecTerm env c sels es'
                                     return (s, pe, R t, e)
  where (sels,es)               = unzip eqs
        tiSel env x l           = tiExp env (ESel (EVar x) l)
tiExp env (ECase e alts)        = do alpha <- newTvar Star
                                     (t,ts,_) <- tiLhs env alpha tiPat pats
                                     (s,pe,es')   <- tiRhs env ts es
                                     let TFun [t0] t1 = t
                                     (s1,pe1,e')  <- tiExpT env (scheme t0) e
                                     e <- mkCaseTerm env e' (tId (tHead t0)) pats es'
                                     return (s++s1, pe++pe1, R t1, e)
  where (pats,es)               = unzipAlts (map argsToRhs alts)
        tiPat env x (PLit l)    = tiExp env (EAp (EVar x) [ELit l])
        tiPat env x (PCon k []) = do (t,_) <- inst (findType env k)
                                     te <- newEnv paramSym (funArgs t)
                                     tiExp env (eLam te (EAp (EVar x) [eAp (ECon k) (map EVar (dom te))]))
        tiPat env x (PWild)     = do y <- newName tempSym
                                     t <- newTvar Star
                                     tiExp (addTEnv [(y,scheme t)] env) (EAp (EVar x) [EVar y])
tiExp env (EReq e e')           = do alpha <- newTvar Star
                                     beta <- newTvar Star
                                     (s,pe,e) <- tiExpT env (scheme (tRef alpha)) e
                                     (s',pe',e') <- tiExpT env (scheme (tCmd alpha beta)) e'
                                     return (s++s', pe++pe', R (tRequest beta), EReq e e')
tiExp env (EAct e e')           = do alpha <- newTvar Star
                                     beta <- newTvar Star
                                     (s,pe,e) <- tiExpT env (scheme (tRef alpha)) e
                                     (s',pe',e') <- tiExpT env (scheme (tCmd alpha beta)) e'
                                     return (s++s', pe++pe', R tAction, EAct e e')
tiExp env (EDo x tx c)
  | isTvar tx                   = do (s,pe,t,c) <- tiCmd (setSelf x tx env) c
                                     let s' = case stateT env of Nothing -> []; Just t' -> [(t',tx)]
                                     return (s'++s, pe, R (tCmd tx t), EDo x tx c)
  | otherwise                   = internalError0 "Explicitly typed do expressions not yet implemented"
tiExp env (ETempl x tx te c)
  | isTvar tx                   = do n <- newName stateTypeSym
                                     let env' = setSelf x (TId n) (addTEnv te (addKEnv0 [(n,Star)] env))
                                     (s,pe,t,c) <- tiCmd env' c
                                     return ((TId n, tx):s, pe, R (tClass t), ETempl x (TId n) te c)
  | otherwise                   = internalError0 "Explicitly typed class expressions not yet implemented"


tiCmd env (CRet e)              = do alpha <- newTvar Star
                                     (s,pe,e) <- tiExpT env (scheme alpha) e
                                     return (s, pe, alpha, CRet e)
tiCmd env (CExp e)              = do alpha <- newTvar Star
                                     (s,pe,e) <- tiExpT env (scheme (tCmd (fromJust (stateT env)) alpha)) e
                                     return (s, pe, alpha, CExp e)
tiCmd env (CGen x tx e c)       = do (s,pe,e) <- tiExpT env (scheme (tCmd (fromJust (stateT env)) tx)) e
                                     (s',pe',t,c) <- tiCmd (addTEnv [(x,scheme tx)] env) c
                                     return (s++s', pe++pe', t, CGen x tx e c)
tiCmd env (CAss x e c)          = do (s,pe,e) <- tiExpT env (findType env x) e
                                     (s',pe',t,c) <- tiCmd env c
                                     return (s++s', pe++pe', t, CAss x e c)
tiCmd env (CLet bs c)           = do (s,pe,bs) <- tiBinds env bs
                                     (s',pe',t,c) <- tiCmd (addTEnv (tsigsOf bs) env) c
                                     return (s++s', pe++pe', t, CLet bs c)


-- Compute a list of
tiLhs env alpha tiX xs          = do x <- newName tempSym
                                     let env' = addTEnv [(x,scheme alpha)] env
                                     (_,pes,ts,es) <- fmap unzip4 (mapM (tiX env' x) xs)    -- ignore returned substs (must be null)
                                     (s,_,f) <- resolve (target alpha env) (concat pes)     -- ignore returned qe (filter pes instead)
                                     let ts1 = map scheme' (subst s ts)
                                         es1 = map f es
                                         pes1 = subst s (map (filter (not . isCoercion . fst)) pes) -- preserve non-coercions for each alt
                                         (es2,ts2) = unzip (zipWith3 qual pes1 es1 ts1)
                                     ts3 <- genL (tevars (subst s env')) ts2
                                     es2 <- mapM (redTerm (coercions env)) es2
                                     return (subst s alpha, ts3, es2)


-- Build a record term ---------------------------------------------------------------------------------------------------
mkRecTerm env c sels es         = return (mkOne c)
  where cs                      = map ltype sels
        groups                  = map (\c -> (c, [ (l,e) | (c',l,e) <- zip3 cs sels es, c == c' ])) (nub cs)
        graph                   = nodesFrom c
        nodesFrom c             = (c,ns) : concatMap nodesFrom (rng ns)
          where ns              = [ (sel w, snd (subsyms p)) | (w,p) <- nodes wg, w `notElem` indirect ]
                indirect        = map snd (arcs wg)
                sel w           = ff (lookup' (coercions env) w)
                wg              = findAbove env c
        ff (ELam _ (ESel _ l))  = l
        ltype l                 = headsym (head (ctxt (findType env l)))
        mkOne c                 = ERec c (eqs0 ++ case lookup c groups of Just eqs -> eqs; Nothing -> [])
          where eqs0            = mapSnd mkOne (lookup' graph c)


-- Build a case term -------------------------------------------------------------------------------------------------------
mkCaseTerm env e0 c pats0 es0
  | any isLitPat pats0          = return (ECase e0 (zipAlts pats0 es0))
  | otherwise                   = mkOne e0 c
  where (altsK,altsW)           = partition (isConPat . altPat) (zipAlts pats0 es0)
        (pats,es)               = unzipAlts altsK
        cs                      = map ptype pats
        groups                  = map (\c -> (c, [ Alt p e | (c',p,e) <- zip3 cs pats es, c == c' ])) (nub cs)
        graph                   = nodesFrom c
        nodesFrom c             = (c,ns) : concatMap nodesFrom (rng ns)
          where ns              = [ (con w, fst (subsyms p)) | (w,p) <- nodes wg, w `notElem` indirect ]
                indirect        = map snd (arcs wg)
                con w           = ff (lookup' (coercions env) w)
                wg              = findBelow env c
        ff (ELam _ (EAp (ECon k) _)) = k
        ptype (PCon k _)        = tId (tHead (body (findType env k)))
        mkOne e0 c              = do alts0 <- mapM mkAlt0 (lookup' graph c)
                                     return (ECase e0 (alts0 ++ (case lookup c groups of Just alts -> alts; Nothing -> []) ++ altsW))
        mkAlt0 (k,c)            = do x <- newName tempSym
                                     (F [sc] _, _) <- inst (findType env k)
                                     e <- mkOne (EVar x) c
                                     return (Alt (pCon0 k) (ELam [(x,sc)] e))

        

-- data Pack m a = Pack (m a) \\ Eq a
-- data Exists m > Pack m a \\ a
--               = Test (m Int)

-- Translation 1:
-- data Pack m a = Pack (m a) \\ Eq a
-- data Exists m = Exists_from_Pack (Pack m a) \\ a
--               | Test (m Int)

-- Translation 2:
-- data Pack m a = Pack (Eq a) (m a)
-- data Exists m = Exists_from_Pack (Pack m a) \\ a
--               | Test (m Int)

-- Constructor types:
-- Pack             :: Eq a -> m a -> Pack m a \\ a, m
-- Exists_from_Pack :: Pack (m a) -> Exists m \\ a, m
-- Test             :: m Int -> Exists m \\ m

-- case e0 of
--   Pack -> (\w r -> e1)
--   Test -> (\q -> e2

-- case e0 of
--   Exixts_from_Pack -> (\x -> case x of
--                                Pack -> (\w r -> e1)
--                                _    -> fail
--   Test             -> (\q -> e2)


-- Formulation without subtyping: ------------------------------
--
-- data Exists m = Pack (m a) \\ Eq a, a
--               | Test (m Int)

-- Translation:
-- data Exists m = Pack (Eq a) (m a) \\ a
--               | Test (m Int)

-- Constructor types:
-- Pack  :: Eq a -> m a -> Exists m \\ a, m
-- Test  :: m Int -> Exists m \\ m

-- case e0 of
--   Pack -> (\(w :: Eq _) (r :: _ _) -> e1)
--   Test -> (\(q :: _ Int) -> e2)





{-
   
    record A v
       a :: Eq v => v -> [v]            w : Eq v |- (.a) : A v -> v -> [v]  =  \t -> t.a w

    record B v < A v =
       b :: v                                    |- (.b) : B v -> v         =  \t -> t.b


    { a v = [v], b = 0 }

    w : Eq v, ca : xx < A v |- (.a) x  :  v -> [v]  =  (\t -> t.a w) (ca x)  

              cb : xx < B v |- (.b) x  :  v         =  (\t -> t.b)   (cb x)

    b2a/ca, id/cb, B v / xx

    w : Eq v |- (.a) x  :  v -> [v]  =  (\t -> t.a w) ((.b2a) x)   =  x.b2a.a w   =  (.a) . (.b2a) .

             |- (.b) x  :  v         =  (t -> t.b)    (id x)       =  x.b


-}

{-

record A =
    a :: Ta

record B < A =
    b :: Tb

record C < A =
    c :: Tc

record D < B,C =
    d :: Td

===>

record A =
    a :: Ta

record B =
    b2a :: A
    b   :: Tb

record C =
    c2A :: A
    c   :: Tc

record D =
    d2b :: B
    d2c :: C
    d   :: Td


D < B < A
D < C < A

{ a|b2a|d2b = Ma
  a|c2a|d2c = Ma
  b|d2b     = Mb         :: D
  c|d2c     = Mc
  d         = Md
}

{ d2b = { b2a = { a = Ma }
          b   = Mb }
  d2c = { c2a = { a = Ma }
          c   = Mc }
  d   = Md }

        A
      /   \
     B     C
      \   /
        D

{ B2A -> { D2B -> { D -> \x.Md }
           B   -> \x.Mb }
  C2A -> { D2C -> { D -> \x.Md }
           C   -> \x.Mc }
  A   -> \x.Ma }

{ B2A|D2B|D -> \x.Md
  C2A|D2C|D -> \x.Md
  B2A|B     -> \x.Mb       :: A -> t
  C2A|C     -> \x.Mc
  A         -> \x.Ma
}

A->t < B->t < D->t
A->t < C->t < D->t

data D =
    D Td

data C > D =
    C Tc

data B > D =
    B Rb

data A > B,C =
    a Ta

===>

data D =
    D Td

data C =
    D2C D
    C Tc

data B =
    D2B D
    B Tb

data A =
    B2A B
    C2A C
    A Ta

-}
