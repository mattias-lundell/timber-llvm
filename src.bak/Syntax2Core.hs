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

module Syntax2Core where


import Common
import Syntax
import Monad
import qualified Core
import PP

syntax2core m = s2c m


-- translate a module in the empty environment

s2c                             :: Module -> M s Core.Module
s2c (Module v is ds ps)         = do (xs,es,ts,ws,bss) <- s2cDecls env0 ds [] [] [] [] [] []
                                     return (Core.Module v is' xs es ts ws bss)
  where env0                    = Env { sigs = [] }
        is'                     = [(b,n) | Import b n <- is]


-- type signature environments
data Env                        = Env { sigs :: Map Name Type }

addSigs te env                  = env { sigs = te ++ sigs env }


-- Syntax to Core translation of declarations ========================================================

-- Translate top-level declarations, accumulating signature environment env, kind environment ke, 
-- type declarations ts, instance names ws, bindings bs as well as default declarations xs
s2cDecls env [] ke ts ws bss xs es 
                                = do bss <- s2cBindsList env (reverse bss)
                                     ke' <- mapM s2cKSig (impl_ke `zip` repeat KWild)
                                     xs' <- mapM s2cDefault xs
                                     es' <- mapM s2cExtern es
                                     let ds = Core.Types (reverse ke ++ ke') (reverse ts)
                                     return (xs', es', ds, ws, bss)
  where impl_ke                 = dom ts \\ dom ke

s2cDecls env (DKSig c k : ds) ke ts ws bss xs es
                                = do ck  <- s2cKSig (c,k)
                                     s2cDecls env ds (ck:ke) ts ws bss xs es
s2cDecls env (DData c vs bts cs : ds) ke ts ws bss xs es
                                = do bts <- mapM s2cQualType bts
                                     cs  <- mapM s2cConstr cs
                                     s2cDecls env' ds ke ((c,Core.DData vs bts cs):ts) ws bss xs es
  where env'                    = addSigs (teConstrs c vs cs) env
s2cDecls env (DRec isC c vs bts ss : ds) ke ts ws bss xs es
                                = do bts <- mapM s2cQualType bts
                                     sss <- mapM s2cSig ss
                                     s2cDecls env' ds ke ((c,Core.DRec isC vs bts (concat sss)):ts) ws bss xs es
  where env'                    = addSigs (teSigs c vs ss) env
s2cDecls env (DType c vs t : ds) ke ts ws bss xs es
                                = do t <- s2cType t
                                     s2cDecls env ds ke ((c,Core.DType vs t):ts) ws bss xs es
s2cDecls env (DInstance vs : ds) ke ts ws bss xs es
                                = s2cDecls env ds ke ts (ws++vs) bss xs es
s2cDecls env (DDefault d : ds) ke ts ws bss xs es
                                = s2cDecls env ds ke ts ws bss (d ++ xs) es
s2cDecls env (DExtern es' : ds) ke ts ws bss xs es
                                = s2cDecls env ds ke ts ws bss xs (es ++ es')
s2cDecls env (DBind bs : ds) ke ts ws bss xs es
                                = s2cDecls env ds ke ts ws (bs:bss) xs es


s2cBindsList env []             = return []
s2cBindsList env (bs:bss)       = do (te,bs) <- s2cBinds env bs
                                     bss <- s2cBindsList (addSigs te env) bss
                                     return (bs:bss)


s2cDefault (Default t a b)      = return (Default t a b)
s2cDefault (Derive n t)         = do t <- s2cQualType t
                                     return (Derive n t)

s2cExtern (Extern n t)         = do t <- s2cQualType t
                                    return (Extern n t)


--translate a constructor declaration
s2cConstr (Constr c ts ps)      = do ts <- mapM s2cQualType ts
                                     (qs,ke) <- s2cQuals ps
                                     return (c, Core.Constr ts qs ke)


-- translate a field declaration
s2cSig (Sig vs qt)              = s2cTSig vs qt


-- add suppressed wildcard kind  
pVar v                          = PKind v KWild


-- buld a signature environment for data constructors cs in type declaration tc vs
teConstrs tc vs cs              = map f cs
  where t0                      = foldl TAp (TCon tc) (map TVar vs)
        f (Constr c ts ps)      = (c,TQual (tFun ts t0) (ps ++ map pVar vs))

-- build a signature environment for record selectors ss in type declaration tc vs
teSigs tc vs ss                 = concat (map f ss)
  where t0                      = foldl TAp (TCon tc) (map TVar vs)
        f (Sig ws (TQual t ps)) = ws `zip` repeat (TQual (TFun [t0] t) (ps ++ map pVar vs))
        f (Sig ws t)            = ws `zip` repeat (TQual (TFun [t0] t) (map pVar vs))


-- Signatures =========================================================================

-- translate a type signature
s2cTSig vs t                    = do ts <- mapM (s2cQualType . const t) vs
                                     return (vs `zip` ts)


-- Types ==============================================================================

-- translate a qualified type scheme
s2cQualType (TQual t qs)        = do (ps,ke) <- s2cQuals qs
                                     t <- s2cRhoType t
                                     return (Core.Scheme t ps ke)
s2cQualType t                   = s2cQualType (TQual t [])


-- translate a rank-N function type
s2cRhoType (TFun ts t)          = do ts <- mapM s2cQualType ts
                                     t <- s2cRhoType t
                                     return (Core.F ts t)
s2cRhoType t                    = liftM Core.R (s2cType t)


-- translate a monomorphic type
s2cType (TSub t t')             = s2cType (TFun [t] t')
s2cType (TFun ts t)             = do ts <- mapM s2cType ts
                                     t <- s2cType t
                                     return (Core.TFun ts t)
s2cType (TAp t t')              = liftM2 Core.TAp (s2cType t) (s2cType t')


s2cType (TCon c)                = return (Core.TId c)
s2cType (TVar v)                = return (Core.TId v)
s2cType (TWild)                 = do k <- newKVar
                                     Core.newTvar k


-- translate qualifiers, separating predicates from kind signatures along the way
s2cQuals qs                     = s2c [] [] qs
  where
    s2c ps ke []                = return (reverse ps, reverse ke)
    s2c ps ke (PKind v k : qs)  = do k <- s2cKind k
                                     s2c ps ((v,k) : ke) qs
    s2c ps ke (PType t : qs)    = do p <- s2cQualType t
                                     s2c (p : ps) ke qs


-- Kinds ==============================================================================

-- translate a kind
s2cKind Star                    = return Star
s2cKind KWild                   = newKVar
s2cKind (KFun k k')             = liftM2 KFun (s2cKind k) (s2cKind k')

s2cKSig (v,k)                   = do k <- s2cKind k
                                     return (v,k)

-- Expressions and bindings ============================================================

-- the translated default case alternative
dflt                                    = [Core.Alt Core.PWild (Core.EVar (prim Fail))]


-- translate a case alternative, inheriting type signature t top-down
s2cAE env t alt                         = s2cA s2cEc env t alt
s2cAS env alt                           = s2cA s2cSc env TWild alt

s2cA s2cR env t (Alt p (RExp r))        = s2cA2 s2cR env t (p,r)

s2cA2 s2cR env t (p,r)                  = s2cA' (pFlat p) r
  where
    s2cA' (PLit l,[]) e                 = do e' <- s2cR env t e
                                             return (Core.Alt (Core.PLit l)  e')
    s2cA' (PCon c,ps) e                 = do e' <- s2cR (addSigs te env) t e
                                             te' <- s2cTE te
                                           --return (Alt (Core.PCon c []) (Core.eLam te' e')) -- old
                                             return (Core.Alt (Core.PCon c te') e') -- new
      where (te,t')                     = mergeT ps (lookupT c env)


-- translate a record field
s2cF env (Field s e)                    = do e <- s2cEc env (snd (splitT (lookupT s env))) e
                                             return (s,e)


-- split bindings bs into type signatures and equations
splitBinds bs                           = s2cB [] [] bs
  where 
    s2cB sigs eqs []                    = (reverse sigs, reverse eqs)
    s2cB sigs eqs (BSig vs t : bs)      = s2cB (vs `zip` repeat t ++ sigs) eqs bs
    s2cB sigs eqs (BEqn (LFun v []) (RExp e) : bs)
                                        = s2cB sigs ((v,e):eqs) bs


-- translate equation eqs in the scope of corresponding signatures sigs
s2cBinds env bs                         = do (ts,es) <- fmap unzip (mapM s2cEqn eqs)
                                             let te = vs `zip` ts
                                             te' <- s2cTE te
                                             return (te, Core.Binds isRec te' (vs `zip` es))
  where (sigs,eqs)                      = splitBinds bs
        vs                              = dom eqs
        isRec                           = not (null (filter (not . isPatTemp) vs `intersect` evars (rng eqs)))
        env'                            = addSigs sigs env
        s2cEqn (v,e)                    = case lookup v sigs of
                                            Nothing -> s2cEi env' e
                                            Just t  -> do e <- s2cEc env' t' e
                                                          return (t,e)
                                              where t' = if explicit (annot v) then expl t else peel t


-- Expressions, inherit mode ===============================================================

-- translate an expression, inheriting type signature t top-down
s2cEc env t (ELam ps e)         = do e' <- s2cEc (addSigs te env) t' e
                                     te' <- s2cTE te
                                     return (Core.ELam te' e')
  where (te,t')                 = mergeT ps t
s2cEc env _ (EAp e1 e2)         = do (t,e1) <- s2cEi env e1
                                     let (t1,_) = splitT t
                                     e2 <- s2cEc env (peel t1) e2
                                     return (Core.eAp2 e1 [e2])
s2cEc env t (ELet bs e)         = do (te',bs') <- s2cBinds env bs
                                     e' <- s2cEc (addSigs te' env) t e
                                     return (Core.ELet bs' e')
s2cEc env t (ECase e alts)      = do e <- s2cEc env TWild e
                                     alts <- mapM (s2cAE env t) alts
                                     return (Core.ECase e (alts++dflt))
s2cEc env t (EMatch m)          = do m <- s2cMc env t m
                                     return (eMatch m)
s2cEc env t (ESelect e s)       = do e <- s2cEc env (peel t1) e
                                     return (Core.ESel e s)
  where (t1,_)                  = splitT (lookupT s env)
s2cEc env t (ECon c)            = return (Core.ECon c)
s2cEc env t (EVar v)            = return (Core.EVar v)
s2cEc env t (ELit l)            = return (Core.ELit l)
s2cEc env _ e                   = s2cE env e


-- Expressions, agnostic mode ================================================================

-- translate an expression whose type cannot be rank-N polymorphic 
-- (i.e., no point inheriting nor synthesizing signatures)
s2cE env (ERec (Just (c,_)) eqs)        = do eqs <- mapM (s2cF env) eqs
                                             return (Core.ERec c eqs)
s2cE env (EAct (Just x) (Stmts[SExp e]))= do (_,e) <- s2cEi env e
                                             return (Core.EAct (Core.EVar x) e)
s2cE env (EReq (Just x) (Stmts[SExp e]))= do (_,e) <- s2cEi env e
                                             return (Core.EReq (Core.EVar x) e)
s2cE env (EDo (Just x) Nothing ss)      = do c <- s2cS env ss
                                             t <- s2cType TWild
                                             return (Core.EDo x t c)
s2cE env (ETempl (Just x) Nothing ss)   = do c <- s2cS (addSigs te env) ss
                                             t <- s2cType TWild
                                             te' <- s2cTE te
                                             return (Core.ETempl x t te' c)
  where 
    vs                                  = assignedVars ss
    te                                  = sigs' ss

    sigs' (Stmts ss)                    = sigs ss
    sigs []                             = []
    sigs (SAss (PSig (PVar v) t) _ :ss) = (v,t) : sigs ss
    sigs (SAss (PVar v) _:ss)           = (v,TWild) : sigs ss
    sigs (_ : ss)                       = sigs ss

s2cE env e                              = internalError "s2cE: did not expect" e


-- Pattern Matching ============================================================================

--eMatch = Core.EMatch -- new
eMatch = Core.eMatch . desugarMatch  -- compat

desugarMatch = foldMatch Core.eCommit Core.eFail Core.eFatbar eCase Core.ELet
  where
    eCase e = Core.ECase e . map (uncurry Core.Alt)

s2cMc env t m =
  case m of
    MCommit r     -> MCommit `fmap` s2cEc env t r
    MFail         -> return MFail
    MFatbar m1 m2 -> liftM2 MFatbar (s2cMc env t m1) (s2cMc env t m2)
    Case e alts   -> do e <- s2cEc env TWild e
                        alts <- mapM (s2cA2 s2cMc env t) alts
                        return (Case e (uncurry zip (Core.unzipAlts alts)++dflt)) -- dflt??
      where
        dflt = [(Core.PWild, MFail)]

    Let bs m      -> do (te',bs') <- s2cBinds env bs
                        e' <- s2cMc (addSigs te' env) t m
                        return (Let bs' e')

-- Statements ==================================================================================

-- translate a statement list
s2cSc env t ss                           = s2cS env ss

s2cS  env (Stmts ss)                     = s2cSs env ss

s2cSs env []                            = return (Core.CRet (Core.eUnit))
s2cSs env [SRet e]                      = do (t,e') <- s2cEi env e
                                             return (Core.CRet e')
s2cSs env [SExp e]                      = do (t,e') <- s2cEi env e
                                             return (Core.CExp e')
s2cSs env [SCase e alts]                = do e <- s2cEc env TWild e
                                             alts <- mapM (s2cAS env) alts
                                             return (Core.CCase e alts)
s2cSs env (SGen (PSig (PVar v) t) e :ss)= do t' <- s2cType t
                                             e' <- s2cEc env TWild e
                                             c <- s2cSs (addSigs [(v,t)] env) ss
                                             return (Core.CGen v t' e' c)
s2cSs env (SGen (PVar v) e : ss)        = do (_,e') <- s2cEi env e
                                             t <- s2cType TWild
                                             c <- s2cSs env ss
                                             return (Core.CGen v t e' c)
s2cSs env (SAss (PSig v t) e : ss)      = s2cSs env (SAss v e : ss)
s2cSs env (SAss (PVar v) e : ss)        = do e' <- s2cEc env (lookupT v env) e
                                             c <- s2cSs env ss
                                             return (Core.CAss v e' c)
s2cSs env (SBind bs : ss)               = do (te',bs') <- s2cBinds env bs
                                             c <- s2cSs (addSigs te' env) ss
                                             return (Core.CLet bs' c)
s2cSs env ss                            = internalError "s2cSs: did not expect" (Stmts ss)


-- Expressions, synthesize mode ================================================================

-- translate an expression, synthesize type signature bottom-up
s2cEi env (ELam ps e)           = do (t,e') <- s2cEi (addSigs te env) e
                                     te' <- s2cTE te
                                     return (TFun (rng te) t, Core.ELam te' e')
  where (te,_)                  = mergeT ps TWild
s2cEi env (EAp e1 e2)           = do (t,e1) <- s2cEi env e1
                                     let (t1,t2) = splitT t
                                     e2 <- s2cEc env (peel t1) e2
                                     return (t2, Core.eAp2 e1 [e2])
s2cEi env (ELet bs e)           = do (te',bs') <- s2cBinds env bs
                                     (t,e') <- s2cEi (addSigs te' env) e
                                     return (t, Core.ELet bs' e')
s2cEi env (ECase e alts)        = do e <- s2cEc env TWild e
                                     alts <- mapM (s2cAE env TWild) alts
                                     return (TWild, Core.ECase e (alts++dflt))
s2cEi env (EMatch m)            = do m <- s2cMc env TWild m
                                     return (TWild, eMatch m)
s2cEi env (ESelect e s)         = do e <- s2cEc env (peel t1) e
                                     return (t2, Core.ESel e s)
  where (t1,t2)                 = splitT (lookupT s env)
s2cEi env (ECon c)              = return (lookupT c env, Core.ECon c)
s2cEi env (EVar v)              = return (lookupT v env, Core.EVar v)
s2cEi env (ELit l)              = return (TWild, Core.ELit l)
s2cEi env e                     = do e' <- s2cE env e
                                     return (TWild, e')


-- Misc ====================================================================================

-- translate type enviroment te
s2cTE te                        = mapM s2cVT te
  where s2cVT (v,t)             = do t <- s2cQualType t
                                     return (v,t)


-- split a function type into domain (one parameter only) and range
-- if not a function type, fail gracefully (error will be caught later by real type-checker)
splitT (TFun [t] t')            = (t, t')
splitT (TFun (t:ts) t')         = (t, TFun ts t')
splitT TWild                    = (TWild,TWild)
splitT _                        = (TWild,TWild)


-- return the domain of a function type (all immediate parameters)
splitArgs (TFun ts t)           = ts
splitArgs t                     = []


-- merge any signatures in patterns ps with domain of type t, pair with range of t
mergeT ps t                     = (zipWith f ts ps, t')
  where (ts,t')                 = split (length ps) t
        f t (PVar v)            = (v,t)
        f t (PSig (PVar v) t')  = (v,t')
        f t e                   = internalError "mergeT: did not expect" e
        split 0 t               = ([],t)
        split n t               = (t1:ts,t')
          where (t1,t2)         = splitT t
                (ts,t')         = split (n-1) t2


-- find and instantate the type signature for x if present, otherwise return _
lookupT x env                   = case lookup x (sigs env) of
                                    Nothing -> TWild
                                    Just t  -> peel t


-- peel off the outermost qualifiers of a type, replacing all bound variables with _
peel (TQual t ps)               = mkWild (bvars ps) t
peel t                          = t


-- turn class and subtype predicates into explicit function arguments, and peel off the quantifiers
expl (TQual t ps)               = mkWild (bvars ps) (TFun ts t)
  where ts                      = [ t | PType t <- ps ]


-- replace all variables vs in a type by _
mkWild vs (TQual t ps)          = TQual (mkWild vs' t) (map (mkWild' vs') ps)
  where vs'                     = vs \\ bvars ps
mkWild vs (TAp t t')            = TAp (mkWild vs t) (mkWild vs t')
mkWild vs (TFun ts t)           = TFun (map (mkWild vs) ts) (mkWild vs t)
mkWild vs (TSub t t')           = TSub (mkWild vs t) (mkWild vs t')
mkWild vs (TList t)             = TList (mkWild vs t)
mkWild vs (TTup ts)             = TTup (map (mkWild vs) ts)
mkWild vs (TVar v)
          | v `elem` vs         = TWild
mkWild vs t                     = t


-- replace all variables vs in a predicate by _
mkWild' vs (PType t)          = PType (mkWild vs t)
mkWild' vs p                  = p

-- Checking whether bindings are recursive 

checkRecBinds (Core.Binds _ te  es@[(x,e)])
                              = Core.Binds (x `elem` evars e) te es
checkRecBinds (Core.Binds _ te es) 
                              = Core.Binds True te es

