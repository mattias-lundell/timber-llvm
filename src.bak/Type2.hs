{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
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

module Type2 where
    
import Common
import Core
import Env
import Decls
import PP

typecheck2 e2 m                 = t2Module e2 m

t2Module (Module _ _ _ es' ds' ws' [bs']) (Module v ns xs es ds ws bss)
                                = do bss <- t2TopBinds env2 bss
                                     return (Module v ns xs es ds ws bss)
  where env2                    = addTEnv0 te2 (addKEnv0 ke2 env1)
        te2                     = tenvSelsCons ds ++ extsMap (es' ++ es)
        ke2                     = ksigsOf ds
        env1                    = addTEnv0 te1 (addKEnv0 ke1 env0)
        te1                     = tsigsOf bs' ++ tenvSelsCons ds'
        ke1                     = ksigsOf ds'
        env0                    = addTEnv0 primPredEnv (initEnv v)


-- Note 1:  During type-checking of a top-level right-hand side, all unification variables found will 
-- be generalized before a dependent binding group is checked. Thus there is no need to propagate any 
-- substitution from one top-level binding group to the next

t2TopBinds env []               = return []
t2TopBinds env (bs:bss)         = do (_,bs) <- t2Binds env bs
                                     bss <- t2TopBinds (addTEnv (tsigsOf bs) env) bss
                                     return (bs:bss)


-- Note 2:  In order to enable subsequent translation into the System F-like type system of Kindle, 
-- the type abstraction and application points are encoded in the resulting terms as uniquely shaped 
-- let-bindings (see encodeTAbs and encodeTApp defined in Core).

t2Binds env (Binds r te eqs)    = do (s0,eqs') <- t2Eqs eqs
                                     let mono = [ t | (x,e) <- eqs, let t = lookup' te x, monoRestrict r t e ]
                                         tvs0 = tevars (subst s0 env) ++ tvars (subst s0 mono)
                                     quants <- mapM (t2Quant tvs0) (subst s0 te)
                                     let env2 = addTEnv te' env
                                         te'  = [ (x,t) | (x,(t,_,_)) <- quants ]
                                         ss   = [ (x,s) | (x,(_,s,_)) <- quants ]
                                         s1   = [ (x,e) | (x,(_,_,e)) <- quants, e /= EVar x ]
                                     eqs'' <- mapM (t2Gen env2 s0 ss s1) eqs'
                                     return (s0, Binds r te' eqs'')
  where env1                    = addTEnv te env

        t2Eqs []                = return (nullSubst, [])
        t2Eqs ((x,e):eqs)       = do (s1,e) <- t2ExpTscoped env1 sc e
                                     (s2,eqs) <- t2Eqs eqs
                                     return (mergeSubsts [s1,s2], (x,e):eqs)
          where sc              = lookup' te x

        t2Quant tvs0 (x,t)      = do ids <- newNames tyvarSym (length tvs)
                                     let ts  = map TId ids
                                         s   = tvs `zip` ts
                                         ke' = ke ++ ids `zip` map tvKind tvs
                                     e <- encodeTApp ts (EVar x)
                                     return (x, (Scheme (subst s rh) (subst s ps) ke', s, e))
          where tvs             = nub (filter (`notElem` tvs0) (tvars rh))
                Scheme rh ps ke = t
                
        t2Gen env s ss s1 (x,e) = do e' <- encodeTAbs ke (subst s1 (subst s0 e))
                                     return (x, e')
          where s0              = mergeSubsts [lookup' ss x, s]
                ke              = quant (findType env x)


-- Note 3: the program is known to be typeable at this point, thus there is no need to generalize, freshly 
-- instantiate, and then match an inferred type against a skolemized version of the expected type scheme 
-- (together with checking for escaping skolem variables).  Instead, all we need to ensure is that any 
-- type equalities implied by the match are captured in the resulting substitution, treating all-quantified 
-- variables as scoped constants.

t2ExpTscoped env sc e           = do (s1,rh,e') <- t2Exp env e
                                     s2 <- mgi rh (quickSkolem sc)
                                     return (mergeSubsts [s1,s2], e')
                                     

t2ExpT env (Scheme t qs []) e   = t2ExpTscoped env (Scheme t qs []) e
t2ExpT env sc e                 = do (s,e) <- t2ExpTscoped env sc e
                                     e <- encodeTAbs (quant sc) e
                                     return (s, e)


t2ExpTs env [] []               = return (nullSubst, [])
t2ExpTs env (sc:scs) (e:es)     = do (s1,e) <- t2ExpT env sc e
                                     (s2,es) <- t2ExpTs env scs es       
                                     return (mergeSubsts [s1,s2], e:es)


t2Exps env []                   = return (nullSubst, [], [])
t2Exps env (e:es)               = do (s1,t,e) <- t2Exp env e
                                     (s2,ts,es) <- t2Exps env es
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s (t:ts), e:es)
                                      

t2Exp env (ELit l)              = return (nullSubst, R (litType l), ELit l)
t2Exp env (EVar x)              = do (rh,ts) <- t2Inst (findType env x)
                                     e <- encodeTApp ts (EVar x)
                                     return (nullSubst, rh, e)
t2Exp env (ECon k)              = do (rh,ts) <- t2Inst (findType env k)
                                     e <- encodeTApp ts (ECon k)
                                     return (nullSubst, rh, e)
t2Exp env (ESel e l)            = do (F (sc:scs) rh,ts) <- t2Inst (findType env l)
                                     (s,e) <- t2ExpT env sc e
                                     e' <- encodeTApp ts (ESel e l)         -- NOTE: the *full* instantiation of l is remembered here,
                                     return (s, subst s (tFun scs rh), e')  -- including the actual struct type arguments (C.f.: c2k.cExp)
t2Exp env (ELam te e)           = do (s,rh,e) <- t2Exp (addTEnv te env) e
                                     return (s, F (subst s (rng te)) rh, ELam te e)
t2Exp env (EAp e es)            = do (s,rh,e) <- t2Exp env e
                                     t2Ap env s rh e es
t2Exp env (ELet bs e)           = do (s1,bs) <- t2Binds env bs
                                     (s2,rh,e) <- t2Exp (addTEnv (subst s1 (tsigsOf bs)) env) e
                                     return (mergeSubsts [s1,s2], rh, ELet bs e)
t2Exp env (ERec c eqs)          = do alphas <- mapM newTvar (kArgs (findKind env c))
                                     (t,scs) <- t2Lhs env (foldl TAp (TId c) alphas) t2Sel ls
                                     (s,es) <- t2ExpTs env scs es
                                     e <- encodeTApp (snd (tFlat t)) (ERec c (ls `zip` es))
                                     return (s, R (subst s t), e)
  where (ls,es)                 = unzip eqs
        t2Sel env x l           = t2Exp env (ESel (EVar x) l)
t2Exp env (ECase e alts)        = do alpha <- newTvar Star
                                     (TFun [t0] t1,scs) <- t2Lhs env alpha t2Pat ps
                                     (s0,e) <- t2ExpT env (scheme t0) e
                                     (s1,es) <- t2ExpTs env scs es
                                     let s = mergeSubsts [s0,s1]
                                     return (s, R (subst s t1), ECase e (zipWith argsToLhs ps es))
  where (ps,es)                 = unzipAlts (map argsToRhs alts)
        t2Pat env x (PLit l)    = t2Exp env (EAp (EVar x) [ELit l])
        t2Pat env x (PCon k []) = do (rh,_) <- t2Inst (findType env k)
                                     te <- newEnv paramSym (funArgs rh)
                                     t2Exp env (eLam te (EAp (EVar x) [eAp (ECon k) (map EVar (dom te))]))
        t2Pat env x (PWild)     = do y <- newName tempSym
                                     t <- newTvar Star
                                     t2Exp (addTEnv [(y,scheme t)] env) (EAp (EVar x) [EVar y])
t2Exp env (EReq e1 e2)          = do alpha <- newTvar Star
                                     beta <- newTvar Star
                                     (s1,e1) <- t2ExpT env (scheme (tRef alpha)) e1
                                     (s2,e2) <- t2ExpT env (scheme (tCmd alpha beta)) e2
                                     let s = mergeSubsts [s1,s2]
                                     return (s, R (tRequest (subst s beta)), EReq e1 e2)
t2Exp env (EAct e1 e2)          = do alpha <- newTvar Star
                                     beta <- newTvar Star
                                     (s1,e1) <- t2ExpT env (scheme (tRef alpha)) e1
                                     (s2,e2) <- t2ExpT env (scheme (tCmd alpha beta)) e2
                                     let s = mergeSubsts [s1,s2]
                                     return (s, R tAction, EAct e1 e2)
t2Exp env (EDo x tx c)          = do (s1,t,c) <- t2Cmd (setSelf x tx env) c
                                     let s2 = case stateT env of Nothing -> nullSubst; Just t' -> unif [(t',tx)]
                                         s = mergeSubsts [s1,s2]
                                     return (s, R (subst s (tCmd tx t)), EDo x tx c)
t2Exp env (ETempl x tx te c)    = do (s,t,c) <- t2Cmd (setSelf x tx (addTEnv te env)) c
                                     return (s, R (tClass t), ETempl x tx te c)

        
t2Cmd env (CRet e)              = do alpha <- newTvar Star
                                     (s,e) <- t2ExpT env (scheme alpha) e
                                     return (s, subst s alpha, CRet e)
t2Cmd env (CExp e)              = do alpha <- newTvar Star
                                     (s,e) <- t2ExpT env (scheme (tCmd (fromJust (stateT env)) alpha)) e
                                     return (s, subst s alpha, CExp e)
t2Cmd env (CGen x tx e c)       = do (s1,e) <- t2ExpT env (scheme (tCmd (fromJust (stateT env)) tx)) e
                                     (s2,t,c) <- t2Cmd (addTEnv [(x,scheme tx)] env) c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t, CGen x tx e c)
t2Cmd env (CAss x e c)          = do (s1,e) <- t2ExpT env (findType env x) e
                                     (s2,t,c) <- t2Cmd env c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t, CAss x e c)
t2Cmd env (CLet bs c)           = do (s1,bs) <- t2Binds env bs
                                     (s2,t,c) <- t2Cmd (addTEnv (tsigsOf bs) env) c
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s t, CLet bs c)

                                     

t2Ap env s1 (F scs rh) e es     = do (s2,es) <- t2ExpTs env scs es
                                     let s = mergeSubsts [s1,s2]
                                     return (s, subst s rh, EAp e es)
t2Ap env s1 rh e es             = do (s2,rhs,es) <- t2Exps env es
                                     t <- newTvar Star
                                     s3 <- mgi rh (F (map scheme' rhs) (R t))
                                     let s = mergeSubsts [s1,s2,s3]
                                     return (s, R (subst s t), EAp e es)


t2Lhs env alpha t2X xs          = do x <- newName tempSym
                                     let env' = addTEnv [(x,scheme alpha)] env
                                     (ss,rhs,_) <- fmap unzip3 (mapM (t2X env' x) xs)
                                     let s = mergeSubsts ss
                                     scs <- mapM (t2Gen (tevars (subst s env'))) (subst s rhs)
                                     return (subst s alpha, scs)
  where t2Gen tvs0 rh           = do ids <- newNames tyvarSym (length tvs)
                                     let s = tvs `zip` map TId ids
                                     return (Scheme (subst s rh) [] (ids `zip` map tvKind tvs))
          where tvs             = nub (filter (`notElem` tvs0) (tvars rh))
    

t2Inst (Scheme rh ps ke)        = do ts <- mapM newTvar ks
                                     return (subst (vs `zip` ts) (tFun ps rh), ts)
  where (vs,ks)                 = unzip ke



-- Skolemize a type scheme, relying on the uniqueness of all bound type variables
quickSkolem (Scheme rh ps ke)   = tFun ps rh


mgi (R t) (R u)                 = return (unif [(t,u)])
mgi (F ts t) (F us u)           = do s <- mgi t u
                                     ss <- mapM mgiSc (us `zip` ts)
                                     return (mergeSubsts (s:ss))
mgi (R (TFun ts t)) rh          = mgi (F (map scheme ts) (R t)) rh
mgi rh (R (TFun us u))          = mgi rh (F (map scheme us) (R u))
mgi (R t) (F us u)              = do (t':ts) <- mapM newTvar (replicate (length us + 1) Star)
                                     let s1 = unif [(t,TFun ts t')]
                                     s2 <- mgi (R (subst s1 t)) (F us u)
                                     return (s2@@s1)
mgi (F ts t) (R u)              = do (u':us) <- mapM newTvar (replicate (length ts + 1) Star)
                                     let s1 = unif [(u,TFun us u')]
                                     s2 <- mgi (F ts t) (R (subst s1 u))
                                     return (s2@@s1)


mgiSc (Scheme rh [] [], Scheme rh' [] [])
                                = mgi rh rh'
mgiSc (sc, sc')                 = do (rh,_) <- t2Inst sc
                                     mgi rh (quickSkolem sc')


unif []                         = nullSubst
unif ((Tvar n,t):eqs)
  | t == Tvar n                 = unif eqs
  | otherwise                   = let s = n +-> t; s' = unif (subst s eqs) in s' @@ s
unif ((t,Tvar n):eqs)           = let s = n +-> t; s' = unif (subst s eqs) in s' @@ s
unif ((TAp t u, TAp t' u'):eqs) = unif ((t,t'):(u,u'):eqs)
unif ((TId c, TId c'):eqs)
  | c == c'                     = unif eqs
unif ((TFun ts t, TFun us u):eqs)
  | length ts == length us      = unif ((t,u) : (ts `zip` us) ++ eqs)
unif eqs                        = internalError0 ("Type2.unif\n" ++ render (nest 4 (vpr eqs)))


mergeSubsts ss                  = unif (mapFst Tvar (concat ss))
