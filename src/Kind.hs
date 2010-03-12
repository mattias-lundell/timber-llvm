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

module Kind  where

import PP
import Common
import Core
import Env
import Derive
import Depend
import Monad

kindcheck m                             = kiModule m

kiModule (Module _ _ _ _ ds' _ [bs']) (Module v ns xs es ds ws bss)
                                         = do ds <- kiDecls env ds
                                              (bss',xs1) <- derive (concatMap bvars bss ++ bvars bs') (ds' `catDecls` ds) xs
                                              let env' = addKEnv0 (ksigsOf ds) env
                                              bss <- mapM (kiBinds env') (bss++bss')
                                              return (Module v ns xs1 es ds (ws++dom(concatMap tsigsOf bss')) bss)
  where env                             = addKEnv0 (ksigsOf ds') (initEnv v)


-- Kind unification ------------------------------------------------------------

type KSubst = Map Int Kind

type KEqs = [(Kind,Kind)]

kunify                                  :: KEqs -> M s KSubst
kunify []                               = return nullSubst
kunify ((Star,Star):cs)                 = kunify cs
kunify ((KVar n,k):cs)                  = kvarBind n k cs
kunify ((k,KVar n):cs)                  = kvarBind n k cs
kunify ((KFun k1 k2,KFun k1' k2'):cs)   = kunify ((k1,k1'):(k2,k2'):cs)
kunify ((k1,k2):eqs)                    = fail ("Kinds do not unify: " ++ render (pr k1 <+> text "and" <+> pr k2))

kvarBind n k cs
  | k == KVar n                         = kunify cs
  | n `elem` kvars k                    = fail "Infinite kind inferred"
  | otherwise                           = do s' <- kunify (subst s cs)
                                             return (s' @@ s)
  where s                               = n +-> k

kindUnify cs                            = do s <- kunify cs
                                             return (freeVars s `zip` repeat Star @@ s)
  where freeVars s                      = nub (concat (map kvars (rng s)))


kiRho env (R t)                         = kiType env t
kiRho env (F scs t)                     = do css <- mapM (kiScheme env) scs
                                             cs  <- kiRho env t
                                             return (concat css ++ cs)

kiType env t                            = do (cs,k) <- kiTExp env t
                                             return ((k,Star) : cs)

kiType' env t                           = do cs <- kiType env t
                                             s <- kunify cs
                                             return (subst s t)

kiTExp env (TFun ts t)                  = do css <- mapM (kiType env) ts
                                             cs  <- kiType env t
                                             return (cs ++ concat css, Star)
kiTExp env (TId c)                      = return ([], findKind env c)
kiTExp env (Tvar n)                     = return ([], tvKind n)
kiTExp env (TAp t t')                   = do (cs,k) <- kiTExp env t
                                             (cs',k') <- kiTExp env t'
                                             kv <- newKVar
                                             return ((k,KFun k' kv):cs++cs', kv)

-- Handle type declarations ----------------------------------------------------

kiDecls env t@(Types ke ds)             = do css <- mapM (kiDecl (addKEnv0 ke env)) ds
                                             s <- kindUnify (concat css) `handle` \m -> errorTree m t
                                             return (Types (subst s ke) (subst s ds))


newTScope env vs                        = do ks <- mapM (const newKVar) vs
                                             return (addKEnv (vs `zip`ks) env)


kiDecl env (i, DData vs bs ks)          = do env' <- newTScope env vs
                                             cs   <- kiType env' (tAp' i vs)
                                             css1 <- mapM (kiScheme env') bs
                                             css2 <- mapM (kiConstr env' . snd) ks
                                             return (cs ++ concat (css1 ++ css2))
kiDecl env (i, DRec _ vs bs ss)         = do env' <- newTScope env vs
                                             cs   <- kiType env' (tAp' i vs)
                                             css1 <- mapM (kiScheme env') bs
                                             css2 <- mapM (kiScheme env' . snd) ss
                                             return (cs ++ concat (css1 ++ css2))
kiDecl env (i, DType vs t)              = do env' <- newTScope env vs
                                             (cs1,k1) <- kiTExp env' (tAp' i vs)
                                             (cs2,k2) <- kiTExp env' t
                                             return ((k1,k2) : cs1 ++ cs2)


kiConstr env (Constr ts ps ke)          = do css1 <- mapM (kiScheme env') ts
                                             css2 <- mapM (kiScheme env') ps
                                             return (concat (css1 ++ css2))
  where env'                            = addKEnv ke env


kiScheme env (Scheme t ps ke)           = do cs <- kiRho env' t
                                             css <- mapM (kiScheme env') ps
                                             return (cs ++ concat css)
  where env'                            = addKEnv ke env


kiTEnv env te                           = do css <- mapM (kiScheme env . snd) te
                                             s <- kindUnify (concat css) `handle` \m -> errorTree m te
                                             return (subst s te)

kiMaybeScheme env Nothing               = return []
kiMaybeScheme env (Just t)              = kiScheme env t


-- Handle bindings -------------------------------------------------------------

kiBinds env (Binds r te es)             = do te <- kiTEnv env te
                                             es <- mapM (kiEqn env) es
                                             return (Binds r te es)


-- Traverse expressions --------------------------------------------------------


kiExp env (ELam te e)                   = do te <- kiTEnv env te
                                             e <- kiExp env e
                                             return (ELam te e)
kiExp env (EAp e es)                    = do e  <- kiExp env e
                                             es <- mapM (kiExp env) es
                                             return (EAp e es)
kiExp env (ELet bs e)                   = do bs <- kiBinds env bs
                                             e <- kiExp env e
                                             return (ELet bs e)
kiExp env (ERec c es)                   = do es <- mapM (kiEqn env) es
                                             return (ERec c es)
kiExp env (ECase e alts)                = do e <- kiExp env e
                                             alts <- mapM (kiAlt env) alts
                                             return (ECase e alts)
kiExp env (EAct e e')                   = do e <- kiExp env e
                                             e' <- kiExp env e'
                                             return (EAct e e')
kiExp env (EReq e e')                   = do e <- kiExp env e
                                             e' <- kiExp env e'
                                             return (EReq e e')
kiExp env (ETempl x t te c)             = do t <- kiType' env t
                                             te <- kiTEnv env te
                                             c <- kiCmd env c
                                             return (ETempl x t te c)
kiExp env (EDo x t c)                   = do t <- kiType' env t
                                             c <- kiCmd env c
                                             return (EDo x t c)
kiExp env e                             = return e


kiAlt env (Alt p e)                     = do p <- kiPat env p
                                             e <- kiExp env e
                                             return (Alt p e)

kiPat env (PCon c te)                   = do te <- kiTEnv env te
                                             return (PCon c te)
kiPat env p                             = return p

kiEqn env (v,e)                         = do e <- kiExp env e
                                             return (v,e)



-- Traverse commands -----------------------------------------------------------

kiCmd env (CAss x e c)                  = do e <- kiExp env e
                                             c <- kiCmd env c
                                             return (CAss x e c)
kiCmd env (CGen x t e c)                = do cs <- kiType env t
                                             s <- kindUnify cs `handle` \m -> errorTree m t
                                             e <- kiExp env e
                                             c <- kiCmd env c
                                             return (CGen x (subst s t) e c)
kiCmd env (CLet bs c)                   = do bs <- kiBinds env bs
                                             c <- kiCmd env c
                                             return (CLet bs c)
kiCmd env (CRet e)                      = do e <- kiExp env e
                                             return (CRet e)
kiCmd env (CExp e)                      = do e <- kiExp env e
                                             return (CExp e)


-- Compute kind of general type expression -------------------------------------

kindOfType env (TFun _ _)               = Star
kindOfType env (Tvar n)                 = tvKind n
kindOfType env (TId c)                  = findKind env c
kindOfType env (TAp t t')               = k
  where KFun k' k                       = kindOfType env t
