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

module Lambdalift(lambdalift) where

import Monad
import Common
import Kindle
import PP


lambdalift ds m                 = localStore (llModule ds m)


data Env                        = Env { decls      :: Decls,                    -- global type declarations
                                        thisSubst  :: Map Name AType,           -- 
                                        thisVars   :: [Name],                   -- variables reachable through "this"
                                        locals     :: ATEnv,                    -- non-global value names in scope
                                        expansions :: Map Name ([Name],[Name])  -- non-global functions and their added type/term parameters
                                      }

nullEnv                         = Env { decls = primDecls,
                                        thisSubst = [],
                                        thisVars = [],
                                        locals = [],
                                        expansions = []
                                      }

addDecls ds env                         = env { decls = ds ++ decls env }

setThis vs xs env                       = env { thisSubst = vs `zip` map TThis [1..], thisVars = xs }

addLocals te env                        = env { locals = te ++ prune (locals env) (dom te) }    -- prune so that shadowed variables don't
                                                                                                -- get lifted multiple times

addExpansions exps env                  = env { expansions = exps ++ expansions env }

findStructInstance env (Prim CLOS _) ts = (closBind ts, [])
findStructInstance env n ts             = (subst s te, [ t | (v,t) <- s, v `elem` vs' ])
  where Struct vs te l                  = if isTuple n then tupleDecl n else lookup' (decls env) n
        s                               = vs `zip` ts
        vs'                             = equants l

-- Convert a module
llModule dsi (Module m ns es ds bs)     = do bs <- mapM (llBind env0) bs
                                             s <- currentStore
                                             return (Module m ns es (ds ++ declsOfStore s) (bs ++ bindsOfStore s))
  where env0                            = addDecls (ds ++ dsi) nullEnv


declsOfStore s                          = [ d | Left d <- s ]

bindsOfStore s                          = [ b | Right b <- s ]


-- Convert a binding
llBind env (x, Fun vs t te c)           = do c <- llCmd (addLocals te env) c
                                             return (x, Fun vs t te c)
llBind env (x, Val t e)                 = do e <- llExp env e
                                             return (x, Val (mkT env t) e)


{- Lambda-lifting a set of recursive bindings:

   \v ->                                                letrec f r v x = ... r ... v ...
   letrec f x = ... r ... v ...                                g r v y = ... f r v e ...
          r   = { a = ... g e ... }         ==>         in \v ->
          g y = ... f e ...                             letrec r  = { a = ... g r v e ... }
   in f e                                               in f r v e
-}

{- Closing a struct with function fields:

   \v ->                                                \v ->
   { f x = ... v ...                                    { f x = ... this.v ...
     w   = ... v ... }                      ==>           w   = ... v ...
                                                          v   = v }
-}

-- Convert a command
llCmd env (CBind r bs c)                = do vals' <- mapM (llBind env') vals
                                             funs' <- mapM (llBind (setThis [] [] env')) funs
                                             store <- currentStore
                                             let fs = dom funs' `intersect` [ f | Right (f,_) <- store ]
                                             fs' <- mapM (newName . str) fs
                                             let s = fs `zip` fs'
                                             mapM_ liftFun (subst s funs')
                                             c' <- llCmd env2 c
                                             return (cBindR r (subst s vals') (subst s c'))
  where (vals,funs)                     = partition isVal bs
        free0                           = evars funs
        free1                           = free0 ++ concat [ xs | (f,(_,xs)) <- expansions env, f `elem` free0 ]
        fte                             = locals (if r then env1 else env) `restrict` free1
        vs1                             = concat [ vs | (f,(vs,_)) <- expansions env, f `elem` free0 ]
        fvs                             = nub (typevars funs ++ typevars fte ++ vs1)
        env1                            = addLocals (mapSnd typeOf' vals) env
        env2                            = addExpansions (dom funs `zip` repeat (fvs, dom fte)) env1
        env'                            = if r then env2 else env
        liftFun (x, Fun vs t te c)      = addToStore (Right (x, Fun (fvs++vs) t (fte++te) c))
llCmd env (CRet e)                      = liftM CRet (llExp env e)
llCmd env (CRun e c)                    = liftM2 CRun (llExp env e) (llCmd env c)
llCmd env (CUpd x e c)                  = liftM2 (CUpd x) (llExp env e) (llCmd env c)
llCmd env (CUpdS e x e' c)              = do e <- llExp env e
                                             liftM2 (CUpdS e x) (llExp env e') (llCmd env c)
llCmd env (CUpdA e i e' c)              = liftM4 CUpdA (llExp env e) (llExp env i) (llExp env e') (llCmd env c)
llCmd env (CSwitch e alts)              = liftM2 CSwitch (llExp env e) (mapM (llAlt env) alts)
llCmd env (CSeq c c')                   = liftM2 CSeq (llCmd env c) (llCmd env c')
llCmd env (CBreak)                      = return CBreak
llCmd env (CRaise e)                    = liftM CRaise (llExp env e)
llCmd env (CWhile e c c')               = liftM3 CWhile (llExp env e) (llCmd env c) (llCmd env c')
llCmd env (CCont)                       = return CCont


-- Convert a switch alternative
llAlt env (ACon x vs te c)              = liftM (ACon x vs (mkT env te)) (llCmd (addLocals te env) c)
llAlt env (ALit l c)                    = liftM (ALit l) (llCmd env c)
llAlt env (AWild c)                     = liftM AWild (llCmd env c)


-- Convert an expression
llExp env (ECall x ts es)               = do es <- mapM (llExp env) es
                                             case lookup x (expansions env) of
                                                Just (vs,xs) -> return (ECall x (mkT env (map tVar vs ++ ts)) (map (mkEVar env) xs ++ es))
                                                Nothing -> return (ECall x (mkT env ts) es)
llExp env ee@(ENew n ts0 bs)
  | null fte && null fvs                = liftM (ENew n ts) (mapM (llBind env) bs)
  | otherwise                           = do n' <- getStructName (Struct tvs (te ++ mapSnd ValT fte) (Extends n ts tvs))
                                             vals' <- mapM (llBind env) vals
                                             funs' <- mapM (llBind (setThis tvs (dom fte) env)) funs
                                             -- tr ("lift ENew: " ++ render (pr (TCon n ts)) ++ "  fvs: " ++ show fvs ++ "   new: " ++ show n')
                                             return (ECast (TCon n ts) (ENew n' (ts'++map close' fvs) (vals' ++ funs' ++ map close fte)))
  where ts                              = mkT env ts0
        (vals,funs)                     = partition isVal bs
        free0                           = evars funs
        free1                           = free0 ++ concat [ xs | (f,(_,xs)) <- expansions env, f `elem` free0 ]
        fte                             = locals env `restrict` free1
        vs1                             = concat [ vs | (f,(vs,_)) <- expansions env, f `elem` free0 ]
        fvs                             = nub (typevars funs ++ typevars fte ++ vs1)
        tvs                             = take (length ts') abcSupply ++ fvs
        (te,ts')                        = findStructInstance env n ts
        close (x,t)                     = (x, Val (mkT env t) (mkEVar env x))
        close' v                        = mkT env (tVar v)
llExp env (EVar x)                      = return (mkEVar env x)
llExp env (EThis)                       = return (EThis)
llExp env (ELit l)                      = return (ELit l)
llExp env (ESel e l)                    = liftM (flip ESel l) (llExp env e)
llExp env (EEnter e f ts es)            = do e <- llExp env e
                                             liftM (EEnter e f (mkT env ts)) (mapM (llExp env) es)
llExp env (ECast t e)                   = liftM (ECast (mkT env t)) (llExp env e)


mkEVar env x                            = if x `elem` thisVars env then ESel EThis x else EVar x

mkT env t                               = subst (thisSubst env) t

getStructName str                       = do s <- currentStore
                                             case findStruct str (declsOfStore s) of
                                               Just n  -> return n
                                               Nothing -> do n <- newName typeSym
                                                             addToStore (Left (n, str))
                                                             return n
