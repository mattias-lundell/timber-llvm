{-# LANGUAGE PatternGuards, ParallelListComp, FlexibleInstances #-}

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

module Kindlered where

import Monad
import Common
import Kindle
import PP
import System

kindlered ds m                          = redModule ds m


data Env                                = Env { decls :: Decls }

nullEnv                                 = Env { decls = [] }

addDecls ds env                         = env { decls = ds ++ decls env }

findSel env l                           = head [ t | (_,Struct _ te _) <- decls env, (l',t) <- te, l'==l ]


-- Convert a module
redModule dsi (Module m ns es ds bs)    = do bs <- mapM (redBind env0) bs
                                             return (Module m ns es ds bs)
  where env0                            = addDecls (ds++dsi) nullEnv


-- Convert a binding
redBind env (x, Fun vs t te c)          = do c <- redCmd env c
                                             c <- tailOptimize x te c
                                             return (x, Fun vs t te c)
redBind env (x, Val t e)                = do e <- redExp env e
                                             return (x, Val t e)
-- Tail recursion optimization

tailOptimize x te c
  | isTailRecursive x c                 = do c <- redTailCall x te c
                                             return (CWhile (ELit (lInt 1)) c (CRaise (ELit (lInt 1))))
  | otherwise                           = return c


isTailRecursive x (CRet (ECall y _ _))  = x == y
isTailRecursive x (CRun _ c)            = isTailRecursive x c
isTailRecursive x (CBind _ _ c)         = isTailRecursive x c
isTailRecursive x (CUpd _ _ c)          = isTailRecursive x c
isTailRecursive x (CUpdS _ _ _ c)       = isTailRecursive x c
isTailRecursive x (CUpdA _ _ _ c)       = isTailRecursive x c
isTailRecursive x (CSwitch _ alts)      = or [isTailRecursiveAlt x a | a <- alts]
isTailRecursive x (CSeq c1 c2)          = (isTailRecursive x c1) || (isTailRecursive x c2)
isTailRecursive x (CWhile _ c1 c2)      = isTailRecursive x c2
isTailRecursive _ _                     =  False

isTailRecursiveAlt x (ACon _ _ _ c)     = isTailRecursive x c
isTailRecursiveAlt x (ALit _ c)         = isTailRecursive x c
isTailRecursiveAlt x (AWild c)          = isTailRecursive x c


redTailCall x vs (CRet (ECall y ts es))    
  | x == y                              = updateParams vs es
redTailCall x vs (CBind r bs c)         = liftM (CBind r bs) (redTailCall x vs c)
redTailCall x vs (CRun e c)             = liftM (CRun e) (redTailCall x vs c)
redTailCall x vs (CUpd y e c)           = liftM (CUpd y e) (redTailCall x vs c)
redTailCall x vs (CUpdS e y v c)        = liftM (CUpdS e y v) (redTailCall x vs c)
redTailCall x vs (CUpdA e i e' c)       = liftM (CUpdA e i e') (redTailCall x vs c)
redTailCall x vs (CSwitch e alts)       = liftM (CSwitch e) (mapM (redTailAlt x vs) alts)
redTailCall x vs (CSeq c c')            = liftM2 CSeq (redTailCall x vs c) (redTailCall x vs c')
redTailCall x vs (CWhile e c c')        = liftM2 (CWhile e) (redTailCall x vs c) (redTailCall x vs c')
redTailCall _ _ c                       = return c

redTailAlt x vs (ACon y us te c)        = liftM (ACon y us te) (redTailCall x vs c)
redTailAlt x vs (ALit l c)              = liftM (ALit l) (redTailCall x vs c)
redTailAlt x vs (AWild c)               = liftM AWild (redTailCall x vs c)

updateParams [] []                      = return CCont
updateParams ((x,t):te) (e:es)
  | e == EVar x                         = updateParams te es
  | x `elem` evars es                   = do y <- newName tempSym
                                             c <- updateParams te (subst [(x,EVar y)] es)
                                             return (CBind False [(y,Val t (EVar x))] (CUpd x e c))
  | otherwise                           = do c <- updateParams te es
                                             return (CUpd x e c)



single x e                              = length (filter (==x) (evars e)) == 1

newRef (ENew (Prim Ref _) _ _)          = True
newRef _                                = False

-- Convert a command
redCmd env c
  | not (null es)                       = liftM CRaise (redExp env (head es))
  where es                              = raises (subexps c)
redCmd env (CRet e)                     = do e <- redExp env e
                                             redRet env e
redCmd env e0@(CBind False [(x,Val _ e)] (CRet e'))
  | single x e' && not (newRef e)       = redCmd env (CRet (subst [(x,e)] e'))
redCmd env (CBind r bs c)               = liftM2 (CBind r) (mapM (redBind env) bs) (redCmd env c)
redCmd env (CRun e c)                   = liftM2 CRun (redExp env e) (redCmd env c)
redCmd env (CUpd x e c)                 = liftM2 (CUpd x) (redExp env e) (redCmd env c)
redCmd env (CUpdS e x v c)              = do f <- redAssign env (arrayDepth t) (ESel e x) v
                                             liftM f (redCmd env c)
  where ValT t                          = findSel env x
redCmd env (CUpdA e i e' c)             = do e <- redExp env e
                                             liftM2 (CUpdA e i) (redExp env e') (redCmd env c)
redCmd env (CSwitch e alts)             = liftM2 CSwitch (redExp env e) (mapM (redAlt env) alts)
redCmd env (CSeq c c')                  = liftM2 CSeq (redCmd env c) (redCmd env c')
redCmd env (CBreak)                     = return CBreak
redCmd env (CRaise e)                   = liftM CRaise (redExp env e)
redCmd env (CWhile e c c')              = liftM3 CWhile (redExp env e) (redCmd env c) (redCmd env c')
redCmd env (CCont)                      = return CCont


redRet env (EEnter e f ts es)           = do c <- redRet env e
                                             return (cMap (ff env f ts es) c)
redRet env e                            = return (CRet e)


ff env f ts es (ENew n _ bs)
  | not (refThis c)                     = subst (vs `zip` ts) (subst (dom te `zip` es) c)
  where Fun vs t te c                   = lookup' bs f
ff env f ts es e                        = CRet (EEnter e f ts es)


-- Convert a switch alternative
redAlt env (ACon x vs te c)             = liftM (ACon x vs te) (redCmd env c)
redAlt env (ALit l c)                   = liftM (ALit l) (redCmd env c)
redAlt env (AWild c)                    = liftM AWild (redCmd env c)




-- Convert an expression
redExp env (EEnter e f ts es)           = do e <- redExp env e
                                             es <- mapM (redExp env) es
                                             redEnter env e f ts es
redExp env e@(ECall (Prim IndexArray _) _ _)
                                        = redIndexArray env e 0
redExp env (ECall p@(Prim SizeArray _) [t] [e])
                                        = liftM (ECall p [t] . (:[])) (redExp' env e)
redExp env (ECall x ts es)              = do es <- mapM (redExp env) es
                                             return (ECall x ts es)
redExp env (ENew n ts bs)               = do bs <- mapM (redBind env) bs
                                             redNew env n ts bs
redExp env (EVar x)                     = return (EVar x)
redExp env (EThis)                      = return (EThis)
redExp env (ELit l)                     = return (ELit l)
redExp env a@(ESel e l)
  | stateVar (annot l)                  = redIndexArray env a 0
  | otherwise                           = liftM (flip ESel l) (redExp env e)
redExp env (ECast t e)                  = liftM (ECast t) (redExp env e)


redEnter env e@(ENew n _ bs) f ts es    = case c of
                                             CRet e' | not (refThis e') 
                                                    -> return (subst (vs `zip` ts) (subst (dom te `zip` es) e'))
                                             _      -> return (EEnter e f ts es)
  where Fun vs t te c                   = lookup' bs f
redEnter env e f ts es                  = return (EEnter e f ts es)


redNew env _ _ [(Prim Code _, Fun vs _ te (CRet (EEnter e (Prim Code _) ts es)))]
  | ts == map tVar vs && es == map EVar (dom te) && not (refThis e)
                                        = return e
redNew env n ts bs                      = return (ENew n ts bs)


-- Convert an expression in a safe context (no cloning needed)
redExp' env (ECast t e)                 = liftM (ECast t) (redExp' env e)
redExp' env (ESel e l)                  = liftM (flip ESel l) (redExp env e)
redExp' env e                           = redExp env e


-- Convert an array indexing expression
redIndexArray env (ECall (Prim IndexArray _) [t] [a,i]) n
                                        = do a <- redIndexArray env a (n+1)
                                             i <- redExp env i
                                             return (indexArray t a i)
redIndexArray env (ESel e l) n
  | stateVar (annot l)                  = do e <- redExp env e
                                             return (clone (arrayDepth t - n) t (ESel e l))
  where ValT t                          = findSel env l
redIndexArray env a n                   = redExp env a


indexArray t a i                        = ECall (prim IndexArray) [t] [a,i]


intExp i                                = ELit (lInt i)

arrayDepth (TCon (Prim Array _) [t])    = 1 + arrayDepth t
arrayDepth _                            = 0

clone 0 t e                             = e
clone 1 t e@(ECall (Prim p _) _ _)
  | p `elem` [ListArray,UniArray]       = e
clone _ t e@(ECall (Prim p _) _ _)
  | p `elem` [EmptyArray,SizeArray]     = e
clone n t (ECall (Prim CloneArray _) _ [e,ELit (LInt _ n')])
  | toInteger n == n'                   = e
clone n t e                             = (ECall (prim CloneArray) [stripArray n t] [e, intExp n])

stripArray 0 t                          = t
stripArray n (TCon (Prim Array _) [t])  = stripArray (n-1) t

{-
    a|x|y|z := e
    a|x|y   := a|x|y \\ (z,e)
    a|x     := a|x \\ (y, a|x|y \\ (z,e))
    a       := a \\ (x, a|x \\ (y, a|x|y \\ (z,e)))
-}

redAssign env n e0 (ECall (Prim UpdateArray _) [t] [a,i,v])
  | e0 == a                         = redAssign env (n-1) (indexArray t a i) v
  | otherwise                       = do f1 <- redAssign env n e0 a
                                         f2 <- redAssign env (n-1) (indexArray t e0 i) v
                                         return (f1 . f2)
redAssign env n e0 (ECall (Prim ListArray _) [t] [e])
  | Just es <- constElems e, 
    let m = length es               = do f <- redAssign env n e0 (ECall (prim EmptyArray) [t] [ELit (lInt (toInteger m))])
                                         fs <- mapM mkAssign ([0..] `zip` es)
                                         return (foldl (.) f fs)
  where constElems (ENew (Prim CONS _) _ bs)
                                    = do es <- constElems eb
                                         return (ea:es)
          where Val _ ea            = lookup' bs (head abcSupply)
                Val _ eb            = lookup' bs (head (tail abcSupply))
        constElems (ENew (Prim NIL _) _ bs)
                                    = Just []
        constElems _                = Nothing
        mkAssign (i,e)              = redAssign env (n-1) (indexArray t e0 (intExp i)) e
redAssign env n e0 (ECall (Prim UniArray _) [t] [m,e])
                                    = do x <- newName tempSym
                                         s <- newName tempSym
                                         i <- newName paramSym
                                         let f0 = cBind [(x,Val t e),(s,Val tInt m),(i,Val tInt (intExp 0))]
                                         f <- redAssign env n e0 (ECall (prim EmptyArray) [t] [EVar s])
                                         let f1 c = CWhile (ECall (prim IntLT) [] [EVar i, EVar s]) c
                                         f2 <- redAssign env (n-1) (indexArray t e0 (EVar i)) (EVar x)
                                         let c = CUpd i (ECall (prim IntPlus) [] [EVar i, intExp 1]) CCont
                                         return (f0 . f . f1 (f2 c))
redAssign env n (ESel e x) v        = do v' <- redExp env v
                                         return (CUpdS e x (clone n t v'))
  where ValT t                      = findSel env x
redAssign env n (ECall (Prim IndexArray _) [t] [e,i]) v
                                    = do v' <- redExp env v
                                         return (CUpdA e i (clone n t v'))

