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

module Decls where

import Common
import Core
import Env
import Reduce
import PP

-- Declaration processing ---------------------------------------------------------------------

{-

?    Check acyclic tsym deps   
?    Generate future subtype instances
?    Generate future class instances

-}


-- Extend kind environment
-- Initialize empty class witness graph
-- Extract selector and constructor type schemes
-- Construct class member type schemes and bindings
-- Replace subtyping in type declarations with explicit selectors/constructors
-- Initialize subtyping graph with reflexivity witnesses
-- Close subtyping graph under transitivity (report cyclic and ambiguity errors)
-- Return extended environment, transformed decls and added witness bindings


typeDecls env ps (Types ke ds)          = do (ds,pe1,eq1) <- desub env0 ds
                                             (env',bs) <- instancePreds env0 (pe1 ++ ps)
                                             let tds = Types ke ds
                                             return (addTEnv0 (tenvSelsCons tds) env', tds, catBinds (Binds False pe1 eq1)  bs)
  where env0                            = addClasses cs (addKEnv0 ke env)
        cs                              = [ c | (c, DRec True _ _ _) <- ds ]

impDecls env (Types ke ds)              =  env1
  where 
        env1                            = addClasses cs (addKEnv0 ke env)
        cs                              = [ c | (c, DRec True _ _ _) <- ds ]

-- Close the top-level instance delcarations
-- Return the extended environment and the added witness bindings

instancePreds env pe                    = do (env',qe,eq) <- closePreds0 env pe
                                             let bss = preferParams env' pe qe eq
                                             return (env', concatBinds bss)

impPreds env pe                         = addPreds (addPEnv0 pe env) pe


-- Computes the stand-alone type schemes associated with selectors and constructors
-- Note: these constants have no corresponding definition (i.e., no rhs)

tenvSelsCons (Types ke ds)              = concatMap (tenvSelCon ke) ds

tenvSelCon ke0 (c,DRec _ vs _ ss)       = map (f t ke) ss
  where (t,ke)                          = mkHead ke0 c vs
        f t ke (l, Scheme rh ps ke')    = (l, Scheme rh (scheme t : ps) (ke++ke'))
tenvSelCon ke0 (c,DData vs _ cs)        = map (f t ke) cs
  where (t,ke)                          = mkHead ke0 c vs
        f t ke (k, Constr ts ps ke')    = (k, Scheme (tFun' ts t) ps (ke++ke'))
tenvSelCon ke0 _                        = []

tenvCon ke0 (c,DData vs _ cs)           = map (f t ke) cs
  where (t,ke)                          = mkHead ke0 c vs
        f t ke (k, Constr ts ps ke')    = (k, Scheme (tFun' ts t) ps (ke++ke'))
tenvCon ke0 _                           = []

mkHead ke0 i vs                         = (tAp' i vs, vs `zip` kArgs (findKind0 ke0 i))


-- Decomposition of type declarations ---------------------------------------------------------

desub env ds                            = do (ds',pes,eqs) <- fmap unzip3 (mapM desub' ds)
                                             return (ds', concat pes, concat eqs)
  where 
    desub' (i, DData vs bs cs)          = do (pe,eq,cs') <- fmap unzip3 (mapM (con (fromMod i) i (mkHead ke0 i vs)) bs)
                                             return ((i, DData vs [] (cs'++cs)), pe, eq)
    desub' (i, DRec isC vs bs ss)       = do (pe,eq,ss') <- fmap unzip3 (mapM (sel (fromMod i) i (mkHead ke0 i vs)) bs)
                                             return ((i, DRec isC vs [] (ss'++ss)), pe, eq)
    desub' (i, DType vs t)              = return ((i, DType vs t), [], [])
    ke0                                 = kindEnv0 env
    con m i (t0,ke0) (Scheme (R t) [] ke) =
                                          do w <- newNameModPub m (public (annot i)) coercionSym
                                             k <- newNameModPub m (public (annot i)) (coerceConstr t t0)
                                             x <- newName paramSym
                                             let p  = (w, Scheme (R (t `sub` t0)) [] (ke0++ke))
                                                 eq = (w, ELam [(x,scheme t)] (EAp (ECon k) [EVar x]))
                                                 c  = (k, Constr [scheme t] [] ke)
                                             return (p, eq, c)
    sel m i (t0,ke0) (Scheme (R t) [] ke) =
                                          do w <- newNameModPub m (public (annot i)) coercionSym
                                             l <- newNameModPub m (public (annot i)) (coerceLabel t0 t)
                                             x <- newName paramSym
                                             let p  = (w, Scheme (R (t0 `sub` t)) [] (ke0++ke))
                                                 eq = (w, ELam [(x,scheme t0)] (ESel (EVar x) l))
                                                 s  = (l, Scheme (R t) [] ke)
                                             return (p, eq, s)

coerceConstr t t0                       = coerceConstrSym ++ "_" ++ render (prId3 (tId(tHead t))) ++ "_" ++ render (prId3 (tId (tHead t0)))

coerceLabel t0 t                        = coerceLabelSym  ++ "_" ++ render (prId3 (tId(tHead t0))) ++ "_" ++ render (prId3 (tId (tHead t)))

{-

   *data Exists m = All a . Pack (All b . D m b a) (All b . C m b a -> m b a)

    Pack :: All m,a . (All b . D m b a) -> (All b . C m b a -> m b a) -> Exists m

    f1 :: All a,b . (All b . D m b a) -> D m' b a
    f1 :: All a . (All b . D m b a) -> (All b . D m' b a)
    f2 :: All a,b . (All b . D m b a) -> (All b . C m b a -> m b a) -> C m' b a -> m' b a
      f2 :: All a . (All b . D m b a) -> (All b . C m b a -> m b a) -> (All b . C m' b a -> m' b a)
    case x of
      Pack -> \d::(All b . D m b a) -> \r::(All b . C m b a -> m b a) -> Pack (f1 d) (f2 d r)


   *data Exists m = All a . (All b . D m b a) => Pack (All b . C m b a -> m b a)

    Pack :: All m,a . (All b . D m b a) => (All b . C m b a -> m b a) -> Exists m

    f1 :: All a,b . (All b . D m b a) -> D m' b a
      f1 :: All a . (All b . D m b a) -> (All b . D m' b a)
    f2 :: All a,b . (All b . D m b a) -> (All b . C m b a -> m b a) -> C m' b a -> m' b a
      f2 :: All a . (All b . D m b a) -> (All b . C m b a -> m b a) -> (All b . C m' b a -> m' b a)
    case x of
      Pack -> \d::(All b . D m b a) => \r::(All b . C m b a -> m b a) -> Pack (f1 d) (f2 d r)


    
   *data Exists m = Pack (D m b a \\ b) (C m b a -> m b a \\ b) \\ a

    Pack :: (D m b a \\ b) -> (C m b a -> m b a \\ b) -> Exists m \\ m, a

    f1 :: (D m b a \\ b) -> D m' b a \\ a, b
      f1 :: (D m b a \\ b) -> (D m' b a \\ b) \\ a
    f2 :: (D m b a \\ b) -> (C m b a -> m b a \\ b) -> C m' b a -> m' b a \\ a, b
      f2 :: (D m b a \\ b) -> (C m b a -> m b a \\ b) -> (C m' b a -> m' b a \\ b) \\ a
    case x of
      Pack -> \d::(D m b a \\ b) -> \r::(C m b a -> m b a \\ b) -> Pack (f1 d) (f2 d r)


   *data Exists m = Pack (m b a \\ b, C m b a) \\ a, (D m b a \\ b)

    Pack :: (m b a \\ b, C m b a) -> Exists m \\ m, a, (D m b a \\ b)

    f1 :: (D m b a \\ b) -> D m' b a \\ a,b
      f1 :: (D m b a \\ b) -> (D m' b a \\ b) \\ a
    f2 :: (m b a \\ b, C m b a) -> m' b a \\ a, b, C m' b a, (D m b a \\ b)
      f2 :: (m b a \\ b, C m b a) -> (m' b a \\ b, C m' b a) \\ a, (D m b a \\ b)
    case x of
      Pack -> \d::(D m b a \\ b) => \r::(m b a \\ b, C m b a) -> Pack (f1 d) (f2 d r)



   *record T a =
        x :: b -> a -> b \\ b, b < a

    x :: T a -> b -> a -> b \\ b, b < a

    f :: (b -> a -> b \\ b, b < a) -> b -> a' -> b \\ b, b < a'
      f :: (b -> a -> b \\ b, b < a) -> (b -> a' -> b \\ b, b < a')
    { x = f r.x }



   *record T a =
        x :: (b<a) -> b -> a -> b \\ b

    x :: T a -> (b<a) -> b -> a -> b \\ b

    f :: ((b<a) -> b -> a -> b \\ b) -> (b<a') -> b -> a' -> b \\ b
      f :: ((b<a) -> b -> a -> b \\ b) -> ((b<a') -> b -> a' -> b \\ b)
    { x = f r.x }
-}

