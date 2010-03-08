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

module Derive where

import Common
import Core
import Env
import PP


derive ts ds []                       = return ([],[])
derive ts ds (d@(Default _ _ _) : xs)
                                      = do (is,xs) <- derive ts ds xs
                                           return (is,d:xs)
derive ts ds (Derive n sc : xs)       = do let (s,d) = analyze ds sc
                                               n1 = str (fst s)
                                               m = fromMod (fst s)
                                           (is,xs) <- derive ts ds xs
                                           if n1=="Show" && m==Just "Prelude"
                                             then do i <- mkShowInstance n sc s d
                                                     return (i:is,xs)
                                             else 
                                             if n1=="Parse" && m==Just "Prelude" 
                                             then do let eq = EVar (findByNameStr ts "==")
                                                     i <- mkParseInstance n eq sc s d
                                                     return (i:is,xs)
                                             else do b <- mkFunPair d
                                                     i <- mkInstance n ts s sc b
                                                     -- tr ("### Default instance \n"++render (prInsts i))
                                                     return (i:is,xs)

analyze ds sc                         = case tFlat (scheme2Type sc) of
                                           (TId c,[t']) -> findDataType c t'
                                           _ -> errorTree "Not an instance of a one-parameter type constructor" sc 
  where findDataType c t'             = case tFlat t' of
                                          (TId d,_) -> case lookup c (tdefsOf ds) of
                                             Nothing -> errorTree ("Unknown typeclass "++show c) sc
                                             Just c'@(DRec True [a] _ _) -> case lookup d (tdefsOf ds) of
                                                                              Nothing -> errorTree ("Unknown data type "++show d) sc
                                                                              Just d'@(DData _ _ _) -> ((c,c'),(d,d')) 
                                                                              Just _ -> errorTree (show d++" is not a data type") sc
                                             Just (DRec True _ _ _) -> errorTree "Default instances only for one-parameter typeclasses; not" sc
                                             Just _ -> errorTree (show c++" is not a typeclass") sc         
                                                   
mkInstance n ts (nm,DRec True [a] ps ss) sc (ns,bss)
                                      = do  fs <- mapM (mkField a ts ns) ss
                                            return (Binds False [(n,sc)] [(n,foldr ELet (ERec nm fs) bss)])

mkField a ts [from,to] (nm,sc)        = do let nm' = findByNameStr ts (str nm)
                                           e <- expand a from to (EVar nm') sc
                                           return (nm,e)

findByNameStr ts s                  = lookup' [(name0 (str n),n {annot = (annot n) {explicit=False}}) | n <- ts ] (name0 s)

expand a from to e sc                 = exp0 to (e,scheme2Type sc)
  where exp0 f (e,TFun ts t)          = do ns <- newNames paramSym (length ts)
                                           let xs = map EVar ns
                                           es <- mapM (exp0 from) (zip xs ts)
                                           e' <- exp0 to (EAp e es,t)
                                           eLam' ns e'
        exp0 f (e,TId n)
          | n==a                      = return (EAp (EVar f) [e])
        exp0 _ (e,t)                  = expTup (e,tFlat t)
                                             
        expTup (e,(TId (Tuple n _),ts))
                                      = do let n = length ts
                                           ns <- newNames paramSym n
                                           let xs = map EVar ns
                                           es <- mapM (exp0 to) (zip xs ts)
                                           lam <- eLam' ns (etup n es)
                                           return (ECase e [Alt (pCon0 (tuple n)) lam])
        expTup (e,_)                  = return e

eLam' xs e                            = do ts <- mapM (\_ -> newTvar Star) xs
                                           return (ELam (zipWith (\x t -> (x,scheme t)) xs ts) e)
                                           
mkFunPair (nm,DData _ (_:_) _)        = errorIds "Not yet implemented: default instances for data type with subtypes" [nm]
mkFunPair (nm,DData vs ss cs)         = do [from,to] <- mapM newName ["from"++str nm,"to"++str nm]
                                           frhs <- fromRHS
                                           trhs <- toRHS
                                           return ( [from,to], [Binds False [(from,fromType)] [(from,frhs)], Binds False [(to,toType)]  [(to,trhs)]])

  where ot                            = foldr1 tEither (map mkTuple cs)
        dt                            = foldl TAp (TId nm) (map TId vs)
        mkTuple (_,Constr [] _ _)     = tUnit
        mkTuple (_,Constr ss _ _)     = foldr1 (ttup2) (map scheme2Type ss)
        ttup2 t1 t2                   = ttup 2 [t1,t2]
        etup2 e1 e2                   = etup 2 [e1,e2]
        fromType                      = Scheme (F [scheme dt] (R ot)) [] (map (\n -> (n,Star)) vs)
        toType                        = Scheme (F [scheme ot] (R dt)) [] (map (\n -> (n,Star)) vs)

        prefixLR fs e                 = foldr (\f e -> EAp (ECon f) [e]) e (map prim fs)
        mkAlt fs (nm,Constr [] _ _)   = return (Alt (pCon0 nm) (prefixLR fs eUnit))
        mkAlt fs (nm,Constr ss _ _)   = do ns <- newNames tempSym (length ss)
                                           lam <- eLam' ns  (prefixLR fs (foldr1 (etup2) (map EVar ns)))
                                           return (Alt (pCon0 nm) lam)
        alts fs [c]                   = do a <- mkAlt fs c
                                           return [a]
        alts fs (c:cs)                = do a <- mkAlt (fs++[LEFT]) c
                                           as <- alts (fs++[RIGHT]) cs
                                           return (a:as)
        fromRHS                       = do x  <- newName paramSym
                                           as <- alts [] cs
                                           return (ELam [(x,scheme dt)] (ECase (EVar x) as))

        mkCase t@(TAp (TAp (TId (Prim EITHER _)) a) b) (c:cs) (x:xs)
                                      = do [y,z] <- newNames paramSym 2
                                           l <- mkCase a [c] (y:xs)
                                           r <- mkCase b cs (z:xs)
                                           lam1 <- eLam' [y] l
                                           lam2 <- eLam' [z] r
                                           return (ECase (EVar x) [Alt (pCon0 (prim LEFT)) lam1,
                                                                   Alt (pCon0 (prim RIGHT)) lam2])
        mkCase t@(TAp (TAp (TId (Tuple 2 _)) a) b) cs (x:xs)
                                      = do [y,z] <- newNames paramSym 2
                                           r <- mkCase b cs (z:y:xs)
                                           lam <- eLam' [y,z] r
                                           return (ECase (EVar x) [Alt (pCon0 (tuple 2)) lam])
        mkCase t (c:_) xs
          | t == tUnit                = return (ECon c)
          | otherwise                 = return (EAp (ECon c) (map EVar (reverse xs)))
        toRHS                         = do x <- newName paramSym
                                           r <- mkCase ot (map fst cs) [x] 
                                           eLam' [x] r


ttup n ts                             = foldl TAp (TId (tuple n)) ts
etup n es                             = EAp (ECon (tuple n)) es


scheme2Type (Scheme r _ _)            = rho2Type r
rho2Type (R t)                        = t
rho2Type (F ss r)                     = TFun (map scheme2Type ss) (rho2Type r) 

mkShowInstance n sc s (nm, DData vs [] cs) 
                                      = do sh <- mkShow
                                           return (Binds False [(n,sc)] [(n,ERec (fst s) [(n',sh)])])
  where DRec _ _ _ [(n',_)]           = snd s
        mkShow                        = do x <- newName paramSym
                                           as <- mapM mkShowAlt cs
                                           eLam' [x] (ECase (EVar x) as)
        mkShowAlt (nm,Constr [] _ _)  = return (Alt (pCon0 nm) (ELit (LStr Nothing (str nm))))
        mkShowAlt (nm,_)              = errorIds "Sorry, as yet only default Show for enumeration types, without constructors as" [nm]
--        cons                          = ECon (prim CONS)
--        nil                           = ECon (prim NIL)
        chr c                         = ELit (lChr c)
mkShowInstance n sc s (nm, DData vs _ cs) 
                                      = errorIds "Not yet implemented: default Show instance for data type with subtypes" [nm]



mkParseInstance n eq sc s (nm, DData vs [] cs) 
                                      = do p <- mkParse
                                           return (Binds False [(n,sc)] [(n,ERec (fst s) [(n',p)])])
  where DRec _ _ _ [(n',_)]           = snd s
        mkParse                       = do x <- newName paramSym
                                           as <- mapM (mkParseAlt x) cs
                                           eLam' [x] (eMatch (foldr eFatbar (eCommit (EAp (ECon (prim LEFT)) [mkList "no Parse"])) as))
        mkParseAlt x (nm,Constr [] _ _) = return (ECase (EAp eq [EVar x, mkList (str nm)])
                                                  [Alt (pCon0 (prim TRUE)) (eCommit (EAp (ECon (prim RIGHT)) [ECon nm])),Alt PWild eFail])
        mkParseAlt x (nm,_)           = errorIds "Sorry, as yet only default Parse for enumeration types, without constructors as" [nm]
--        cons                          = ECon (prim CONS)
--        nil                           = ECon (prim NIL)
        mkList str                    = ELit (LStr Nothing str)
        chr c                         = ELit (lChr c)
mkParseInstance n eq sc s (nm, DData vs _ cs) 
                                      = errorIds "Not yet implemented: default Parse instance for data type with subtypes" [nm]
