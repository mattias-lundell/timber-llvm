{-# LANGUAGE FlexibleInstances #-}

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

module Desugar1 where

import List(sort, sortBy)
import Monad
import Common hiding (isPublic)
import Syntax
import Depend
import PP

desugar1 e0 (Module c is ds ps)  = do let (env,expInfo) = mkEnv c (ds ++ ps) e0
                                          ds' = dsDecls env ds
                                          ps' = dsDecls env {isPublic = False} ps
                                      return (Module c is ds' ps',expInfo)

{-
    This module performs desugaring transformations that must be performed before the renaming pass:
    - Implements Record Stuffing; i.e. replacing ".." in record patterns and expressions 
      by sequences of bindings of the form "x=x" for missing record fields x.  
    - Adds type tag to anonymous record expressions/patterns.
    - Makes the "self" variable explicit.
    - Checks validity of result statement
    - Replaces prefix expression statement with dummy generator statement
    - Replaces if/case statements with corresponding expressions forms
    - Checks the restricted command syntax of template bodies
    - Unfolds use of type synonyms
-}

-- The selector environment --------------------------------------------------------------------------------------

data Env                        = Env { sels :: Map Name [Name], 
                                        self :: Maybe Name , 
                                        selSubst :: Map Name Name,
                                        modName :: Maybe String,
                                      --isPat :: Bool,
                                        isPublic :: Bool,
                                        tsyns :: Map Name ([Name],Type)
                                  } deriving Show


env0 ss                       = Env { sels = [], self = Nothing, selSubst = [], modName = Nothing,
                                    --isPat = False, 
                                      isPublic = True, tsyns = ss }


mkEnv c ds (rs,rn,ss)           = (env {sels = map transClose recEnv, modName = Just(str c), selSubst = rnSels },
                                   (map (\(n,ss) -> (qName (str c) n,ss)) closeRecEnvLoc,tsynsLoc))
  where (env,ssLoc)             = tsynE (env0 ss) [] tsynDecls
        tsynsLoc                = map (\(n,y) -> (n {fromMod = Just (str c)},y)) ssLoc
        recEnvLoc               = [ (c,(map type2head ts, concat (map sels ss))) | DRec _ c _ ts ss <- ds ] 
        recEnvImp               = zip ns (zip (repeat []) ss) where (ns,ss) = unzip rs
        closeRecEnvLoc          = map transClose recEnvLoc
        closeRecEnvImp          = map transClose recEnvImp
        recEnv                  = recEnvLoc ++ recEnvImp
        selsLocQual             = concatMap (snd . snd) recEnvLoc
        selsLoc                 = map dropMod selsLocQual
        rnSels                  = zip selsLoc selsLocQual ++ zip selsLocQual selsLocQual ++ rn
        sels (Sig vs _)         = map (qName (str c)) vs
        transClose (c,(cs,ss))  = (c, sort (ss ++ nub(concat (map (selectors [c]) cs))))
        selectors cs0 c
          | c `elem` cs0        = errorIds "Circular struct dependencies" (c:cs0)
          | otherwise           = case lookup c recEnv of
                                    Just (cs,ss) -> ss ++ concat (map (selectors (c:cs0)) cs)
                                    Nothing      -> errorIds "Unknown struct constructor"  [c]

        tsynDecls                
          | not (null dups)     = errorIds "Duplicate type synonym declarations" dups
          | otherwise           = case topSort (tyCons . snd) syns of
                                    Left ns     -> errorIds "Mutually recursive type synonyms" ns
                                    Right syns' -> syns'
        syns                    = [(c,(vs,t)) | DType c vs t <- ds]
        dups                    = duplicates (map fst syns)


tsynE env ls []                 = (env,ls)
tsynE env ls ((c,(vs,t)) : ps) 
    | c `elem` tyCons t         = errorIds "Type synonym is recursive" [c]
    | otherwise                 = tsynE env {tsyns =p : tsyns env} (p:ls) ps
 where p                        =  (c,(vs,ds1 env t))

selsFromType env c              = case lookup c (sels env) of
                                    Just ss -> ss
                                    Nothing -> if fromMod c == modName env
                                               then selsFromType env (c {fromMod = Nothing})
                                               else errorIds "Unknown struct constructor" [c]


typeFromSels env ss             = case [ c | (c,ss') <- sels env, ss' == ss ] of
                                    []  -> errorIds "No struct type with selectors" ss
                                    [c] -> c
                                    cs  -> case filter isQualified cs of
                                             [c] -> c
                                             _   -> errorIds ("Multiple struct types with same selectors") cs


haveSelf env                    = self env /= Nothing

tSubst env c ts                 = case lookup c (tsyns env) of
                                     Nothing -> foldl TAp (TCon c) ts1
                                     Just (vs,t)
                                       | length vs > length ts -> errorIds "Type synonym not fully applied" [c]
                                       | otherwise -> foldl TAp (subst (zip vs (take (length vs) ts1)) t) 
                                                                (drop (length vs) ts1)
  where ts1                     = ds1 env ts

ren env cs                      = map ren' cs     
  where ren' c                  = case lookup c (selSubst env) of
                                     Nothing -> errorIds "Unknown struct selector" [c]
                                     Just c' -> c'

-- Check that patEnv is only used with patterns before removing the isPat flag (transitional)
ds1pat  env p                   = ds1 (patEnv env) (p::Pat)
ds1pats env ps                  = ds1 (patEnv env) (ps::[Pat])
patEnv env                      = env -- {isPat = True}

sortFields fs                   = sortBy cmp fs
  where cmp (Field l1 _) (Field l2 _) = compare (str l1) (str l2)

-- Desugaring -------------------------------------------------------------------------------------------

dsDecls env (DType c vs t : ds) = DType c vs (ds1 env t) : dsDecls env ds
dsDecls env (DData c vs ss cs : ds) = DData c vs (ds1 env ss) (ds1 env cs) : dsDecls env ds
dsDecls env (DRec b c vs ss ss' : ds) = DRec b c vs (ds1 env ss) (ds1 env ss') :dsDecls env ds
dsDecls env (DPSig n t : ds)    = DInstance [n] : DBind [BSig [n] (ds1 env t)] : dsDecls env ds
dsDecls env (DDefault ts : ds)  = DDefault (ds1 env ts) : dsDecls env ds
dsDecls env ds@(DBind _ : _)    = dsDs [] ds
  where dsDs bs (DBind bs':ds)  = dsDs (bs++bs') ds
        dsDs bs ds              = DBind (ds1 env bs) : dsDecls env ds
dsDecls env (DExtern es : ds)   = DExtern (ds1 env es) : dsDecls env ds
dsDecls env (d : ds)            = d : dsDecls env ds
dsDecls env []                  = []


class Desugar1 a where
    ds1 :: Env -> a -> a

instance Desugar1 a => Desugar1 [a] where
    ds1 env                     = map (ds1 env)

instance Desugar1 a => Desugar1 (Maybe a) where
    ds1 env Nothing             = Nothing
    ds1 env (Just a)            = Just (ds1 env a)

instance Desugar1 (Default Type) where
   ds1 env (Default _ a b)      = Default (isPublic env) a b
   ds1 env (Derive v t)         = Derive v (ds1 env t)

instance Desugar1 Constr where
    ds1 env (Constr c ts ps)    = Constr c (ds1 env ts) (ds1 env ps)

instance Desugar1 Sig where
    ds1 env (Sig vs t)          = Sig vs (ds1 env t)

instance Desugar1 Type where
    ds1 env t@(TAp t1 t2)       = case tFlat t of
                                     (TCon c,ts) -> tSubst env c ts
                                     _ -> TAp (ds1 env t1) (ds1 env t2)
    ds1 env (TQual t ps)        = TQual (ds1 env t) (ds1 env ps)
    ds1 env (TSub t1 t2)        = TSub (ds1 env t1) (ds1 env t2)
    ds1 env (TList t)           = TList (ds1 env t)
    ds1 env (TTup ts)           = TTup (ds1 env ts)
    ds1 env (TFun ts t)         = TFun (ds1 env ts) (ds1 env t)
    ds1 env (TCon c)            = tSubst env c []
    ds1 env t                   = t

instance Desugar1 (Extern Type) where
    ds1 env (Extern n t)        = Extern n (ds1 env t)

instance Desugar1 Pred where
    ds1 env (PType t)           = PType (ds1 env t)
    ds1 env p                   = p
instance Desugar1 Decl where
    ds1 env (DBind bs)          = DBind (ds1 env bs)
    ds1 env d                   = d

instance Desugar1 Bind where
    ds1 env (BEqn lh rh)        = BEqn (ds1 env lh) (ds1 env rh)
    ds1 env (BSig vs t)         = BSig vs (ds1 env t)

instance Desugar1 Lhs where
    ds1 env (LFun v ps)         = LFun v (ds1pats env ps)
    ds1 env (LPat (PVar x))     = LFun x []
    ds1 env (LPat p)            = LPat (ds1pat env p)

instance Desugar1 a => Desugar1 (Rhs a) where
    ds1 env (RExp e)            = RExp (ds1 env e)
    ds1 env (RGrd gs)           = RGrd (ds1 env gs)
    ds1 env (RWhere e bs)       = RWhere (ds1 env e) (ds1 env bs)

instance Desugar1 a => Desugar1 (GExp a) where
    ds1 env (GExp qs e)         = GExp (ds1 env qs) (ds1 env e)

instance Desugar1 Qual where
    ds1 env (QExp e)            = QExp (ds1 env e)
    ds1 env (QGen p e)          = QGen (ds1pat env p) (ds1 env e)
    ds1 env (QLet bs)           = QLet (ds1 env bs)


instance Desugar1 Pat where
    ds1 env (PRec Nothing fs)
      | not (null dups)            = errorIds "Duplicate field definitions in struct" dups
      | otherwise                  = PRec (Just (c,True)) (sortFields (ds1 env fs))
      where c                      = typeFromSels env (sort (ren env sels))
            dups                   = duplicates sels
            sels                   = bvars fs
    ds1 env (PRec (Just (c,all)) fs)
      | not (null dups)            = errorIds "Duplicate field definitions in struct" dups
      | all && not (null miss)     = errorIds "Missing selectors in struct" miss
      | otherwise                  = PRec (Just (c,True)) (sortFields (fs' ++ ds1 env fs))
      where miss                   = ren env (selsFromType env c) \\ ren env (bvars fs)
            dups                   = duplicates (ren env (bvars fs))
            fs'                    = map (\s -> Field (tag0 (mkLocal s)) (PVar (dropMod (tag0 s)))) miss
            mkLocal s
              | fromMod s == modName env = dropMod s
              | otherwise          = s
    ds1 env (PAp e1 e2)            = PAp (ds1 env e1) (ds1 env e2)
    ds1 env (PTup es)              = PTup (ds1 env es)
    ds1 env (PList es)             = PList (ds1 env es)
    ds1 env (PSig e t)             = PSig (ds1 env e) (ds1 env t)
    ds1 env e                    = e

instance Desugar1 Exp where
--    ds1 env e@(EVar _)             = e
    ds1 env (ERec Nothing fs)
      | not (null dups)            = errorIds "Duplicate field definitions in struct" dups
      | otherwise                  = ERec (Just (c,True)) (sortFields (ds1 env fs))
      where c                      = typeFromSels env (sort (ren env sels))
            dups                   = duplicates sels
            sels                   = bvars fs
    ds1 env (ERec (Just (c,all)) fs)
      | not (null dups)            = errorIds "Duplicate field definitions in struct" dups
      | all && not (null miss)     = errorIds "Missing selectors in struct" miss
      | otherwise                  = ERec (Just (c,True)) (sortFields (fs' ++ ds1 env fs))
      where miss                   = ren env (selsFromType env c) \\ ren env (bvars fs)
            dups                   = duplicates (ren env (bvars fs))
            fs'                    = map (\s -> Field (tag0 (mkLocal s)) (EVar (dropMod (tag0 s)))) miss
            mkLocal s
              | fromMod s == modName env = dropMod s
              | otherwise          = s
    ds1 env (EBStruct c0 _ bs)     = EBStruct (Just c) labs (ds1 env bs)
      where labs                   = selsFromType env c
            c                      = case c0 of Just c -> c; _ -> typeFromSels env (sort (ren env sels))
            sels0                  = bvars bs
            cs                     = concat [ stuffedCons p | BEqn (LPat p) _ <- bs ]
            sels1                  = concatMap (boundSels env) cs \\ sels0
            sels                   = sels0 ++ sels1
            boundSels env (c,ls)   = selsFromType env c \\ ren env ls
    ds1 env (ELet bs e)            = ELet (ds1 env bs) (ds1 env e)
    ds1 env (EAp e1 e2)            = EAp (ds1 env e1) (ds1 env e2)
    ds1 env (ETup es)              = ETup (ds1 env es)
    ds1 env (EList es)             = EList (ds1 env es)
    ds1 env (ESig e t)             = ESig (ds1 env e) (ds1 env t)
    ds1 env (ELam ps e)            = ELam (ds1pats env ps) (ds1 env e)
    ds1 env (ECase e as)           = ECase (ds1 env e) (ds1 env as)
    ds1 env (EMatch m)             = EMatch (ds1 env m)
    ds1 env (EIf e1 e2 e3)         = EIf (ds1 env e1) (ds1 env e2) (ds1 env e3)
    ds1 env (ENeg (ELit (LInt p i))) = ELit (LInt p (-i))
    ds1 env (ENeg (ELit (LRat p r))) = ELit (LRat p (-r))
    ds1 env (ENeg e)               = EAp (EVar (name' "negate" e)) (ds1 env e)
    ds1 env (ESeq e1 Nothing e3)   = EAp (EAp (EVar (name' "enumFromTo" e1)) (ds1 env e1)) (ds1 env e3)
    ds1 env (ESeq e1 (Just e2) e3) = EAp (EAp (EAp (EVar (name' "enumFromThenTo" e1)) (ds1 env e1)) (ds1 env e2)) (ds1 env e3)
    ds1 env (EComp e qs)           = EComp (ds1 env e) (ds1 env qs)
    ds1 env (ESectR e op)          = ESectR (ds1 env e) op
    ds1 env (ESectL op e)          = ESectL op (ds1 env e)
    ds1 env (ESelect e s)          = ESelect (ds1 env e) s
    ds1 env e@(ELit (LInt _ n))
  {-  | isPat env                  = e
      | otherwise               -} = EAp (EVar (name' "fromInt" e)) e 

    ds1 env (ETempl Nothing t ss)  = ds1 env (ETempl (Just (name0 "self")) (ds1 env t) ss)
    ds1 env (EDo Nothing t ss)
      | haveSelf env               = ds1 env (EDo (self env) (ds1 env t) ss)
      | otherwise                  = ds1 env (EDo (Just (name0 "self")) (ds1 env t) ss)
    ds1 env e@(EAct Nothing ss)
      | haveSelf env               = ds1 env (EAct (self env) ss)
      | otherwise                  = errorTree "Action outside class" e
    ds1 env e@(EReq Nothing ss)
      | haveSelf env               = ds1 env (EReq (self env) ss)
      | otherwise                  = errorTree "Request outside class" e

    ds1 env (ETempl v t (Stmts []))= errorTree "Class with empty statement list" (ETempl Nothing t (Stmts []))
    ds1 env (ETempl v t ss)      = ETempl v t (ds1Templ (env{self=v}) [] [] ss)
    ds1 env (EDo v t ss)         = EDo v t (ds1 (env { self=v }) ss)
    ds1 env (EAct v ss)          = EAct v (Stmts [SExp (EDo v Nothing (ds1 env ss))])
    ds1 env (EReq v ss)          = EReq v (Stmts [SExp (EDo v Nothing (ds1 env ss))])

    ds1 env (EAfter e1 e2)       = EAfter (ds1 env e1) (ds1 env e2)
    ds1 env (EBefore e1 e2)      = EBefore (ds1 env e1) (ds1 env e2)
    ds1 env (EForall qs ss)      = ds1Forall False env qs ss

    ds1 env e                    = e

stuffedCons (PRec (Just (c,False)) fs)
                                 = (c,bvars fs) : concat [ stuffedCons p | Field _ p <- fs ]
stuffedCons (PTup ps)            = concatMap stuffedCons ps
stuffedCons (PList ps)           = concatMap stuffedCons ps
stuffedCons (PAp p p')           = stuffedCons p ++ stuffedCons p'
stuffedCons _                    = []

instance Desugar1 r => Desugar1 (Match Pat Exp [Bind] r) where
    ds1 env = mapMatch (ds1 env) (ds1 env) (ds1 env) (ds1 env)

instance Desugar1 a => Desugar1 (Alt a) where
    ds1 env (Alt p rh)           = Alt (ds1pat env p) (ds1 env rh)

instance Desugar1 a => Desugar1 (Field a) where
    ds1 env (Field l e)          = Field l (ds1 env e)

instance Desugar1 Stmts where
    ds1 env (Stmts ss) = Stmts (ds1S env ss)
      where
        ds1S env []                  = [SRet unit]
        ds1S env [SRet e]            = [SRet (ds1 env e)]
      --ds1S env [SExp e]            = [SExp (ds1 env e)]
        ds1S env (SExp (EForall qs ss') : ss) =
                                       SGen PWild (ds1Forall True env qs ss') : ds1S env ss 
        ds1S env (SExp e : ss)       = SGen PWild (ds1 env e) : ds1S env ss
        ds1S env (s@(SRet _) : ss)   = errorTree "Result statement must be last in sequence" s
        ds1S env (SGen p e : ss)     = SGen (ds1pat env p) (ds1 env e) : ds1S env ss
        ds1S env ss@(SBind _ : _)    = dsBs [] ss
          where dsBs bs (SBind bs' : ss) = dsBs (bs++bs') ss
                dsBs bs ss               = SBind (ds1 env bs) : ds1S env ss
        ds1S env (SAss p e : ss)     = dsAss p e : ds1S env ss
          where dsAss (PAp (PAp (PVar (Prim IndexArray _)) a) i) e
                                     = dsAss a (EAp (EAp (EAp (EVar (prim UpdateArray)) (pat2exp a)) (pat2exp i)) e)
                dsAss p e            = SAss (ds1 env p) (ds1 env e)
                {-
                    a|x|y|z := e
                    a|x|y   := a|x|y \\ (z,e)
                    a|x     := a|x \\ (y, a|x|y \\ (z,e))
                    a       := a \\ (x, a|x \\ (y, a|x|y \\ (z,e)))
                -}
        -- New: keep SCase in tail position
        --ds1S env [SCase e as]        = [SCase (ds1 env e) (ds1 env as)]
        --(Old) eliminate SCase in other places
        ds1S env (SCase e as : ss)   = ds1S' env (SExp (ECase e (map doAlt as)) : ss)
          where doAlt (Alt p r)      = Alt (ds1pat env p) (doRhs r)
                doRhs (RExp ss)      = RExp (eDo env ss)
                doRhs (RGrd gs)      = RGrd (map doGrd gs)
                doRhs (RWhere r bs)  = RWhere (doRhs r) bs
                doGrd (GExp qs ss)   = GExp qs (eDo env ss)
        ds1S env (SMatch m : ss)     = SMatch (ds1 env m) : ds1S env ss
        ds1S env (SIf e ss' : ss)    = doIf (EIf e (eDo env ss')) ss
          where doIf f (SElsif e ss':ss) = doIf (f . EIf e (eDo env ss')) ss
                doIf f (SElse ss':ss)    = ds1S' env (SExp (f (eDo env ss')) : ss)
                doIf f ss                = doIf f (SElse (Stmts []) : ss)
        ds1S env (s@(SElsif _ _) : _)= errorTree "elsif without corresponding if" s
        ds1S env (s@(SElse _) : _)   = errorTree "else without corresponding if" s

        ds1S' env [SExp e]           = [SExp (ds1 env e)]
        ds1S' env ss                 = ds1S env ss

-- We translate forall constructions to calls of Prelude support functions. Unfortunately, we
-- need six such functions for efficiency:
-- forallList for the general case
-- forallSeq when the list in the generator is a sequence [a..b] (to avoid building a list)
-- forallSeq1 when the list in the generator is a sequence [a,c..b] (to avoid building a list)
-- All three come in a variant with name suffix Unit when they execute for side effect only (to allow for 
-- tail call elimination).

ds1Forall _ env [] ss              = eDo env ss --(ds1S env ss)
ds1Forall _ env (QLet bs : qs) ss  = ds1 env (ELet bs (EForall qs ss))
ds1Forall b env (QGen p (ESeq e1 Nothing e3) : qs) ss
                                   = eFAp env (name' (forallSeq b) p) p (ds1Forall b env qs ss) [e1,e3]
ds1Forall b env (QGen p (ESeq e1 (Just e2) e3) : qs) ss
                                   = eFAp env (name' (forallSeq1 b) p) p (ds1Forall b env qs ss) [e1,e2,e3]
ds1Forall b env (QGen p e : qs) ss = eFAp env (name' (forallList b) p) p (ds1Forall b env qs ss) [e]
ds1Forall b env (QExp e : qs) ss   = EIf (ds1 env e) (eDo env (Stmts [])) (ds1Forall b env qs ss)

ds1Templ env bss asg (Stmts ss)  = Stmts (ds1T env bss asg ss)
  where
    ds1T env []  asg [SRet e]        = reverse asg ++ [SRet (ds1 env e)]
    ds1T env bss asg [SRet e]        = SBind (ds1 env (concat (reverse bss))) : reverse asg ++ [SRet (ds1 env e)]
    ds1T env bss asg [s]             = errorTree "Last statement in class must be result, not" s
    ds1T env bss asg (s@(SRet _):_)  = errorTree "Result statement must be last in sequence" s
    ds1T env bss asg (SBind bs : ss) = ds1T env (bs:bss) asg ss
    ds1T env bss asg (SAss p e : ss) = ds1T env bss (SAss (ds1pat env p) (ds1 env e) : asg) ss
    ds1T env bss asg (SGen (PVar x) e : ss)
       | isGenerated x               = SGen (PVar x)  (ds1 env e) : ds1T env bss asg ss
    ds1T env bss asg (s : _)         = errorTree "Illegal statement in class: " s

eFAp env n p e es                = eAp (EAp (EVar n) (ELam [ds1 env p] e)) (map (ds1 env) es)

eDo env ss                       = ds1 env (EDo Nothing Nothing ss)

name' s  e                       = Name s 0 Nothing (noAnnot {location = loc (posInfo e)})
  where loc Unknown              = Nothing
        loc (Between p _)        = Just p


forallSeq True = "forallSeqUnit"
forallSeq False = "forallSeq"

forallSeq1 True = "forallSeq1Unit"
forallSeq1 False = "forallSeq1"

forallList True = "forallListUnit"
forallList False = "forallList"




-- Printing --------

instance Pr (Module,([(Name, [Name])], [(Name, ([Name], Type))])) where
 
   pr (m,_) = pr m
