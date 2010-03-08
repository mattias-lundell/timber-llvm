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

module Rename where

{-

This module does the following:
  - Checks all binding groups and patterns for duplicates (both types & terms).
  - Checks all signatures for duplicates and dangling references (kind and type sigs).
  - Completes all type signatures and constructor definitions with explicit type variable bindings.
  - Shuffles explicit signatures as close as possible to their binding occurrences (even inside patterns)
  - Checks that variables bound in generators and equations in commands do not shadow state variables.
  - Checks that assignments are to declared state variables only.
  - Resolves proper binding occurrences for all identifier references by setting the unique tag in each Name 
    identifier, or possibly replacing a Name with a Prim.
-}

import Monad
import Common
import Syntax
import Depend
import PP
import List(sort)

renameM e1 m                       = rename (initEnv e1) m
                                

-- Syntax traversal environment --------------------------------------------------

data Env                           = Env { rE :: Map Name Name, 
                                           rT :: Map Name Name, 
                                           rS :: Map Name Name,
                                           rL :: Map Name Name,
                                           self :: [Name],
                                           void :: [Name]
                                         } deriving Show

initEnv (rL',rT',rE')              = Env { rE = primTerms ++ rE', rT = primTypes ++ rT', rS = [], rL = primSels ++ rL', self = [], void = [] }

stateVars env                      = dom (rS env)

tscope env                         = dom (rT env)


renE env n@(Tuple _ _)             = n
renE env n@(Prim _ _)              = n
renE env v                         = case lookup v (rE env) of
                                       Just n  -> n { annot = (annot v) {suppress = suppress (annot n), public = public (annot n)} }
                                       Nothing -> checkQualError v (rE env)


   where ms = [m | Name s t (Just m) _ <- dom (rE env), s == str v]
         checkQualError v rE =  if fromMod v==Nothing && length ms > 1
                                then errorIds ("Variable not in scope; imported from several modules (" ++ concat(intersperse ", " ms) ++")") [v]
                                else errorIds "Undefined identifier" [v]
                                
renS env n@(Tuple _ _)             = n
renS env n@(Prim _ _)              = n
renS env v                         = case lookup v (rS env) of
                                       Just n  -> n { annot = a { stateVar = True} }
                                       Nothing -> errorIds "Undefined state variable" [v]
  where a                          = annot v

renL env v                         = case lookup v (rL env) of
                                       Just n  -> n { annot = (annot v){suppress = suppress (annot n), public = public (annot n)} }
                                       Nothing -> errorIds "Undefined selector" [v]

renT env n@(Tuple _ _)             = n
renT env n@(Prim _ _)              = n
renT env v                         = case lookup v (rT env) of
                                       Just n  -> n { annot = (annot v) {suppress = suppress (annot n), public = public (annot n)} }
                                       Nothing -> errorIds "Undefined type identifier" [v]

extRenE env vs
  | not (null shadowed)            = errorIds "Illegal shadowing of state variables" shadowed
  | not (null shadowed')           = errorIds "Illegal shadowing of state reference" shadowed'
  | otherwise                      = do rE' <- renaming (noDups "Duplicate variables" (legalBind vs))
                                        return (env { rE = rE' ++ rE env })
  where shadowed                   = intersect vs (stateVars env)
        shadowed'                  = intersect vs (self env)


setRenS env vs
  | not (null shadowed)            = errorIds "Illegal shadowing of state reference" shadowed
  | otherwise                      = do rS' <- renaming (noDups "Duplicate state variables" (legalBind vs))
                                        return (env { rS = rS', void = vs })
  where shadowed                   = intersect vs (self env)

unvoid vs env                      = env { void = void env \\ vs }

unvoidAll env                      = env { void = [] }

extRenT env vs                     = do rT' <- renaming (noDups "Duplicate type variables" (legalBind vs))
                                        return (env { rT = rT' ++ rT env })

extRenEMod _ _ env []              = return env
extRenEMod pub m env vs            = do rE' <- extRenXMod pub m (rE env) vs
                                        return (env {rE = rE'})

extRenTMod _ _ env []              = return env
extRenTMod pub m env vs            = do rT' <- extRenXMod pub m (rT env) vs
                                        return (env {rT = rT'})

extRenLMod _ _ env []              = return env
extRenLMod pub m env vs            = do rL' <- extRenXMod pub m (rL env) vs
                                        return (env {rL = rL'})

extRenSelf env s                   = do rE' <- renaming (legalBind [s])
                                        return (env { rE = rE' ++ rE env, self = [s] })

extRenXMod pub m rX vs             = do rX1 <- renaming (map (qName (str m)) (legalBind vs))
                                        let rX2 = map suppressPair rX1
                                        return  (mergeRenamings1 (map dropModFst rX2) rX ++ rX2)
  where suppressPair (n1,n2)       = (n1, n2 {annot = (annot n2) {suppress = True, public = pub}}) 
        dropModFst (n1,n2)         = (dropMod n1,n2)

legalBind vs                        = map checkName vs
  where checkName v@(Tuple _ _)	    = errorIds "Tuple constructors may not be redefined" [v]
        checkName v@(Prim _ _)	    = errorIds "Rigid symbol may not be redefined" [v]
        checkName v
          | fromMod v == Nothing    = v
          | otherwise               = errorIds "Binding occurrence may not be qualified" [v]


-- Binding of overloaded names ------------------------------------------------------------------------

oloadBinds xs ds                    = f te
  where te                          = [ (s,t') | DRec True c vs _ sels <- ds, let p = PType (foldl TAp (TCon c) (map TVar vs)),
                                                 Sig ss t <- sels, s <- ss, s `notElem` xs, let t' = tQual [p] t ]
        f []                        = []
        f (sig:te)                  = mkSig sig : mkEqn sig : f te
        mkSig (s,t)                 = BSig [s] t
        mkEqn (s,TQual t ps)        = BEqn (LFun s' (map PVar (w:ws))) (RExp (foldl EAp (ESelect (EVar w) s') (map EVar ws)))
          where w:ws                = take (length ps) abcSupply
                s'                  = annotExplicit s


-- Renaming -----------------------------------------------------------------------------------------

class Rename a where
  rename :: Env -> a -> M s a

instance Rename a => Rename [a] where
  rename env as                    = mapM (rename env) as

instance Rename a => Rename (Maybe a) where
  rename env (Just e)              = liftM Just (rename env e)
  rename env Nothing               = return Nothing


instance Rename Module where
  rename env (Module c is ds ps)   = do assert (null kDups) "Duplicate kind signatures" kDups
                                        assert (null tDups) "Duplicate type constructors" tDups
                                        assert (null sDups) "Duplicate selectors" sDups
                                        assert (null cDups) "Duplicate constructors" cDups
                                        assert (null wDups) "Duplicate instance declarations" wDups
                                        assert (null eDups) "Duplicate top-level variable" eDups
                                        assert (null tcDups) "Duplicate typeclass declaration" tcDups
                                        assert (null dks)   "Dangling kind signatures" dks
                                        assert (null dws1)  "Dangling instance declarations" dws1
                                        assert (null dws2)  "Dangling instance declarations" dws2
                                        assert (null dtcs1) "Dangling typeclass declarations" dtcs1
                                        assert (null dtcs2) "Dangling typeclass declarations" dtcs2
                                        assert (null badImpl) "Illegal type signature for instance" badImpl
                                        env1 <- extRenTMod True  c env  (ts1 ++ ks1')
                                        env2 <- extRenEMod True  c env1 (cs1 ++ vs1 ++ vs1' ++ vss++is1++ bvars es1)
                                        env3 <- extRenLMod True  c env2 ss1
                                        env4 <- extRenTMod False c env3 ((ts2 \\ ks1') ++ ks2)
                                        env5 <- extRenEMod False c env4 (cs2 ++ (vs2 \\ vss) ++ vs2'++is2++bvars es2)
                                        env6 <- extRenLMod False c env5 ss2
                                        -- tr (concatMap (\(n1,n2) -> render (pr n1) ++ "," ++ render (pr n2) ++ "   ") (rE env6))
                                        ds <- rename env6 (ds1 ++ ps1)
                                        bs <- rename env6 (bs1' ++ bs2' ++ shuffleB (bs1 ++ bs2))
                                        return (Module c is (ds ++ map DBind (groupBindsS bs)) [])
   where (ks1,ts1,ss1,cs1,ws1,tcs1,is1,es1,bss1) = renameD [] [] [] [] [] [] [] [] [] ds
         (ks2,ts2,ss2,cs2,ws2,tcs2,is2,es2,bss2) = renameD [] [] [] [] [] [] [] [] [] ps
         ds1                       = mergeTClasses tcs1 ds
         ps1                       = mergeTClasses tcs2 ps
         vs1                       = concat (map bvars bss1)
         vs2                       = concat (map bvars bss2)
         bs1                       = concat (reverse bss1)
         bs2                       = concat (reverse bss2)
         vss                       = concat [ ss | BSig ss _ <- bs1 ] \\ vs1
         vs                        = vs1 ++ vs2
         ks                        = ks1 ++ ks2
         ts                        = ts1 ++ ts2
         ws                        = ws1 ++ ws2
         bs1'                      = oloadBinds vs ds1
         bs2'                      = oloadBinds vs ps1
         vs1'                      = bvars bs1'
         vs2'                      = bvars bs2'
         ks1'                      = ks1 \\ ts1
         dks                       = ks \\ ts
         dws1                      = ws1 \\ concat [ ss | BSig ss _ <- bs1 ]
         dws2                      = ws2 \\ concat [ ss | BSig ss _ <- bs2 ]
         dtcs1                     = tcs1 \\ [ n | DRec False n _ _ _ <- ds ]
         dtcs2                     = tcs2 \\ [ n | DRec False n _ _ _ <- ps ]
         badImpl                   = concat [ ws | BSig ss t <- bs1++bs2, not (null (ss `intersect` ws)), isWild t ]
         kDups                     = duplicates ks
         tDups                     = duplicates ts
         sDups                     = duplicates (ss1 ++ ss2)
         cDups                     = duplicates (cs1 ++ cs2)
         wDups                     = duplicates ws
         eDups                     = duplicates vs
         tcDups                    = duplicates (tcs1++tcs2)
         isWild (TQual t ps)       = isWild t || any isWild [ t | PType t <- ps ]
         isWild (TAp t t')         = isWild t || isWild t'
         isWild (TSub t t')        = isWild t || isWild t'
         isWild (TList t)          = isWild t
         isWild (TTup ts)          = any isWild ts
         isWild (TFun ts t)        = any isWild (t:ts)
         isWild TWild              = True
         isWild _                  = False
         
mergeTClasses tcs (DRec isC n vs ts ss : ds) = DRec (isC || elem n tcs) n vs ts ss : mergeTClasses tcs ds
mergeTClasses tcs (DTClass _ : ds) = mergeTClasses tcs ds
mergeTClasses tcs (DBind _ : ds) = mergeTClasses tcs ds
mergeTClasses tcs (d : ds) = d : mergeTClasses tcs ds
mergeTClasses _ [] = []

renameD ks ts ss cs ws tcs is es bss (DKSig c k : ds)
                                   = renameD (c:ks) ts ss cs ws tcs is es bss ds
renameD ks ts ss cs ws tcs is es bss (DRec _ c _ _ sigs : ds)
                                   = renameD ks (c:ts) (sels++ss) cs ws tcs is es bss ds
  where sels                       = concat [ vs | Sig vs t <- sigs ]
renameD ks ts ss cs ws tcs is es bss (DData c _ _ cdefs : ds)
                                   = renameD ks (c:ts) ss (cons++cs) ws tcs is es bss ds
  where cons                       = [ c | Constr c _ _ <- cdefs ]
renameD ks ts ss cs ws tcs is es bss (DType c _ _ : ds)
                                   = renameD ks (c:ts) ss cs ws tcs is es bss ds
renameD ks ts ss cs ws tcs is es bss (DInstance vs : ds)
                                   = renameD ks ts ss cs (ws++vs) tcs is es bss ds
renameD ks ts ss cs ws tcs is es bss (DTClass vs : ds)
                                   = renameD ks ts ss cs ws (tcs++vs) is es bss ds
renameD ks ts ss cs ws tcs is es bss (DDefault ps : ds)
                                   = renameD ks ts ss cs ws tcs (is1 ++ is) es bss ds
  where is1                        = [i | Derive i _ <- ps]
renameD ks ts ss cs ws tcs is es bss (DBind bs : ds)
                                   = renameD ks ts ss cs ws tcs is es (bs : bss) ds
renameD ks ts ss cs ws tcs is es bss (DExtern es' : ds)
                                   = renameD ks ts ss cs ws tcs is (es ++ es') bss ds
renameD ks ts ss cs ws tcs is es bss []
                                   = (ks, ts, ss, cs, ws, tcs, is, es, bss)


instance Rename Decl where
  rename env d@(DKSig _ _)         = return d
  rename env (DData c vs ts cs)    = do env' <- extRenT env vs
                                        liftM2 (DData (renT env c) (map (renT env') vs)) (renameQTs env' ts) (rename env' cs)
  rename env (DRec isC c vs ts ss) = do env' <- extRenT env vs
                                        liftM2 (DRec isC (renT env c) (map (renT env') vs)) (renameQTs env' ts) (rename env' ss)
  rename env (DType c vs t)        = do env' <- extRenT env vs
                                        liftM (DType (renT env c) (map (renT env') vs)) (rename env' t)
  rename env (DInstance vs)        = return (DInstance (map (renE env) vs))
  rename env (DDefault ts)         = liftM DDefault (rename env ts)
  rename env (DBind bs)            = liftM DBind (rename env bs)
  rename env (DExtern es)          = liftM DExtern (rename env es)

instance Rename (Default Type) where
  rename env (Default pub a b)     = return (Default pub (renE env a) (renE env b))
  rename env (Derive v t)          = liftM (Derive (renE env v)) (renameQT env t)

instance Rename Constr where
  rename env (Constr c ts ps)      = do env' <- extRenT env (bvars ps')
                                        liftM2 (Constr (renE env c)) (rename env' ts) (rename env' ps')
   where ps'                       = completeP env ts ps

instance Rename (Extern Type) where
  rename env (Extern n t)          = liftM (Extern (renE env n)) (renameQT env t)
completeP env t ps
  | not (null dups)                = errorIds "Duplicate type variable abstractions" dups
  | not (null dang)                = errorIds "Dangling type variable abstractions" dang
  | otherwise                      = ps ++ zipWith PKind implicit (repeat KWild)
  where vs                         = tyvars t ++ tyvars ps
        bvs                        = bvars ps
        dups                       = duplicates bvs
        dang                       = bvs \\ vs
        implicit                   = nub vs \\ (tscope env ++ bvs)


renameQT env (TQual t ps)          = rename env (TQual t (completeP env t ps))
renameQT env t
  | null ps                        = rename env t
  | otherwise                      = rename env (TQual t ps)
  where ps                         = completeP env t []


renameQTs env ts                   = mapM (renameQT env) ts


instance Rename Sig where
  rename env (Sig vs t)            = liftM (Sig (map (renL env) vs)) (renameQT env t)

instance Rename Pred where
  rename env (PType p)             = liftM PType (rename env p)
  rename env (PKind v k)           = return (PKind (renT env v) k)

instance Rename Type where
  rename env (TQual t ps)          = do env' <- extRenT env (bvars ps)
                                        liftM2 TQual (rename env' t) (rename env' ps)
  rename env (TCon c)              = return (TCon (renT env c))
  rename env (TVar v)              = return (TVar (renT env v))
  rename env (TAp t1 t2)           = liftM2 TAp (rename env t1) (rename env t2)
  rename env (TSub t1 t2)          = liftM2 TSub (rename env t1) (rename env t2)
  rename env TWild                 = return TWild
  rename env (TList ts)            = liftM TList (rename env ts)
  rename env (TTup ts)             = liftM TTup (rename env ts)
  rename env (TFun t1 t2)          = liftM2 TFun (rename env t1) (rename env t2)

instance Rename Bind where
  rename env (BEqn (LFun v ps) rh) = do env' <- extRenE env (pvars ps)
                                        ps' <- rename env' ps
                                        liftM (BEqn (LFun (renE env v) ps')) (rename env' rh)
  rename env (BEqn (LPat p) rh)    = do p' <- rename env p
                                        liftM (BEqn (LPat p')) (rename env rh)
  rename env (BSig vs t)           = liftM (BSig (map (renE env) vs)) (renameQT env t)

instance Rename Pat where
  rename env (PVar v)
    | v `elem` void env            = errorIds"Uninitialized state variable" [v]
    | v `elem` stateVars env       = return (PVar (renS env v))
    | otherwise                    = return (PVar (renE env v))
  rename env (PCon c)              = return (PCon (renE env c))
  rename env (PAp e1 e2)           = liftM2 PAp (rename env e1) (rename env e2)
  rename env (PLit l)              = return (PLit l)
  rename env (PTup ps)             = liftM PTup (rename env ps)
  rename env (PList es)            = liftM PList (rename env es)
  rename env PWild                 = return PWild
  rename env (PSig e t)            = liftM2 PSig (rename env e) (renameQT env t)
  rename env (PRec m fs)           = liftM (PRec (renRec env m)) (rename env fs)

instance Rename Exp where
  rename env (EVar v)
    | v `elem` void env            = errorIds"Uninitialized state variable" [v]
    | v `elem` stateVars env       = return (EVar (renS env v))
    | otherwise                    = return (EVar (renE env v))
  rename env (ECon c)              = return (ECon (renE env c))
  rename env (ESel l)              = return (ESel (renL env l))
  rename env (EAp e1 e2)           = liftM2 EAp (rename env e1) (rename env e2)
  rename env (ELit l)              = return (ELit l)
  rename env (ETup ps)             = liftM ETup (rename env ps)
  rename env (EList es)            = liftM EList (rename env es)
  rename env EWild                 = return EWild
  rename env (ESig e t)            = liftM2 ESig (rename env e) (renameQT env t)
  rename env (ERec m fs)           = liftM (ERec (renRec env m)) (rename env fs)
  rename env (ELam ps e)           = do env' <- extRenE env (pvars ps)
                                        liftM2 ELam (rename env' ps) (rename env' e)
  rename env (ELet bs e)           = do env' <- extRenE env (bvars bs)
                                        bs' <- rename env' (shuffleB bs)
                                        e' <- rename env' e
                                        return (foldr ELet e' (groupBindsS bs'))
  rename env (ECase e as)          = liftM2 ECase (rename env e) (rename env as)
  rename env (EMatch m)            = liftM EMatch (rename env m)
  rename env (EIf e1 e2 e3)        = liftM3 EIf (rename env e1) (rename env e2) (rename env e3)
  rename env (ENeg e)              = liftM ENeg (rename env e)
  rename env (ESeq e1 e2 e3)       = liftM3 ESeq (rename env e1) (rename env e2) (rename env e3)
  rename env (EComp e qs)          = do (qs,e) <- renameQ env qs e
                                        return (EComp e qs)
  rename env (ESectR e op)         = liftM (flip ESectR (renE env op)) (rename env e) 
  rename env (ESectL op e)         = liftM (ESectL (renE env op)) (rename env e)
  rename env (ESelect e l)         = liftM (flip ESelect (renL env l)) (rename env e) 
  rename env (EAct v ss)           = liftM (EAct (fmap (renE env) v)) (rename (unvoidAll env) (shuffleS ss))
  rename env (EReq v ss)           = liftM (EReq (fmap (renE env) v)) (rename (unvoidAll env) (shuffleS ss))
  rename env (EDo (Just v) Nothing ss)
                                   = do env1 <- extRenSelf env v
                                        liftM (EDo (Just (renE env1 v)) Nothing) (rename (unvoidAll env1) (shuffleS ss))
  rename env (ETempl (Just v) Nothing ss)
                                   = do env1 <- extRenSelf env v
                                        env2 <- setRenS env1 st
                                        liftM (ETempl (Just (renE env2 v)) Nothing) (rename env2 (shuffleS ss))
    where st                       = assignedVars ss
  rename env (EAfter e1 e2)        = liftM2 EAfter (rename env e1) (rename env e2)
  rename env (EBefore e1 e2)       = liftM2 EBefore (rename env e1) (rename env e2)
  rename env e@(EBStruct (Just c) ls bs)
                                   = do let ls' = map (dropMod . annotGenerated) ls
                                        env' <- extRenE env ls' 
                                        r <- rename env' (ERec (Just (c,True)) (map (\(s,s') -> Field s (EVar s')) (ls `zip` ls')))
                                        bs' <- mapM (renSBind env' env) bs
                                        return (foldr ELet r (groupBindsS bs'))
  rename env (EForall qs ss)       = do (qs,ss) <- renameQ env qs ss
                                        return (EForall qs ss)
  rename env e                     = internalError "rename did not expect: " e

renRec env (Just (n, t))           = Just (renT env n, t)
renRec env Nothing                 = Nothing

renSBind envL envR (BEqn (LFun v ps) rh)    = do envR' <- extRenE envR (pvars ps)
                                                 ps'   <- rename envR' ps
                                                 liftM (BEqn (LFun (annotGenerated (renE envL v)) ps')) (rename envR' rh)
--renSBind envL envR (BEqn (LPat (EVar v)) rh)= liftM (BEqn (LPat (EVar (annotGenerated (renE envL v))))) (rename envR rh)
renSBind envL envR (BEqn (LPat p) rh)       = do p' <- rename envL p
                                                 rh' <- rename envR rh
                                                 return (BEqn (LPat p') rh')
renSBind _ _ s@(BSig vs t)                  = errorTree "Signature in struct value" s 

instance Rename r => Rename (Match Pat Exp [Bind] r) where
  rename env = mapMMatch (rename env) (rename env) (rename env) (rename env)

instance Rename a => Rename (Field a) where
  rename env (Field l e)           = liftM (Field (renL env l)) (rename env e)

instance Rename a => Rename (Rhs a) where
  rename env (RExp e)              = liftM RExp (rename env e)
  rename env (RGrd gs)             = liftM RGrd (rename env gs)
  rename env (RWhere e bs)         = do env' <- extRenE env (bvars bs)
                                        bs' <- rename env' (shuffleB bs)
                                        e' <- rename env' e
                                        return (foldr (flip RWhere) e' (groupBindsS bs'))


instance Rename a => Rename (GExp a) where
  rename env (GExp qs e)           = do (qs,e) <- renameQ env qs e
                                        return (GExp qs e)


renameQ env [] e0                  = do e0 <- rename env e0
                                        return ([], e0)
renameQ env (QExp e : qs) e0       = do e <- rename env e
                                        (qs,e0) <- renameQ env qs e0
                                        return (QExp e : qs, e0)
renameQ env (QGen p e : qs) e0     = do e <- rename env e
                                        env' <- extRenE env (pvars p)
                                        p <- rename env' p
                                        (qs,e0) <- renameQ env' qs e0
                                        return (QGen p e : qs, e0)
renameQ env (QLet bs : qs) e0      = do env' <- extRenE env (bvars bs)
                                        bs <- rename env' bs
                                        (qs,e0) <- renameQ env' qs e0
                                        return (map QLet (groupBindsS bs) ++ qs, e0)


instance Rename a => Rename (Alt a) where
  rename env (Alt  p rh)           = do env' <- extRenE env (pvars p)
                                        liftM2 Alt (rename env' p) (rename env' rh) 

instance Rename Stmts where
  rename env (Stmts ss) = fmap Stmts (renameS env ss)
    where
      renameS env []               = return []
      renameS env [SRet e]         = liftM (:[]) (liftM SRet (rename env e))
      renameS env (SExp e : ss)    = liftM2 (:) (liftM SExp (rename env e)) (renameS env ss)
      renameS env (SGen p e : ss)  = do env' <- extRenE env (pvars p)
                                        liftM2 (:) (liftM2 SGen (rename env' p) (rename env e)) (renameS env' ss)
      renameS env (SBind bs : ss)  = do env' <- extRenE env (bvars bs)
                                        bs' <- rename env' bs
                                        ss' <- renameS env' ss
                                        return (map SBind (groupBindsS bs') ++ ss')
      renameS env (SAss p e : ss)
        | not (null illegal)       = errorIds "Unknown state variables" illegal
        | otherwise                = liftM2 (:) (liftM2 SAss (rename (unvoidAll env) p) (rename env e)) (renameS (unvoid (pvars p) env) ss)
        where illegal              = pvars p \\ stateVars env
      renameS env (SCase e as:ss)  = do e' <- rename env e
                                        as' <- rename env as
                                        ss' <- renameS env ss
                                        return (SCase e' as':ss')
      renameS env (SMatch m:ss)    = liftM2 ((:) .SMatch) (rename env m) (renameS env ss)
      renameS env s                = internalError0 ("renameS "++show s)


-- Signature shuffling -------------------------------------------------------------------------

shuffleB bs                        = shuffle [] bs


shuffle [] []                      = []
shuffle sigs []                    = errorIds "Dangling type signatures" (dom sigs)
shuffle sigs (BSig vs t : bs)
  | not (null s_dups)              = errorIds "Duplicate type signatures" s_dups
  | otherwise                      = shuffle (vs `zip` repeat t ++ sigs) bs
  where s_dups                     = duplicates vs ++ (vs `intersect` dom sigs)
shuffle sigs (b@(BEqn (LFun v _) _) : bs)
                                   = case lookup v sigs of
                                       Just t  -> BSig [v] t : b : shuffle (prune sigs [v]) bs
                                       Nothing -> b : shuffle sigs bs
shuffle sigs (BEqn (LPat p) rh : bs)
                                   = BEqn (LPat p') rh : shuffle sigs' bs
  where (sigs',p')                 = attach sigs p



shuffleS (Stmts ss)                = Stmts (shuffle' [] ss)

shuffle' [] []                     = []
shuffle' sigs []                   = errorIds "Dangling type signatures for" (dom sigs)
shuffle' sigs (SBind bs : ss)      = SBind (shuffle sigs1 bs1) : shuffle' ([ (v,t) | BSig [v] t <- bs2 ] ++ sigs2) ss
  where (sigs1,sigs2)              = partition ((`elem` vs) . fst) sigs
        (bs1,bs2)                  = partition local (concatMap flat bs)
        vs                         = bvars bs
        local (BSig [v] _)         = v `elem` vs
        local _                    = True
        flat (BSig vs t)           = [ BSig [v] t | v <- vs ]
        flat b                     = [b]
shuffle' sigs (SGen p e : ss)      = SGen p' e : shuffle' sigs' ss
  where (sigs',p')                 = attach sigs p
shuffle' sigs (SAss p e : ss)      = SAss p' e : shuffle' sigs2 ss
  where (sigs',p')                 = attach sigs1 p
        (sigs1,sigs2)              = partition ((`elem` vs) . fst) sigs
        vs                         = evars p
shuffle' sigs (s : ss)             = s : shuffle' sigs ss



attach sigs e@(PSig (PVar v) t)
  | v `elem` dom sigs              = errorIds "Conflicting signatures for" [v]
  | otherwise                      = (sigs, e)
attach sigs (PVar v)               = case lookup v sigs of
                                       Nothing -> (sigs, PVar v)
                                       Just t  -> (prune sigs [v], PSig (PVar v) t)
attach sigs (PAp p1 p2)            = (sigs2, PAp p1' p2')
  where (sigs1,p1')                = attach sigs p1
        (sigs2,p2')                = attach sigs1 p2
attach sigs (PTup ps)              = (sigs', PTup ps')
  where (sigs',ps')                = attachList sigs ps
attach sigs (PList ps)             = (sigs', PList ps')
  where (sigs',ps')                = attachList sigs ps
attach sigs p                      = (sigs, p)

attachList sigs []                 = (sigs, [])
attachList sigs (p:ps)             = (sigs2, p':ps')
  where (sigs1,p')                 = attach sigs p
        (sigs2,ps')                = attachList sigs1 ps

