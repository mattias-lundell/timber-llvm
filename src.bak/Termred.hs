{-# LANGUAGE TypeSynonymInstances #-}

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

module Termred(termred, redTerm, isFinite, constrs) where

import Monad
import Common
import Core
import PP
import Char

termred (Module _ _ _ _ ds' _ [bs']) m         = redModule ds' (eqnsOf bs') m

redTerm coercions e             = redExp (initEnv { eqns = coercions }) e

isFinite e                      = finite initEnv e


data Env                        = Env { eqns :: Map Name Exp,
                                        args :: [Name],
                                        cons :: [Map Name Int],
                                        sels :: TEnv
                                      }

initEnv                         = Env { eqns = [], args = [], cons = cons0, sels = [] }

cons0                           = [ [(prim TRUE, 0), (prim FALSE, 0)] , [(prim NIL, 0), (prim CONS, 2)] ]

consOf ds                       = [ map f ce | (_,DData _ _ ce) <- ds ]
  where f (c, Constr te pe _)   = (c, length te + length pe)

selsOf ds                       = concat [ ss | (_,DRec _ _ _ ss) <- ds ]

conArity env (Tuple n _)        = n
conArity env c                  = lookup' (concat (cons env)) c


complete _ [Tuple _ _]          = True
complete _ []                   = False
complete [] cs0                 = False
complete (cs:css) cs0           = all (`elem`cs0) (dom cs)  ||  complete css cs0

addArgs env vs                  = env { args = vs ++ args env }

addEqns env eqs                 = env { eqns = eqs ++ eqns env }

addDecls env ds                 = env { cons = consOf ds ++ cons env, sels = selsOf ds ++ sels env }


redModule impDecls impEqs (Module m ns xs es ds is bss)
                                = do (bss,_) <- redTopBinds env1 bss
                                     return (Module m ns xs es ds is bss)
  where env0                    = addDecls initEnv (tdefsOf impDecls ++ tdefsOf ds)
        env1                    = addEqns env0 (finiteEqns env0 impEqs)


redTopBinds env []              = return ([], [])
redTopBinds env (bs : bss)      = do Binds r te es <- redBinds env bs
                                     (bss,vs) <- redTopBinds (addEqns env (finiteEqns env es)) bss
                                     let necessary (v,_) = r || maybe (elem v vs) (const True) (fromMod v)
                                         te' = filter necessary te
                                         es' = filter necessary es
                                         bs' = Binds r te' es'
                                         bss' = if null te' then bss else bs':bss
                                     return (bss', idents es' ++ vs)
                                     

finiteEqns env eqs              = filter p eqs
  where p (x,e)                 = isSmall e && finite env e


-- can be safely ignored without changing cbv semantics
value (EVar x)                  = x /= prim New
value (ECon _)                  = True
value (ELit _)                  = True
value (ESel e _)                = value e
value (EAp (EVar (Prim IntDiv _)) [e1,e2])
                                = value e1 && nonzero e2
value (EAp (EVar (Prim FloatDiv _)) [e1,e2])
                                = value e1 && nonzero e2
value (EAp (EVar (Prim _ _)) es)
                                = all value es
value (EAp (EVar (Tuple _ _)) es)
                                = all value es
value (EAp (ECon c) es)         = all value es
value (ELam _ _)                = True
value (ERec _ eqs)              = all (value . snd) eqs
value e                         = False

nonzero (ELit (LInt _ n))       = n /= 0
nonzero (ELit (LRat _ n))       = n /= 0
nonzero _                       = False


-- may be safely inlined (can't lead to infinite expansion even if part of a recursive binding group)
finite env (EVar (Prim c _))    = True --c `notElem` [ListArray, UniArray, UpdateArray]
finite env (EVar (Tuple _ _))   = True
finite env (EVar x)             = x `elem` args env || maybe False (finite env) (lookup x (eqns env))
finite env (ECon _)             = True
finite env (ELit _)             = True
finite env (ESel e _)           = finite env e
finite env (ELam te e)          = finite (addArgs env (dom te)) e
finite env (ERec _ eqs)         = all (finite env) (rng eqs)
finite env (EAp e es)           = all (finite env) (e:es)
finite env (ELet bs e)          = fin bs && finite (addArgs env (bvars bs)) e
  where fin (Binds True _ _)    = False
        fin (Binds _ _ eqns)    = all (finite env . snd) eqns
--finite env (ECase e alts)     = finite env e && all (finite env . snd) alts
finite env (ECase e alts)       = finite env e && and [finite env e|Alt p e<-alts]
finite env e                    = False



redBinds env (Binds r te eqns)  = do eqns <- redEqns env eqns
                                     bs <- staticDelayRule env (Binds r te eqns)
                                     return bs


staticDelayRule env bs@(Binds rec te eqs)
  | not rec                     = return bs
  | rec                         = do (eqs',eqs1) <- walkEqs env te (dom eqs) eqs
                                     ts <- mapM (const (newTvar Star)) eqs1
                                     return (Binds rec (te ++ (dom eqs1 `zip` map scheme ts)) (eqs'++eqs1))

      
walkEqs env te fw []            = return ([], [])
walkEqs env te fw (eq:eqs)      = do (eq,eqs1) <- doEq te eq
                                     (eqs,eqs2) <- walkEqs env te (fw \\ [fst eq] ++ dom eqs1) eqs
                                     return (eq:eqs, eqs1++eqs2)
  where doEq te eq@(x,e)
          | null ke             = do (e,eqs) <- doExp e
                                     return ((x,e), eqs)
          | otherwise           = return ((x,e), [])
          where ke              = quant (lookup' te x)
        doAlt (Alt p e)         = do (e,eqs1) <- doExp e
                                     return (Alt p e, eqs1)
        doExp (ESel e l)
          | isCoerceLabel l     = maybeDelay (\e -> ESel e l) e
          | otherwise           = do (e,eqs1) <- doExp e
                                     return (ESel e l, eqs1)
        doExp (ECase e alts)    = do (e,eqs1) <- doExp e
                                     (alts,eqss) <- fmap unzip (mapM doAlt alts)
                                     return (ECase e alts, eqs1++concat eqss)
        doExp (EAp e es)        = do (e,eqs1) <- doExp e
                                     (es,eqss) <- fmap unzip (mapM doExp es)
                                     return (EAp e es, eqs1++concat eqss)
        doExp (ELet (Binds r te eqs) e)
                                = do (eqs',eqss) <- fmap unzip (mapM (doEq te) eqs)
                                     (e,eqs1) <- doExp e
                                     return (ELet (Binds r te eqs') e, eqs1++concat eqss)
        doExp (ERec n eqs)      = do (eqs',eqss) <- fmap unzip (mapM (doEq (sels env)) eqs)
                                     return (ERec n eqs', concat eqss)
        doExp e                 = return (e, [])
        maybeDelay f (ESel e l)
          | isCoerceLabel l     = maybeDelay (\e -> f (ESel e l)) e
        maybeDelay f (EVar x)
          | x `elem` fw         = do y <- newName tempSym
                                     return (EVar y, [(y, f (EVar x))])
        maybeDelay f e          = do (e,eqs1) <- doExp e
                                     return (f e, eqs1)


redEqns env []                  = return []
redEqns env ((x,e):eqns)        = do e' <- redExp env e
                                     let env' = if finite env e' && isSmall e 
                                                then addEqns env [(x,e')]     -- no risk of infinite inlining
                                                else env
                                     liftM ((x,e'):) (redEqns env' eqns)


redExp env (ERec c eqs)         = do es' <- mapM (redExp env) es
                                     return (ERec c (ls `zip` es'))
  where (ls,es)                 = unzip eqs
redExp env (ETempl x t te c)    = liftM (ETempl x t te) (redCmd env c)
redExp env (EAct e e')          = liftM2 EAct (redExp env e) (redExp env e')
redExp env (EReq e e')          = liftM2 EReq (redExp env e) (redExp env e')
redExp env (EDo x t c)          = liftM (EDo x t) (redCmd env c)
redExp env (ELam te e)          = do e <- redExp (addArgs env (dom te)) e
                                     redEta env te e
redExp env (ESel e s)           = do e <- redExp env e
                                     redSel env e s
redExp env (ECase e alts)       = do e <- redExp env e
                                     redCase env e alts
redExp env (ELet bs e)          = do bs'@(Binds rec te eqs) <- redBinds env bs
                                     if rec then
                                        liftM (ELet bs') (redExp env e)
                                      else
                                        redBeta (addArgs env (dom te)) te e (map (lookup' eqs) (dom te))
redExp env e@(EVar (Prim {}))   = return e
redExp env e@(EVar (Tuple {}))  = return e
redExp env e@(EVar x)           = case lookup x (eqns env) of
                                      Just e' | inline e' -> alphaConvert e'
                                      _ -> return e
  where inline (EVar _)         = True
        inline (ECon _)         = True
        inline (ELit _)         = True
        inline (ELam _ _)       = True
        inline _                = isGenerated x
redExp env (EAp e es)           = do e' <- redExp env e
                                     es' <- mapM (redExp env) es
                                     redApp env e' es'
redExp env (ELit l)             = return (ELit (normLit l))
redExp env e                    = return e


normLit (LInt p i)
  | i >= 0x80000000             = normLit (LInt p (i - 0x100000000))
  | i < -0x80000000             = normLit (LInt p (i + 0x100000000))
  | otherwise                   = LInt p i
normLit l                       = l


isRaise (EAp (EVar (Prim Raise _)) [_])
                                = True
isRaise _                       = False

isPMC (EVar (Prim p _))         = p `elem` [Match,Commit,Fatbar,Fail]
isPMC _                         = False


-- reduce an application e es (head and args already individually reduced)
redApp env e es
  | exception                   = return (head es')
  where es'                     = filter isRaise (e:es)
        exception               = not (isPMC e) && not (null es')
redApp env (EVar (Prim p a)) es 
                                = return (redPrim env p a es)
redApp env e@(EVar x) es        = case lookup x (eqns env) of
                                       Just e' | inline e' -> do e' <- alphaConvert e'; redApp env e' es
                                       _ -> return (EAp e es)
  where inline (ELam _ _)       = True
        inline (EVar _)         = True
        inline _                = False
redApp env (ELam te e) es       = do redBeta env te e es
redApp env (ECase e alts) es
  | length alts' == length alts = liftM (ECase e) (redAlts env alts')
  where alts'                   = [ a | Just a <- map (appAlt env es) alts ]
redApp env (ELet bs e) es       = liftM (ELet bs) (redApp env e es)
redApp env e es                 = return (EAp e es)


appAlt env es (Alt(PCon c te) e)= case skipLambda (conArity env c-length te) e es of
                                    Just e' -> Just (Alt (PCon c te) e')
                                    _       -> Nothing
appAlt env es a                 = Just a


skipLambda 0 e es               = Just (EAp e es)
skipLambda n (ELam te e) es
  | n <= length te              = Just (ELam te1 (eLam te2 (EAp e es)))
  where (te1,te2)               = splitAt n te
skipLambda n e es               = Nothing



-- perform beta reduction (if possible)
redBeta env ((x,t):te) (EVar y) (e:es)
  | x == y                      = redBeta env te e es                      -- trivial body
redBeta env ((x,t):te) b (e:es)
  | inline x e                  = do e' <- redBeta (addEqns env [(x,e)]) te b es
                                     return (bindx e')
  | otherwise                   = liftM (ELet bs) (redBeta env te b es)
  where inline x e              = isSafe x || isEVar e || (value e && finite env e && isSmall e)
        isSafe x                = isEtaExp x || isAssumption x || isCoercion x        
        bindx e'
          | x `elem` evars e'   = ELet bs e'
          | otherwise           = e'
        bs                      = Binds False [(x,t)] [(x,e)]
        isEVar (EVar _)         = True
        isEVar _                = False
redBeta env [] b []             = redExp env b


redEta env te (EAp e es)        = do es <- mapM (redExp env') es
                                     e <- redExp env' e
                                     if okEta e && es == map EVar xs then
                                        return e
                                      else do
                                        liftM (ELam te) (redApp env' e es)
  where okEta (ECon _)          = False
        okEta (EVar (Prim _ _)) = False
        okEta e                 = null (evars e `intersect` xs)
        env'                    = addArgs env (dom te)
        xs                      = dom te
redEta env te e                 = liftM (ELam te) (redExp (addArgs env (dom te)) e)


redSel env e s
  | isRaise e                   = return e
redSel env e@(EVar x) s         = case lookup x (eqns env) of
                                    Just e' | inline e' -> do e' <- alphaConvert e'
                                                              redSel env e' s
                                    _ -> return (ESel e s)
  where inline (ERec _ _)       = True
        inline (EVar _)         = True
        inline _                = False
redSel env (ERec c eqs) s
  | all value (rng eqs)         = case lookup s eqs of
                                    Just e  -> return e
                                    Nothing -> internalError0 ("redSel: did not find selector " ++ show s ++ "   in   " ++ show eqs) s
redSel env e s                  = return (ESel e s)


redCase env e alts
  | isRaise e                   = return e
redCase env e@(EVar x) alts     = case lookup x (eqns env) of
                                    Just e' | inline (eFlat e') -> do e' <- alphaConvert e'; redCase env e' alts
                                    _ -> liftM (ECase e) (redAlts env alts)
  where inline (ECon _,_)       = True
        inline (ELit _,_)       = True
        inline (EVar _, [])     = True
        inline _                = False
redCase env (ELit l@(LStr _ _)) alts       
                                =  redCaseStrLit env l alts
redCase env (ELit l) alts       = findLit env l alts
redCase env e alts              = case eFlat e of
                                    (ECon k, es) -> findCon env k es alts
                                    _            -> liftM (ECase e) (redAlts env alts)

redAlts env alts
  | complete (cons env) cs      = do es <- mapM (redRhs env) es
                                     return (zipWith argsToLhs ps es)   -- normally zipAlts
  | otherwise                   = do es0 <- mapM (redRhs env) es0
                                     return (zipWith argsToLhs ps0 es0)  -- normally zipAlts
  where (cs,ps,es)              = unzip3 [ (c,p,e) | Alt p@(PCon c _) e <- alts ]
        (ps0,es0)               = unzipAlts alts
  
redRhs env (ELam te e)          = do e <- redRhs (addArgs env (dom te)) e
                                     return (ELam te e)
redRhs env e                    = redExp env e


findCon env k es (Alt PWild e:_)= redExp env e
findCon env k es (Alt (PCon k' te) e:_)
  | k == k'                     = redExp env (eAp (eLam te e) es)
findCon env k es (_:alts)       = findCon env k es alts


findLit env l (Alt PWild e:_)   = redExp env e
findLit env l (Alt (PLit l') e:_)
  | l == l'                     = redExp env e
findLit env l (_:alts)          = findLit env l alts


redCaseStrLit env l (Alt PWild e:_) = redExp env e
redCaseStrLit env l (Alt (PLit l') e:_)
 | l == l'                      = redExp env e
redCaseStrLit env (LStr _ "") (Alt (PCon (Prim NIL _) []) e:alts) = redExp env e
redCaseStrLit env l@(LStr _ str) alts@(Alt (PCon (Prim CONS _) _) e:_)
                                = redCase env (foldr (\x y -> EAp cons [chr x,y]) nil str) alts
   where chr x = ELit (LChr Nothing x)
         cons = ECon (prim CONS)
         nil = ECon (prim NIL)
redCaseStrLit env l (_:alts)         = redCaseStrLit env l alts


redPrim env Refl _ [e]                      = e
redPrim env Match a [e]                     = redMatch env a e
redPrim env Fatbar a [e,e']                 = redFat a e e'
redPrim env UniArray a es                   = EAp (EVar (Prim UniArray a)) es
redPrim env IntNeg _ [ELit (LInt _ x)]      = ELit (lInt (-x))
redPrim env IntToFloat _ [ELit (LInt _ x)]  = ELit (lRat (fromInteger x))
redPrim env IntToChar _ [ELit (LInt _ x)]   = ELit (lChr (chr (fromInteger x)))
redPrim env FloatNeg _ [ELit (LRat _ x)]    = ELit (lRat (-x))
redPrim env FloatToInt _ [ELit (LRat _ x)]  = ELit (lInt (truncate x))
redPrim env CharToInt _ [ELit (LChr _ x)]   = ELit (lInt (ord x))
redPrim env p a [ELit (LInt _ x), ELit (LInt _ y)]
  | p `notElem` [IntDiv,IntMod] || y /= 0   = redInt p x y
redPrim env p a [ELit (LRat _ x), ELit (LRat _ y)]
  | p /= FloatDiv || y /= 0                 = redRat p x y
redPrim env p a es                          = eAp (EVar (Prim p a)) es


redMatch env a (ELet bs e)                  = ELet bs (redMatch (addArgs env (bvars bs)) a e)
redMatch env a (ELam te e)                  = ELam te (redMatch (addArgs env (dom te)) a e)
redMatch env a (EAp (EVar (Prim Commit _)) [e]) = e
redMatch env a (ECase e alts)               = ECase e [Alt p (redMatch env a e)|Alt p e<-alts]
redMatch env _ (EVar (Prim Fail a))         = EAp (EVar (Prim Raise a)) [ELit (lInt 1)]
redMatch env _ e@(ELit _)                   = e
redMatch env a e                            = EAp (EVar (Prim Match a)) [e]


redFat a (ELet bs e) e'                     = ELet bs (redFat a e e')
redFat a (EVar (Prim Fail _)) e             = e
redFat a e@(EAp (EVar (Prim Commit _)) _) _ = e
redFat a e e'                               = EAp (EVar (Prim Fatbar a)) [e,e']


redInt IntPlus a b              = ELit (normLit (lInt (a + b)))
redInt IntMinus a b             = ELit (normLit (lInt (a - b)))
redInt IntTimes a b             = ELit (normLit (lInt (a * b)))
redInt IntDiv a b               = ELit (lInt (a `div` b))
redInt IntMod a b               = ELit (lInt (a `mod` b))
redInt IntEQ a b                = eBool (a == b)
redInt IntNE a b                = eBool (a /= b)
redInt IntLT a b                = eBool (a < b)
redInt IntLE a b                = eBool (a <= b)
redInt IntGE a b                = eBool (a >= b)
redInt IntGT a b                = eBool (a > b)
redInt p _ _                    = internalError0 ("redInt: " ++ show p)


redRat FloatPlus a b            = ELit (lRat (a + b))
redRat FloatMinus a b           = ELit (lRat (a - b))
redRat FloatTimes a b           = ELit (lRat (a * b))
redRat FloatDiv a b             = ELit (lRat (a / b))
redRat FloatEQ a b              = eBool (a == b)
redRat FloatNE a b              = eBool (a /= b)
redRat FloatLT a b              = eBool (a < b)
redRat FloatLE a b              = eBool (a <= b)
redRat FloatGE a b              = eBool (a >= b)
redRat FloatGT a b              = eBool (a > b)
redRat p _ _                    = internalError0 ("redRat: " ++ show p)


eBool True                      = ECon (prim TRUE)
eBool False                     = ECon (prim FALSE)


redCmd env (CRet e)             = liftM CRet (redExp env e)
redCmd env (CExp e)             = liftM CExp (redExp env e)
redCmd env (CGen p t (ELet bs e) c)
                                = redCmd env (CLet bs (CGen p t e c))
redCmd env (CGen p t e c)       = liftM2 (CGen p t) (redExp env e) (redCmd env c)
redCmd env (CLet bs c)          = do bs'@(Binds rec te eqs) <- redBinds env bs
                                     if rec then
                                        liftM (CLet bs') (redCmd env c)
                                      else
                                        redBetaC (addArgs env (dom te)) te c (map (lookup' eqs) (dom te))
redCmd env (CAss x e c)         = liftM2 (CAss x) (redExp env e) (redCmd env c)


-- perform beta reduction (if possible)
redBetaC env ((x,t):te) (CRet (EVar y)) (e:es)
  | x == y                      = redBetaC env te (CRet e) es
redBetaC env ((x,t):te) c (e:es)
  | inline x e                  = do c' <- redBetaC (addEqns env [(x,e)]) te c es
                                     return (bindx c')
  | otherwise                   = liftM (CLet bs) (redBetaC env te c es)
  where inline x e              = isSafe x || isSafeEVar e || (value e && finite env e && isSmall e)
        isSafe x                = isEtaExp x || isAssumption x || isCoercion x        
        bindx c'
          | x `elem` evars c'   = CLet bs c'
          | otherwise           = c'
        bs                      = Binds False [(x,t)] [(x,e)]
        isSafeEVar (EVar n)     = not $ isState n
        isSafeEVar _            = False
redBetaC env [] c []            = redCmd env c

-- Constructor presence

isSmall e                       = length (constrs e) < 5

class Constrs a where
    constrs :: a -> [Name]

instance Constrs Binds where
    constrs (Binds rec te eqns) = constrs (map snd eqns)

instance Constrs a => Constrs [a] where
    constrs xs = concatMap constrs xs

instance Constrs Exp where
    constrs (ECon c)             = [c]
    constrs (ESel e l)           = constrs e
    constrs (ELam te e)          = constrs e
    constrs (EAp e e')           = constrs e ++ constrs e'
    constrs (ELet bs e)          = constrs bs ++ constrs e
    constrs (ECase e alts)       = constrs e ++ constrs alts
    constrs (ERec c eqs)         = constrs (map snd eqs)
    constrs (EAct e e')          = constrs e ++ constrs e'
    constrs (EReq e e')          = constrs e ++ constrs e'
    constrs (ETempl x t te c)    = constrs c
    constrs (EDo x t c)          = constrs c
    constrs _                    = []

instance Constrs e => Constrs (Alt e) where
    constrs (Alt p e)            = constrs e


instance Constrs Cmd where
    constrs (CLet bs c)          = constrs bs ++ constrs c
    constrs (CGen x t e c)       = constrs e ++ constrs c
    constrs (CAss x e c)         = constrs e ++ constrs c
    constrs (CRet e)             = constrs e
    constrs (CExp e)             = constrs e

