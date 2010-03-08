{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts,
             GeneralizedNewtypeDeriving #-}

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

module Syntax where

import Common
import Lexer
import PP
import Data.Binary
import Monad(liftM2)

data Module = Module Name [Import] [Decl] [Decl]
            deriving  (Show)

data Import = Import Bool Name
            deriving (Show)
            
data Decl   = DKSig   Name Kind
            | DData   Name [Name] [Type] [Constr]
            | DRec    Bool Name [Name] [Type] [Sig]
            | DType   Name [Name] Type   -- removed by desugaring
--            | DInst   Type [Bind]        --          -"-
            | DPSig   Name Type 
            | DDefault [Default Type]  
            | DInstance [Name]
            | DTClass [Name]
            | DBind   [Bind]
            | DExtern [Extern Type]
            deriving  (Eq,Show)

data Constr =  Constr Name [Type] [Pred]
            deriving (Eq,Show)

data Sig    = Sig [Name] Type
            deriving (Eq,Show)

data Bind   = BSig    [Name] Type
            | BEqn    Lhs (Rhs Exp)
            deriving  (Eq,Show)


type Eqn    = (Lhs, Rhs Exp)

data Type   = TQual   Type [Pred]
            | TCon    Name
            | TVar    Name
            | TAp     Type Type
            | TSub    Type Type
            | TWild
            | TList   Type
            | TTup    [Type]
            | TFun    [Type] Type
            deriving  (Eq,Show)

data Pred   = PType Type
            | PKind   Name Kind
            deriving (Eq,Show)
            
data Lhs    = LFun Name [Pat]
            | LPat Pat
            deriving (Eq,Show)

data Pat    = PVar    Name
            | PAp     Pat Pat
            | PCon    Name
            | PLit    Lit
        --  | PNeg    Lit -- only numeric literals can be negated
            | PTup    [Pat]
            | PList   [Pat]
            | PWild
            | PSig    Pat Type
            | PRec    (Maybe (Name,Bool)) [PField]
            deriving  (Eq,Show)

data Exp    = EVar    Name
            | EAp     Exp Exp
            | ECon    Name
            | ESel    Name
            | ELit    Lit
            | ETup    [Exp]
            | EList   [Exp]
            | EWild
            | ESig    Exp Type
            | ERec    (Maybe (Name,Bool)) [EField]
           -- pattern syntax ends here
            | EBStruct (Maybe Name) [Name] [Bind]  -- struct value in bindlist syntax
            | ELam    [Pat] Exp
            | ELet    [Bind] Exp
           -- the following and ETup, EList removed in desugaring
            | ECase   Exp [Alt Exp]
            | EIf     Exp Exp Exp
            | ENeg    Exp
            | ESeq    Exp (Maybe Exp) Exp
            | EComp   Exp [Qual]
            | ESectR  Exp Name
            | ESectL  Name Exp
            | ESelect Exp Name
            | EDo     (Maybe Name) (Maybe Type) Stmts 
            | ETempl  (Maybe Name) (Maybe Type) Stmts
            | EAct    (Maybe Name) Stmts 
            | EReq    (Maybe Name) Stmts 
            | EAfter  Exp Exp
            | EBefore Exp Exp
            | EForall [Qual] Stmts
           -- the following is introduced in desugaring
            | EMatch  (Match Pat Exp [Bind] Exp)
            deriving  (Eq,Show)

data Field a = Field   Name a
             deriving (Eq,Show)

type EField = Field Exp
type PField = Field Pat
    
data Rhs a  = RExp    a
            | RGrd    [GExp a]
            | RWhere  (Rhs a) [Bind]
            deriving  (Eq,Show)

data GExp a = GExp    [Qual] a
            deriving (Eq,Show)
            
data Alt a  = Alt     Pat (Rhs a)
            deriving  (Eq,Show)

data Qual   = QExp    Exp
            | QGen    Pat Exp
            | QLet    [Bind]
            deriving  (Eq,Show)

newtype Stmts = Stmts [Stmt]
              deriving (Eq,Show,Binary,HasPos)

data Stmt   = SExp    Exp
            | SRet    Exp
            | SGen    Pat Exp
            | SBind   [Bind]
            | SAss    Pat Exp
            | SIf     Exp Stmts
            | SElsif  Exp Stmts
            | SElse   Stmts
            | SCase   Exp [Alt Stmts]
           -- the following is introduced in desugaring
            | SMatch  (Match Pat Exp [Bind] Stmts)
            deriving  (Eq,Show)


-- Primitives ----------------------------------------------------------------

cons x xs                       = EAp (EAp (ECon (prim CONS)) x) xs
nil                             = ECon (prim NIL)
true                            = ECon (prim TRUE)
false                           = ECon (prim FALSE)
unit                            = ECon (tuple 0)

conP c ps                       = foldl PAp (PCon c) ps

consP x xs                      = conP (prim CONS) [x,xs]
nilP                            = PCon (prim NIL)
trueP                           = PCon (prim TRUE)
falseP                          = PCon (prim FALSE)
unitP                           = PCon (tuple 0)

-- Helper functions ----------------------------------------------------------------

imports c is 
      | str c == "Prelude"      = []
      | otherwise               = Import True (name0 "Prelude") : is

mkModule c (is,ds,ps)           = Module c (imports c is) ds ps

newEVarPos v p                  = fmap EVar (newNamePos v p)
newPVarPos v p                  = fmap PVar (newNamePos v p)

op2exp c                        = if isCon c then ECon c else EVar c

isEVar (EVar _)                 = True
isEVar (EWild)                  = True
isEVar _                        = False

isPVar (PVar _)                 = True
isPVar (PWild)                  = True
isPVar _                        = False

isWildPat (PWild)               = True
isWildPat (PVar x)              = isDummy x
isWildPat _                     = False

isEConApp (EAp e es)            = isEConApp e
isEConApp (ECon _)              = True
isEConApp _                     = False

isPConApp (PAp e es)            = isPConApp e
isPConApp (PCon _)              = True
isPConApp _                     = False

isELit (ELit _)                 = True
isELit _                        = False

isPLit (PLit _)                 = True
isPLit _                        = False

isERec (ERec _ _)               = True
isERec _                        = False

isPRec (PRec _ _)               = True
isPRec _                        = False

isESigVar (ESig e _)            = isEVar e
isESigVar e                     = isEVar e

isPSigVar (PSig e _)            = isPVar e
isPSigVar e                     = isPVar e

isETup (ETup _)                 = True
isETup _                        = False

isPTup (PTup _)                 = True
isPTup _                        = False

isSAss (SAss _ _)               = True
isSAss _                        = False

isSGen (SGen _ _)               = True
isSGen _                        = False

isSBind (SBind _)               = True
isSBind _                       = False

isDBind (DBind _)               = True
isDBind _                       = False

isBSig (BSig _ _)               = True
isBSig _                        = False

isPKind (PKind _ _)             = True
isPKind _                       = False

tAp isDType (DType _ _ _)       = True
isDType _                       = False

isLPatEqn (BEqn (LPat _) _)     = True
isLPatEqn _                     = False

tupSize (ETup es)               = length es
tupSize _                       = -1

ptupSize (PTup es)              = length es
ptupSize _                      = -1


eFlat e                         = flat e []
  where flat (EAp e e') es      = flat e (e':es)
        flat e es               = (e,es)

pFlat e                         = flat e []
  where flat (PAp e e') es      = flat e (e':es)
        flat e es               = (e,es)

eLam [] e                       = e
eLam ps (ELam ps' e)            = ELam (ps++ps') e
eLam ps e                       = ELam ps e

eAp                             = foldl EAp

rAp (RExp e) es                 = RExp (eAp e es)
rAp (RWhere rhs bs) es          = RWhere (rAp rhs es) bs
rAp (RGrd gs) es                = RGrd [ GExp qs (eAp e es) | GExp qs e <- gs ]


eLet [] e                       = e
eLet bs e                       = ELet bs e

simpleEqn x e                   = BEqn (LFun x []) (RExp e)


var2lhs v                       = LPat (PVar v)

exp2lhs e                       = case eFlat e of
                                       (EVar v, ps)
                                         |not(null ps) -> LFun v (map exp2pat ps)
                                       _               -> LPat (exp2pat e)

exp2pat :: Exp -> Pat
exp2pat e                       = case e of
                                    EVar x        -> PVar x
                                    EAp p1 p2     -> PAp (exp2pat p1) (exp2pat p2)
                                    ECon c        -> PCon c
                                    ELit l        -> PLit l
                                    ENeg (ELit (LInt p i)) -> PLit (LInt p (-i))
                                    ENeg (ELit (LRat p r)) -> PLit (LRat p (-r))
                                    ETup ps       -> PTup (map exp2pat ps)
                                    EList ps      -> PList (map exp2pat ps)
                                    EWild         -> PWild -- hmm
                                    ESig p t      -> PSig (exp2pat p) t
                                    ERec m fs     -> PRec m [Field l (exp2pat p)|Field l p<-fs]
                                    _             -> errorTree "Illegal pattern" e

pat2exp :: Pat -> Exp
pat2exp p                       = case p of
                                    PVar x    -> EVar x
                                    PAp p1 p2 -> EAp (pat2exp p1) (pat2exp p2)
                                    PCon c    -> ECon c
                                    PLit l    -> ELit l
                                    PTup ps   -> ETup (map pat2exp ps)
                                    PList ps  -> EList (map pat2exp ps)
                                    PWild     -> EWild -- hmm
                                    PSig p t  -> ESig (pat2exp p) t
                                    PRec m fs -> ERec m [Field l (pat2exp p)|Field l p<-fs]
                   
tFun [] t                       = t
tFun ts t                       = TFun ts t

tQual ps (TQual t ps')          = TQual t (ps++ps')
tQual ps t                      = TQual t ps

tFlat t                         = flat t []
  where flat (TAp t1 t2) ts     = flat t1 (t2:ts)
        flat t ts               = (t,ts)


type2cons (TQual t ps)          = case tFlat t of
                                    (TCon c,ts) -> Constr c ts ps
                                    _           -> errorTree "Bad type constructor in" t
type2cons t                     = case tFlat t of
                                    (TCon c,ts) -> Constr c ts []
                                    _           -> errorTree "Bad type constructor in" t

type2head (TQual t ps)          = type2head t
type2head t                     = case tFlat t of
                                    (TCon c,ts) -> c
                                    _           -> errorTree "Bad instance head in" t

stripSigs (ESig p _)            = p
stripSigs p                     = p


tyCons :: Type -> [Name]
tyCons (TQual t ps)             = nub (tyCons t ++ concatMap tC ps)
   where tC (PType t)           = tyCons t
         tC _                   = []
tyCons (TCon c)                 = [c]
tyCons (TVar _)                 = []
tyCons (TAp t1 t2)              = nub (tyCons t1 ++ tyCons t2)
tyCons (TSub t1 t2)             = nub (tyCons t1 ++ tyCons t2)
tyCons TWild                    = []
tyCons (TList t)                = tyCons t
tyCons (TTup ts)                = nub (concatMap tyCons ts)
tyCons (TFun ts t)              = nub (concatMap tyCons (t : ts))

{-
forallClass qs e = class 
                    os <- forall x <- xs do
                             o = new e
                             result o
                    result os

We cannot generate unique names here, but this is only called before renaming.
-}

forallClass qs e                = ETempl Nothing Nothing
                                         (Stmts [ SGen osp (EForall qs 
                                                            (Stmts [SBind [BEqn (LPat op) (RExp (EAp (EVar (prim New)) e))], 
                                                                    SRet oe])),
                                                  SRet ose ])
                                    where o  = name0 "___x"
                                          os = name0 "___xs"
                                          oe  = EVar o
                                          ose = EVar os
                                          op  = PVar o
                                          osp = PVar os

-- Checking syntax of statement lists ----------------------------------------

checkStmts :: Stmts -> Stmts
checkStmts ss
  | isResult ss                 = ss
  | otherwise                   = ss
  where
    isResult (Stmts ss)             = isRes ss

    isRes []                    = False
  --isRes [SExp e]              = tr' ("######## Suspicious tail: " ++ render (pr e)) False
    isRes (SRet e : ss)
      | null ss                 = True
      | otherwise               = errorTree "Illegal continuation after result" (Stmts ss)
    isRes (s@(SCase e alts) : ss)
      | and rs && null ss       = True
      | and rs                  = errorTree "Illegal continuation after final 'case'" (Stmts ss)
      | all not rs              = isRes ss
      | otherwise               = errorTree "Inconsistent alternatives in 'case'" s
      where rs                  = map isResult (concat [ cmds rh | Alt p rh <- alts ])
            cmds (RExp c)       = [c]
            cmds (RGrd gs)      = [ c | GExp qs c <- gs ]
            cmds (RWhere rh bs) = cmds rh
    isRes (s@(SIf e c) : ss)
      | and rs && null ss2      = True
      | and rs                  = errorTree "Illegal continuation after final 'if'" (Stmts ss2)
      | all not rs              = isRes ss2
      | otherwise               = errorTree "Inconsistent branches in 'if'" (Stmts (s:ss1))
      where (ss1,ss2)           = ifTail ss
            rs                  = map isResult (c : map branch ss1)
            branch (SElsif e c) = c
            branch (SElse c)    = c
    isRes (s@(SElsif e c) : ss) = errorTree "'elsif' without preceeding 'if'" s
    isRes (s@(SElse c) : ss)    = errorTree "'else' without preceeding 'if'" s
 -- isRes (s@(SForall q c) : ss)
 --   | isRes c                 = errorTree "Illegal result in 'forall' body" c
 --   | otherwise               = isRes ss
    isRes (s : ss)              = isRes ss


ifTail (SElsif e s : ss)        = (SElsif e s : ss1, ss2)
  where (ss1,ss2)               = ifTail ss
ifTail (SElse s : ss)           = ([SElse s], ss)
ifTail ss                       = ([], ss)


checkClass ss@(Stmts sss)
  | okClass sss                 = ss
  | otherwise                   = errorTree "Missing result in class" ss

okClass []                      = False
okClass (SRet e : ss)
  | null ss                     = True
  | otherwise                   = errorTree "Illegal continuation after result" (Stmts ss)
okClass (SBind bs : ss)         = okClass ss
okClass (SAss p e : ss)         = okClass ss
okClass (s : ss)                = errorTree "Illegal command in class" s


-- Substitution --------------------------------------------------------------

instance Subst Bind Name Exp where
    subst s (BSig xs t)         = BSig xs t
    subst s (BEqn lhs rhs)      = BEqn lhs (subst s rhs)


instance Subst Pat Name Pat where
    subst s (PVar x)            = case lookup x s of
                                    Just e  -> e
                                    Nothing -> PVar x
    subst s (PAp e e')          = PAp (subst s e) (subst s e')
    subst s (PCon c)            = PCon c
    subst s (PLit l)            = PLit l
    subst s (PTup es)           = PTup (subst s es)
    subst s (PList es)          = PList (subst s es)
    subst s (PWild)             = PWild
    subst s (PSig e t)          = PSig (subst s e) t
    subst s (PRec m fs)         = PRec m (subst s fs)

instance Subst Exp Name Exp where
    subst s (EVar x)            = case lookup x s of
                                    Just e  -> e
                                    Nothing -> EVar x
    subst s (EAp e e')          = EAp (subst s e) (subst s e')
    subst s (ECon c)            = ECon c
    subst s (ELit l)            = ELit l
    subst s (ETup es)           = ETup (subst s es)
    subst s (EList es)          = EList (subst s es)
    subst s (EWild)             = EWild
    subst s (ESig e t)          = ESig (subst s e) t
    subst s (ELam ps e)         = ELam ps (subst s e)
    subst s (ELet bs e)         = ELet (subst s bs) (subst s e)
    subst s (ECase e alts)      = ECase (subst s e) (subst s alts)
    subst s (EMatch m)          = EMatch (subst s m)
    subst s (ERec m fs)         = ERec m (subst s fs)
    subst s (EBStruct c ls bs)  = EBStruct c ls (subst s bs)
    subst s (EIf e1 e2 e3)      = EIf (subst s e1) (subst s e2) (subst s e3)
    subst s (ENeg e)            = ENeg (subst s e)
    subst s (ESeq e1 Nothing e2)  = ESeq (subst s e1) Nothing (subst s e2)
    subst s (ESeq e1 (Just e) e2) = ESeq (subst s e1) (Just (subst s e)) (subst s e2)
    subst s (EComp e qs)        = EComp (subst s e) (subst s qs)
    subst s (ESectR e op)       = ESectR (subst s e) op
    subst s (ESectL op e)       = ESectL op (subst s e)
    subst s (ESelect e l)       = ESelect (subst s e) l
    subst s (ESel l)            = ESel l
    subst s (EDo v t st)        = EDo v t (subst s st)
    subst s (ETempl v t st)     = ETempl v t (subst s st) 
    subst s (EAct v st)         = EAct v (subst s st)
    subst s (EReq v st)         = EReq v (subst s st)
    subst s (EAfter e e')       = EAfter (subst s e) (subst s e')
    subst s (EBefore e e')      = EBefore (subst s e) (subst s e')
    subst s (EForall qs st)     = EForall (subst s qs) (subst s st)

instance Subst a Name a => Subst (Field a) Name a where
    subst s (Field l e)         = Field l (subst s e)

instance Subst a Name Exp => Subst (Rhs a) Name Exp where
    subst s (RExp e)            = RExp (subst s e)
    subst s (RGrd gs)           = RGrd (subst s gs)
    subst s (RWhere rhs bs)     = RWhere (subst s rhs) (subst s bs)


instance Subst a Name Exp => Subst (GExp a) Name Exp where
    subst s (GExp qs e)         = GExp (subst s qs) (subst s e)

            
instance Subst a Name Exp => Subst (Alt a) Name Exp where
    subst s (Alt p rhs)         = Alt p (subst s rhs)


instance Subst Qual Name Exp where
    subst s (QExp e)            = QExp (subst s e)
    subst s (QGen p e)          = QGen p (subst s e)
    subst s (QLet bs)           = QLet (subst s bs)


instance Subst Stmts Name Exp where
    subst s (Stmts ss) = Stmts (subst s ss)
            
instance Subst Stmt Name Exp where
    subst s (SExp e)            = SExp (subst s e)
    subst s (SRet e)            = SRet (subst s e)
    subst s (SGen p e)          = SGen p (subst s e)
    subst s (SBind bs)          = SBind (subst s bs)
    subst s (SAss p e)          = SAss p (subst s e)
--    subst s (SForall qs st)     = SForall (subst s qs) (subst s st)
    subst s (SIf e st)          = SIf (subst s e) (subst s st)
    subst s (SElsif e st)       = SElsif (subst s e) (subst s st)
    subst s (SElse st)          = SElse (subst s st)
    subst s (SCase e alts)      = SCase (subst s e) (subst s alts)
    subst s (SMatch m)          = SMatch (subst s m)


instance Subst Type Name Type where
    subst s (TQual t ps)        = TQual (subst s t) (subst s ps)
    subst s (TCon c)            = TCon c
    subst s (TVar a)            = case lookup a s of
                                    Just t  -> t
                                    Nothing -> TVar a
    subst s (TAp t1 t2)         = TAp (subst s t1) (subst s t2)
    subst s (TSub t1 t2)        = TSub (subst s t1) (subst s t2)
    subst s TWild               = TWild
    subst s (TList t)           = TList (subst s t)
    subst s (TTup ts)           = TTup (subst s ts)
    subst s (TFun ts t)         = TFun (subst s ts) (subst s t)

instance Subst Pred Name Type where
    subst s (PType t)           = PType (subst s t)
    subst s p                   = p

-- Printing ==================================================================

-- Modules -------------------------------------------------------------------

instance Pr Module where
    pr (Module c is ds ps)      = text "module" <+> prId c <+> text "where"
                                  $$ vpr is $$ vpr ds 
                                  $$ text "private" $$ vpr ps

instance Pr Import where
   pr (Import True n)           = text "import" <+> prId n
   pr (Import False n)          = text "use" <+> prId n

-- Declarations --------------------------------------------------------------

instance Pr Decl where
    pr (DKSig c k)              = prId c <+> text "::" <+> pr k
    pr (DType c vs t)           = text "type" <+> prId c <+> hsep (map prId vs) 
                                  <+> equals <+> pr t
    pr (DData c vs subs cs)     = text "data" <+> prId c <+> hsep (map prId vs) 
                                  <+> prSubs subs <+> prCons cs
    pr (DRec isC c vs sups ss)  = text kwd <+> prId c <+> hsep (map prId vs) 
                                  <+> prSups sups <+> prEq ss $$ prSigs ss
      where kwd                 = if isC then "typeclass" else "struct"
--    pr (DInst t bs)             = text "instance" <+> pr t <+> text "=" $$ nest 4 (vpr bs)
    pr (DPSig v t)              = text "instance" <+> prId v <+> text "::" <+> pr t
    pr (DDefault ts)            = text "default" <+> hpr ',' ts
    pr (DInstance ns)           = text "instance" <+> hpr ',' ns 
    pr (DTClass ns)             = text "typeclass" <+> hpr ',' ns 
    pr (DBind bs)               = vpr bs
    pr (DExtern es)             = text "extern" <+> hpr ',' es

prPreds []                      = empty
prPreds ps                      = text " \\\\" <+> hpr ',' ps

prEq []                         = empty
prEq bs                         = text "where"

prSigs ss                       = nest 4 (vpr ss)

-- Sub/supertypes -----------------------------------------------------------

prSups []                       = empty
prSups ts                       = char '<' <+> hpr ',' ts

prSubs []                       = empty
prSubs ts                       = char '>' <+> hpr ',' ts
    

-- Constructors and contexts -----------------------------------------------


instance Pr Constr where
  pr (Constr c ts ps)           = prId c <+> hsep (map (prn 3) ts) <> prPreds ps

prCons []                       = empty
prCons (c:cs)                   = vcat (char '=' <+> pr c : map ((char '|' <+>) . pr) cs)



-- Signatures ------------------------------------------------------------

instance Pr Sig where
    pr (Sig vs qt)              = hcat (punctuate comma (map prId vs)) <+> text "::" <+> pr qt
    
    
-- Predicates ------------------------------------------------------------

instance Pr Pred where
    pr (PType t)                = pr t
    pr (PKind v k)              = prId v <+> text "::" <+> pr k

-- Left hand sides ------------------------------------------------------

instance Pr Lhs where
   pr (LFun v ps)               = prId v <+> hsep (map (prn 13) ps)
   pr (LPat p)                  = pr p


-- Patterns --------------------------------------------------------------
{-
instance Pr Pat where

    prn 0 (PCon c ps)           = prId c <+> hsep (map (prn 13) ps) 
    prn 0 p                     = prn 13 p

    prn 13 (PVar v)             = prId v
    prn 13 (PLit l)             = pr l
    prn 13 (PTup ps)            = parens (hpr ',' ps)
    prn 13 (PList ps)           = brackets (hpr ',' ps)
    prn 13 p                    = parens (pr p)

    prn n p                     = prn 13 p
-}

instance Pr Pat where

    prn 11 (PAp e e')           = prn 11 e <+> prn 12 e'
    prn 11 e                    = prn 13 e

    prn 12 e                    = prn 13 e
        
    prn 13 (PVar v)             = prId v
    prn 13 (PCon c)             = prId c
    prn 13 (PWild)              = text "_"
    prn 13 (PLit l)             = pr l
    prn 13 (PRec Nothing fs)    = text "{" <+> hpr ',' fs <+> text "}"
    prn 13 (PRec (Just(c,b)) fs)= prId c <+> text "{" <+> hpr ',' fs <+> (if b then empty else text "..") <+> text "}"
    prn 13 (PSig e qt)          = parens (pr e <+> text "::" <+> pr qt)
    prn 13 (PTup es)            = parens (hpr ',' es)
    prn 13 (PList es)           = brackets (hpr ',' es)
    prn 13 e                    = parens (prn 0 e)

    prn n e                     = prn 11 e

instance Pr [Pat] where pr = vpr
instance Pr [Bind] where pr = vpr

-- Types -----------------------------------------------------------------

instance Pr Type where
    prn 0 (TQual t ps)          = prn 1 t <> prPreds ps
    prn 0 t                     = prn 1 t

    prn 1 (TFun ts t)           = prArgs ts <+> prn 2 t
      where prArgs []           = empty
            prArgs (t:ts)       = prn 2 t <+> text "->" <+> prArgs ts
    prn 1 (TSub t t')           = prn 2 t <+> text "<" <+> prn 2 t'
    prn 1 t                     = prn 2 t

    prn 2 (TAp t t')            = prn 2 t <+> prn 3 t'
    prn 2 t                     = prn 3 t
        
    prn 3 (TCon c)              = prId c
    prn 3 (TVar v)              = prId v
    prn 3 (TWild)               = text "_"
    prn 3 (TList t)             = brackets (pr t)
    prn 3 (TTup ts)             = parens (hpr ',' ts)
    prn 3 t                     = parens (prn 0 t)

-- Bindings ----------------------------------------------------------------

instance Pr Bind where
    pr (BSig vs qt)             = hcat (punctuate comma (map prId vs)) <+> text "::" <+> pr qt
    pr (BEqn p (RExp e))        = pr p <+> equals <+> pr e
    pr (BEqn p rhs)             = pr p $$ nest 2 (prRhs equals rhs)

prRhs eq (RExp e)               = eq <+> pr e
prRhs eq (RGrd gs)              = vcat (map (prGuard eq) gs)
prRhs eq (RWhere rhs bs)        = prRhs eq rhs $$ text "where" $$ nest 4 (vpr bs)

prGuard eq (GExp qs e)          = char '|' <+> hpr ',' qs <+> eq <+> pr e


-- Expressions -------------------------------------------------------------

instance Pr Exp where
    prn 0 (ELam ps e)           = sep [char '\\' <> hsep (map (prn 13) ps) <+> text "->", pr e]
    prn 0 (ELet bs e)           = text "let" <+> vpr bs $$ text "in" <+> pr e
    prn 0 (EIf e e1 e2)         = text "if" <+> pr e $$
                                    nest 3 (text "then" <+> pr e1 $$
                                            text "else" <+> pr e2)
    prn 0 (ECase e alts)        = text "case" <+> pr e <+> text "of" $$ nest 2 (vpr alts)
    prn 0 (EMatch m)            = text "Match" <+> prn 12 m
    prn 0 (EDo v t ss)          = text "do"<>prN v <+> pr ss 
    prn 0 (ETempl v t ss)       = text "class"<>prN v $$ nest 4 (pr ss)
    prn 0 (EAct v ss)           = text "action"<>prN v $$ nest 4 (pr ss) 
    prn 0 (EReq v ss)           = text "request"<>prN v $$ nest 4 (pr ss) 
    prn 0 (EAfter e e')         = text "after" <+> prn 12 e <+> pr e'
    prn 0 (EBefore e e')        = text "before" <+> prn 12 e <+> pr e'
    prn 0 e                     = prn 1 e

    prn 11 (EAp e e')           = prn 11 e <+> prn 12 e'
    prn 11 e                    = prn 12 e
        
    prn 12 (ESelect e l)        = prn 12 e <> text "." <> prId l
    prn 12 e                    = prn 13 e
        
    prn 13 (EVar v)             = prId v
    prn 13 (ECon c)             = prId c
    prn 13 (ESel l)             = parens (text "." <> prId l)
    prn 13 (EWild)              = text "_"
    prn 13 (ELit l)             = pr l
    prn 13 (ERec Nothing fs)    = text "{" <+> hpr ',' fs <+> text "}"
    prn 13 (ERec (Just(c,b)) fs)= prId c <+> text "{" <+> hpr ',' fs <+> (if b then empty else text "..") <+> text "}"
    prn 13 (EBStruct _ _ bs)    = text "struct" $$ nest 4 (vpr bs)
    prn 13 (ENeg e)             = text "-" <+> prn 0 e
    prn 13 (ESig e qt)          = parens (pr e <+> text "::" <+> pr qt)
    prn 13 (ETup es)            = parens (hpr ',' es)
    prn 13 (ESectR e op)        = parens (pr e <> prOp op)
    prn 13 (ESectL op e)        = parens (prOp op <> pr e)
    prn 13 (EList es)           = brackets (hpr ',' es)
    prn 13 (ESeq e Nothing to) 
                                = brackets (pr e <> text ".." <> pr to)
    prn 13 (ESeq e (Just by) to) 
                                = brackets (pr e <> comma <> pr by <> text ".." <> pr to)
    prn 13 (EComp e qs)         = brackets (empty <+> pr e <+> char '|' <+> hpr ',' qs)
    prn 13 (EForall qs ss)      = text "forall" <+> hpr ',' qs <+> text "do" $$ nest 4 (pr ss)
    prn 13 e                    = parens (prn 0 e)

    prn n e                     = prn 11 e
   
prN (Just v) = text "@" <> pr v
prN Nothing = empty 

instance Pr a => Pr (Field a) where
    pr (Field l e)              = prId l <+> equals <+> pr e
    
instance Pr a => Pr (Alt a) where
    pr (Alt p (RExp e))         = pr p <+> text "->" <+> pr e
    pr (Alt p rhs)              = pr p <+> prRhs (text "->") rhs

instance Pr Qual where
    pr (QExp e)                 = pr e
    pr (QGen p e)               = pr p <+> text "<-" <+> pr e
    pr (QLet bs)                = text "let" <+> hpr ';' bs

-- Statements --------------------------------------------------------------

instance Pr Stmts where
    pr (Stmts ss)               = vpr ss

instance Pr Stmt where
    pr (SExp e)                 = pr e
    pr (SRet e)                 = text "result" <+> pr e
    pr (SGen p e)               = pr p <+> text "<-" <+> pr e
    pr (SBind bs)               = vpr bs
    pr (SAss p e)               = pr p <+> text ":=" <+> pr e
    pr (SIf e ss)               = text "if" <+> pr e <+> text "then" $$ nest 4 (pr ss)
    pr (SElsif e ss)            = text "elsif" <+> pr e <+> text "then" $$ nest 4 (pr ss)
    pr (SElse ss)               = text "else" $$ nest 4 (pr ss)
--    pr (SForall qs ss)          = text "forall" <+> hpr ',' qs <+> text "do" $$ nest 4 (pr ss)
    pr (SCase e alts)           = text "case" <+> pr e <+> text "of" $$ nest 4 (vpr alts)
    pr (SMatch m)               = text "Match" <+> prn 12 m


-- Free variables ------------------------------------------------------------

instance Ids Bind where
    idents (BEqn (LFun v ps) rh)= idents rh \\ (v : idents ps)
    idents (BEqn (LPat p) rh)   = idents rh \\ idents p 
    idents (BSig _ _)           = []

instance Ids Pat where
    idents (PVar v)             = [v]
    idents (PAp e e')           = idents e ++ idents e'
    idents (PCon _)             = []
    idents (PLit _)             = []
    idents (PTup es)            = idents es
    idents (PList es)           = idents es
    idents PWild                = []
    idents (PSig e _)           = idents e
    idents (PRec _  fs)         = idents fs

instance Ids Exp where
    idents (EVar v)             = [v]
    idents (EAp e e')           = idents e ++ idents e'
    idents (ETup es)            = idents es
    idents (EList es)           = idents es
    idents (ESig e _)           = idents e
    idents (ERec _  fs)         = idents fs
    idents (EBStruct _ _ bs)    = idents bs
    idents (ELam ps e)          = idents e \\ idents ps
    idents (ELet bs e)          = idents bs ++ (idents e \\ bvars bs)
    idents (ECase e as)         = idents e ++ idents as
    idents (EMatch m)           = idents m
    idents (EIf b t e)          = idents b ++ idents t ++ idents e
    idents (ENeg e)             = idents e
    idents (ESeq f Nothing t)   = idents f ++ idents t
    idents (ESeq f (Just s) t)  = idents f ++ idents s ++ idents t
    idents (EComp e qs)         = (idents e \\ bvars qs) ++ identQuals qs
    idents (ESectR e v)         = v : idents e
    idents (ESectL v e)         = v : idents e
    idents (ESelect e _)        = idents e
    idents (EDo _ _ ss)         = idents ss
    idents (ETempl _ _ ss)      = idents ss
    idents (EAct _ ss)          = idents ss 
    idents (EReq _ ss)          = idents ss 
    idents (EAfter e e')        = idents e ++ idents e'
    idents (EBefore e e')       = idents e ++ idents e'
    idents (EForall qs ss)      = identQuals qs ++ (idents ss \\ bvars qs)
    idents _                    = []

instance Ids a => Ids (Field a) where
    idents (Field _ e)          = idents e
    
instance Ids a => Ids (Rhs a) where
    idents (RExp a)             = idents a
    idents (RGrd gs)            = idents gs
    idents (RWhere a bs)        = idents bs ++ (idents a \\ bvars bs)

instance Ids a => Ids (Alt a) where
    idents (Alt p a)            = idents a \\ idents p     
{-
instance Ids (Rhs Stmts) where
    idents (RExp a)             = idents a
    idents (RGrd gs)            = idents gs
    idents (RWhere a bs)        = idents bs ++ (idents a \\ bvars bs)

instance Ids (GExp Exp) where
    idents (GExp qs a)          = identQuals qs ++ idents a
    
instance Ids (Alt Stmts) where
    idents (Alt p a)            = idents a \\ idents p     
-}    
instance Ids a => Ids (GExp a) where
    idents (GExp qs a)          = identQuals qs ++ idents a


identQuals []                   = []
identQuals (QExp e : qs)        = idents e ++ identQuals qs
identQuals (QGen p e : qs)      = idents e ++ (identQuals qs \\ pvars p)
identQuals (QLet bs : qs)       = idents bs ++ (identQuals qs \\ bvars bs)


instance Ids Stmts where
    idents (Stmts ss) = identSs ss
      where
        identSs []                      = []
        identSs (SExp e : ss)           = idents e ++ identSs ss
        identSs (SRet e : ss)           = idents e ++ identSs ss
        identSs (SGen p e : ss)         = idents e ++ (identSs ss \\ pvars p)
        identSs (SBind bs : ss)         = identSBind bs ss
        identSs (SAss p e : ss)         = idents e ++ (identSs ss \\ pvars p)
      --identSs (SForall qs ss' : ss)   = identQuals qs ++ (identSs ss' \\ bvars qs) ++ identSs ss
        identSs (SIf e ss' : ss)        = idents e ++ idents ss' ++ identSs ss
        identSs (SElsif e ss' : ss)     = idents e ++ idents ss' ++ identSs ss
        identSs (SElse ss' : ss)        = idents ss' ++ identSs ss
        identSs (SCase e as : ss)       = idents e ++ idents as ++ identSs ss
        identSs (SMatch m : ss)         = idents m ++ identSs ss

        identSBind bs (SBind bs' : ss ) = identSBind (bs++bs') ss
        identSBind bs ss                = idents bs ++ (identSs ss \\ bvars bs)


pvars p                         = evars p

assignedVars (Stmts ss)         = concat [ pvars p | SAss p _ <- ss ]


-- Bound variables ----------------------------------------------------------

instance BVars [Bind] where
    bvars []                            = []
    bvars (BEqn (LPat p) _ : bs)        = pvars p ++ bvars bs
    bvars (BEqn (LFun v _) _ : bs)      = bvars' v bs
      where bvars' v (BEqn (LFun v' _) _ : bs)
              | v == v'                 = bvars' v bs
              | otherwise               = v : bvars' v' bs
            bvars' v bs                 = v : bvars bs
    bvars (_ : bs)                      = bvars bs

instance BVars [Field a] where
    bvars bs                            = [ s | Field s e <- bs ]

instance BVars [Qual] where
    bvars []                            = []
    bvars (QGen p _ : qs)               = pvars p ++ bvars qs
    bvars (QLet bs : qs)                = bvars bs ++ bvars qs
    bvars (_ : qs)                      = bvars qs
{-
instance BVars [Stmt] where
    bvars []                            = []
    bvars (SGen p e : ss)               = pvars p ++ bvars ss
    bvars (SBind bs : ss)               = bvars bs ++ bvars ss
    bvars (_ : ss)                      = bvars ss
-}

instance BVars Lhs where
    bvars (LFun v ps)           = pvars ps
    bvars (LPat p)              = []

instance BVars [Pred] where
    bvars []                    = []
    bvars (PKind v k : ps)      = v : bvars ps
    bvars (PType (TVar v) : ps) = v : bvars ps
    bvars (p : ps)              = bvars ps

-- Free type variables -------------------------------------------------------

instance Ids Type where
  idents (TQual t ps)           = (idents t ++ idents ps) \\ bvars ps
  idents (TCon c)               = [c]
  idents (TVar v)               = [v]
  idents (TAp t t')             = idents t ++ idents t'
  idents (TSub t t')            = idents t ++ idents t'
  idents (TWild)                = []
  idents (TList t)              = idents t
  idents (TTup ts)              = concatMap idents ts
  idents (TFun t t')            = idents t ++ idents t'


instance Ids Pred where
  idents (PType (TVar v))       = []
  idents (PType t)              = idents t
  idents (PKind v k)            = []


-- PosInfo -------------------------------------------------------------------

instance HasPos Type where
  posInfo (TQual t ps)          = between (posInfo t) (posInfo ps)
  posInfo (TCon n)              = posInfo n
  posInfo (TVar n)              = posInfo n 
  posInfo (TAp t t')            = between (posInfo t) (posInfo t')
  posInfo (TSub t t')           = between (posInfo t) (posInfo t')
  posInfo TWild                 = Unknown
  posInfo (TList t)             = posInfo t 
  posInfo (TTup ts)             = posInfo ts
  posInfo (TFun ts t)           = between (posInfo ts) (posInfo t)

instance HasPos Pred where
  posInfo (PType t)              = posInfo t
  posInfo (PKind n k)            = posInfo n

instance HasPos Pat where
  posInfo (PVar n)              = posInfo n
  posInfo (PAp e e')            = between (posInfo e) (posInfo e')
  posInfo (PCon c)              = posInfo c
  posInfo (PLit l)              = posInfo l
  posInfo (PTup es)             = posInfo es
  posInfo (PList es)            = posInfo es
  posInfo PWild                 = Unknown
  posInfo (PSig e t)            = between (posInfo e) (posInfo t)
  posInfo (PRec m fs)           = between (posInfo m) (posInfo fs)

instance HasPos Exp where
  posInfo (EVar n)              = posInfo n
  posInfo (ECon c)              = posInfo c
  posInfo (EAp e e')            = between (posInfo e) (posInfo e')
  posInfo (ESel n)              = posInfo n
  posInfo (ETup es)             = posInfo es
  posInfo (EList es)            = posInfo es
  posInfo (ESig e t)            = between (posInfo e) (posInfo t)
  posInfo (ERec m fs)           = between (posInfo m) (posInfo fs)
  posInfo (EBStruct c _ bs)     = between (posInfo c) (posInfo bs)
  posInfo (ELam ps e)           = between (posInfo ps) (posInfo e)
  posInfo (ELet bs e)           = between (posInfo bs) (posInfo e)
  posInfo (ECase e as)          = between (posInfo e) (posInfo as)
  posInfo (EMatch m)            = posInfo m
  posInfo (EIf e t f)           = posInfo [e,t,f]
  posInfo (ENeg e)              = posInfo e
  posInfo (ESeq e1 m e2)        = foldr1 between [posInfo e1, posInfo m, posInfo e2]
  posInfo (EComp e qs)          = between (posInfo e) (posInfo qs)
  posInfo (ESectR e n)          = between (posInfo e) (posInfo n)
  posInfo (ESectL n e)          = between (posInfo n) (posInfo e)
  posInfo (ESelect e n)         = between (posInfo e) (posInfo n)
  posInfo (EDo n t ss)          = between (posInfo n) (posInfo ss)
  posInfo (ETempl n t ss)       = between (posInfo n) (posInfo ss)
  posInfo (EAct n ss)           = between (posInfo n) (posInfo ss)
  posInfo (EReq n ss)           = between (posInfo n) (posInfo ss)
  posInfo (EAfter e e')         = between (posInfo e) (posInfo e')
  posInfo (EBefore e e')        = between (posInfo e) (posInfo e')
  posInfo (ELit l)              = posInfo l
  posInfo (EForall qs ss)       = between (posInfo qs) (posInfo ss)  
  posInfo _                     = Unknown

instance HasPos a => HasPos (Field a) where
  posInfo (Field n e)           = between (posInfo n) (posInfo e)

instance HasPos a => HasPos (Rhs a) where
  posInfo (RExp a)              = posInfo a
  posInfo (RGrd gs)             = posInfo gs
  posInfo (RWhere r bs)         = between (posInfo r) (posInfo bs)
 
instance HasPos a => HasPos (GExp a) where
  posInfo (GExp qs a)           = between (posInfo qs) (posInfo a)

instance HasPos a => HasPos (Alt a) where
  posInfo (Alt p r)             = between (posInfo p) (posInfo r)

instance HasPos Qual where
  posInfo (QExp e)              = posInfo e
  posInfo (QGen p e)            = between (posInfo p) (posInfo e)
  posInfo (QLet bs)             = posInfo bs

instance HasPos Stmt where
  posInfo (SExp e)              = posInfo e
  posInfo (SRet e)              = posInfo e
  posInfo (SGen p e)            = between (posInfo p) (posInfo e)
  posInfo (SBind b)             = posInfo b
  posInfo (SAss p e)            = between (posInfo p) (posInfo e)
--  posInfo (SForall qs ss)       = between (posInfo qs) (posInfo ss)
  posInfo (SIf e ss)            = between (posInfo e) (posInfo ss)
  posInfo (SElsif e ss)         = between (posInfo e) (posInfo ss)
  posInfo (SElse ss)            = posInfo ss
  posInfo (SCase e as)          = between (posInfo e) (posInfo as)

instance HasPos Bind where
  posInfo (BSig ns t)           = posInfo ns
  posInfo (BEqn l r)            = between (posInfo l) (posInfo r)

instance HasPos Lhs where
  posInfo (LFun n ps)           = between (posInfo n) (posInfo ps)
  posInfo (LPat p)              = posInfo p

-- Binary --------------------------------------------------------------------

instance Binary Module where
  put (Module a b c d) = put a >> put b >> put c >> put d
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (Module a b c d)

instance Binary Import where
  put (Import a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (Import a b)

instance Binary Decl where
  put (DKSig a b) = putWord8 0 >> put a >> put b
  put (DData a b c d) = putWord8 1 >> put a >> put b >> put c >> put d
  put (DRec a b c d e) = putWord8 2 >> put a >> put b >> put c >> put d >> put e
  put (DType a b c) = putWord8 3 >> put a >> put b >> put c
--  put (DInst a b) = putWord8 4 >> put a >> put b
  put (DPSig a b) = putWord8 4 >> put a >> put b
  put (DDefault a) = putWord8 5 >> put a
  put (DInstance a) = putWord8 6 >> put a
  put (DTClass a) = putWord8 7 >> put a
  put (DBind a) = putWord8 8 >> put a
  put (DExtern a) = putWord8 9 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (DKSig a b)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (DData a b c d)
      2 -> get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> get >>= \e -> return (DRec a b c d e)
      3 -> get >>= \a -> get >>= \b -> get >>= \c -> return (DType a b c)
--      4 -> get >>= \a -> get >>= \b -> return (DInst a b)
      4 -> get >>= \a -> get >>= \b -> return (DPSig a b)
      5 -> get >>= \a -> return (DDefault a)
      6 -> get >>= \a -> return (DInstance a)
      7 -> get >>= \a -> return (DTClass a)
      8 -> get >>= \a -> return (DBind a)
      9 -> get >>= \a -> return (DExtern a)

      _ -> fail "no parse"

instance Binary Constr where
  put (Constr a b c) = put a >> put b >> put c
  get = get >>= \a -> get >>= \b -> get >>= \c -> return (Constr a b c)

instance Binary Sig where
  put (Sig a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (Sig a b)

instance Binary Bind where
  put (BSig a b) = putWord8 0 >> put a >> put b
  put (BEqn a b) = putWord8 1 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (BSig a b)
      1 -> get >>= \a -> get >>= \b -> return (BEqn a b)
      _ -> fail "no parse"

instance Binary Type where
  put (TQual a b) = putWord8 0 >> put a >> put b
  put (TCon a) = putWord8 1 >> put a
  put (TVar a) = putWord8 2 >> put a
  put (TAp a b) = putWord8 3 >> put a >> put b
  put (TSub a b) = putWord8 4 >> put a >> put b
  put TWild = putWord8 5
  put (TList a) = putWord8 6 >> put a
  put (TTup a) = putWord8 7 >> put a
  put (TFun a b) = putWord8 8 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (TQual a b)
      1 -> get >>= \a -> return (TCon a)
      2 -> get >>= \a -> return (TVar a)
      3 -> get >>= \a -> get >>= \b -> return (TAp a b)
      4 -> get >>= \a -> get >>= \b -> return (TSub a b)
      5 -> return TWild
      6 -> get >>= \a -> return (TList a)
      7 -> get >>= \a -> return (TTup a)
      8 -> get >>= \a -> get >>= \b -> return (TFun a b)
      _ -> fail "no parse"

instance Binary Pred where
  put (PType a) = putWord8 0 >> put a
  put (PKind a b) = putWord8 1 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (PType a)
      1 -> get >>= \a -> get >>= \b -> return (PKind a b)
      _ -> fail "no parse"

instance Binary Lhs where
  put (LFun a b) = putWord8 0 >> put a >> put b
  put (LPat a) = putWord8 1 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (LFun a b)
      1 -> get >>= \a -> return (LPat a)
      _ -> fail "no parse"

instance Binary Pat where
  put (PVar a) = putWord8 0 >> put a
  put (PAp a b) = putWord8 1 >> put a >> put b
  put (PCon a) = putWord8 2 >> put a
  put (PLit a) = putWord8 4 >> put a
  put (PTup a) = putWord8 5 >> put a
  put (PList a) = putWord8 6 >> put a
  put PWild = putWord8 7
  put (PSig a b) = putWord8 8 >> put a >> put b
  put (PRec a b) = putWord8 9 >> put a >> put b

  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> fmap PVar get
      1 -> liftM2 PAp get get
      2 -> fmap PCon get
      4 -> fmap PLit get
      5 -> fmap PTup get
      6 -> fmap PList get
      7 -> return PWild
      8 -> liftM2 PSig get get
      9 -> liftM2 PRec get get
      _ -> fail "no parse"

instance Binary Exp where
  put (EVar a) = putWord8 0 >> put a
  put (EAp a b) = putWord8 1 >> put a >> put b
  put (ECon a) = putWord8 2 >> put a
  put (ESel a) = putWord8 3 >> put a
  put (ELit a) = putWord8 4 >> put a
  put (ETup a) = putWord8 5 >> put a
  put (EList a) = putWord8 6 >> put a
  put EWild = putWord8 7
  put (ESig a b) = putWord8 8 >> put a >> put b
  put (ERec a b) = putWord8 9 >> put a >> put b
  put (ELam a b) = putWord8 10 >> put a >> put b
  put (ELet a b) = putWord8 11 >> put a >> put b
  put (ECase a b) = putWord8 12 >> put a >> put b
  put (EIf a b c) = putWord8 13 >> put a >> put b >> put c
  put (ENeg a) = putWord8 14 >> put a
  put (ESeq a b c) = putWord8 15 >> put a >> put b >> put c
  put (EComp a b) = putWord8 16 >> put a >> put b
  put (ESectR a b) = putWord8 17 >> put a >> put b
  put (ESectL a b) = putWord8 18 >> put a >> put b
  put (ESelect a b) = putWord8 19 >> put a >> put b
  put (EDo a b c) = putWord8 21 >> put a >> put b >> put c
  put (ETempl a b c) = putWord8 22 >> put a >> put b >> put c
  put (EAct a b) = putWord8 23 >> put a >> put b
  put (EReq a b) = putWord8 24 >> put a >> put b
  put (EAfter a b) = putWord8 25 >> put a >> put b
  put (EBefore a b) = putWord8 26 >> put a >> put b
  put (EBStruct a b c) = putWord8 27 >> put a >> put b >> put c
  put (EForall a b) = putWord8 28 >> put a >> put b  

  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (EVar a)
      1 -> get >>= \a -> get >>= \b -> return (EAp a b)
      2 -> get >>= \a -> return (ECon a)
      3 -> get >>= \a -> return (ESel a)
      4 -> get >>= \a -> return (ELit a)
      5 -> get >>= \a -> return (ETup a)
      6 -> get >>= \a -> return (EList a)
      7 -> return EWild
      8 -> get >>= \a -> get >>= \b -> return (ESig a b)
      9 -> get >>= \a -> get >>= \b -> return (ERec a b)
      10 -> get >>= \a -> get >>= \b -> return (ELam a b)
      11 -> get >>= \a -> get >>= \b -> return (ELet a b)
      12 -> get >>= \a -> get >>= \b -> return (ECase a b)
      13 -> get >>= \a -> get >>= \b -> get >>= \c -> return (EIf a b c)
      14 -> get >>= \a -> return (ENeg a)
      15 -> get >>= \a -> get >>= \b -> get >>= \c -> return (ESeq a b c)
      16 -> get >>= \a -> get >>= \b -> return (EComp a b)
      17 -> get >>= \a -> get >>= \b -> return (ESectR a b)
      18 -> get >>= \a -> get >>= \b -> return (ESectL a b)
      19 -> get >>= \a -> get >>= \b -> return (ESelect a b)
      21 -> get >>= \a -> get >>= \b -> get >>= \c -> return (EDo a b c)
      22 -> get >>= \a -> get >>= \b -> get >>= \c -> return (ETempl a b c)
      23 -> get >>= \a -> get >>= \b -> return (EAct a b)
      24 -> get >>= \a -> get >>= \b -> return (EReq a b)
      25 -> get >>= \a -> get >>= \b -> return (EAfter a b)
      26 -> get >>= \a -> get >>= \b -> return (EBefore a b)
      27 -> get >>= \a -> get >>= \b -> get >>= \c -> return (EBStruct a b c)
      28 -> get >>= \a -> get >>= \b -> return (EForall a b)
      _ -> fail "no parse"

instance Binary a => Binary (Field a) where
  put (Field a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (Field a b)

instance (Binary a) => Binary (Rhs a) where
  put (RExp a) = putWord8 0 >> put a
  put (RGrd a) = putWord8 1 >> put a
  put (RWhere a b) = putWord8 2 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (RExp a)
      1 -> get >>= \a -> return (RGrd a)
      2 -> get >>= \a -> get >>= \b -> return (RWhere a b)
      _ -> fail "no parse"

instance (Binary a) => Binary (GExp a) where
  put (GExp a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (GExp a b)

instance (Binary a) => Binary (Alt a) where
  put (Alt a b) = put a >> put b
  get = get >>= \a -> get >>= \b -> return (Alt a b)

instance Binary Qual where
  put (QExp a) = putWord8 0 >> put a
  put (QGen a b) = putWord8 1 >> put a >> put b
  put (QLet a) = putWord8 2 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (QExp a)
      1 -> get >>= \a -> get >>= \b -> return (QGen a b)
      2 -> get >>= \a -> return (QLet a)
      _ -> fail "no parse"

instance Binary Stmt where
  put (SExp a) = putWord8 0 >> put a
  put (SRet a) = putWord8 1 >> put a
  put (SGen a b) = putWord8 2 >> put a >> put b
  put (SBind a) = putWord8 3 >> put a
  put (SAss a b) = putWord8 4 >> put a >> put b
--  put (SForall a b) = putWord8 5 >> put a >> put b
  put (SIf a b) = putWord8 7 >> put a >> put b
  put (SElsif a b) = putWord8 8 >> put a >> put b
  put (SElse a) = putWord8 9 >> put a
  put (SCase a b) = putWord8 10 >> put a >> put b
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (SExp a)
      1 -> get >>= \a -> return (SRet a)
      2 -> get >>= \a -> get >>= \b -> return (SGen a b)
      3 -> get >>= \a -> return (SBind a)
      4 -> get >>= \a -> get >>= \b -> return (SAss a b)
--      5 -> get >>= \a -> get >>= \b -> return (SForall a b)
      7 -> get >>= \a -> get >>= \b -> return (SIf a b)
      8 -> get >>= \a -> get >>= \b -> return (SElsif a b)
      9 -> get >>= \a -> return (SElse a)
      10 -> get >>= \a -> get >>= \b -> return (SCase a b)
      _ -> fail "no parse"

