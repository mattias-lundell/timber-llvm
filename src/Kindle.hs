{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

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

module Kindle where

import Monad
import Common
import PP
import qualified Core
import qualified Env
import Data.Binary
import Control.Monad.Identity

-- Kindle is the main back-end intermediate language.  It is a typed imperative language with dynamic 
-- memory allocation and garbage-collection, that can be described as a slightly extended version of
-- the common subset of C, Java and C++.  The purpose of Kindle is to function as a high-level back-
-- end program format that can be translated into standard imperative languages as well as assembly
-- code without too much difficulty.  None of C's pointer arithmetic features are present, neither
-- are the class hieracrhies of Java and C++.  The type system of Kindle is a miniature variant of
-- System F, but unsafe type-casts are supported.  The main extension compared to Java is nested
-- recursive functions.  Heap-allocated struct objects may contain function-valued components which
-- are invoked using self-application.  This provides a form of basic OO capability that may serve
-- as a target for a closure conversion pass.  For more details on the relation between Kindle and
-- the primary intermediate language Core, see module Core2Kindle.


-- A Kindle module consists of type declarations and term bindings.  A type declaration introduces
-- a struct type, and optionally also an enumeration of type names.  A binding defines either a named 
-- function or a named value of atomic type.  Function bindings are immutable.  All type declarations 
-- are mutually recursive, and so are binding groups.


data Module     = Module Name [Name] TEnv Decls Binds
                deriving (Eq,Show)

-- A type declaration introduces a struct type that defines the layout of heap-allocated objects.
-- A struct type may bind type parameters and may contain both value and function fields.  Each 
-- struct introduces a private namespace for its field names.  A struct can also be declared an
-- extension of another struct, as indicated by the Link parameter.  If a struct is an extension,
-- a prefix of its field names and their types must match the full definition of the extended 
-- struct when instantiated with the given type arguments.  In addition, an extended struct may
-- indicate that a subset of its type parameters are to be existentially quantified when the
-- struct is cast to its base type, and this list must contain the existential parameters of
-- its base type (if any) as a prefix.  If a struct is not an extension, it must either be an 
-- ordinary Top of an extension hierarchy, or a Union - the latter form supporting case analysis
-- between different extensions by means of the 'switch' command.
type Decls      = Map Name Decl

data Decl       = Struct [Name] TEnv Link
                deriving (Eq,Show)

data Link       = Top
                | Union
                | Extends Name [AType] [Name]
                deriving (Eq,Show)

-- A term binding is either a named value of atomic type or a named function.  A named function can
-- bind type parameters, thus introducing polymorphism in the style of System F.  The result and 
-- parameter types of a function are in the scope of the defined type parameters and must all be 
-- atomic.
type Binds      = Map Name Bind

data Bind       = Val    AType Exp
                | Fun    [Name] AType ATEnv Cmd
                deriving (Eq,Show)


-- The type of a binding is either just an atomic type in case of a value binding, or a triple 
-- consisting of abstracted type parameters, the argument types, and the result type in the 
-- function binding case.
type TEnv       = Map Name Type

data Type       = ValT   AType
                | FunT   [Name] [AType] AType
                deriving (Eq,Show)


-- An atomic type is either a named constructor or a type variable (both possibly applied to
-- further atomic type arguments), or an index into the sequence of existentially quantified
-- type components of the surrounding struct (only valid within function-valued field definitions).
type ATEnv      = Map Name AType

data AType      = TCon   Name [AType]
                | TVar   Name [AType]
                | TThis  Int
                deriving (Eq,Show)


-- A function body is a command that computes the desired result, possibly while performing imperative
-- side-effects.  The current type system does not distinguish pure functions from side-effecting ones,
-- although that separation will indeed be maintained by the translation from type-checked Core programs.
data Cmd        = CRet    Exp                 -- simply return $1
                | CRun    Exp Cmd             -- evaluate $1 for its side-effects only, then execure tail $2
                | CBind   Bool Binds Cmd      -- introduce (recursive? $1) local bindings $2, then execute tail $3
                | CUpd    Name Exp Cmd        -- overwrite value variable $1 with value $2, execute tail $3
                | CUpdS   Exp Name Exp Cmd    -- overwrite value field $2 of struct $1 with value $3, execute tail $4
                | CUpdA   Exp Exp Exp Cmd     -- overwrite index $2 of array $1 with value $3, execute tail $4
                | CSwitch Exp [Alt]           -- depending on the dynamic value of $1, choose tails from $2
                | CSeq    Cmd Cmd             -- execute $1; if fall-through, continue with $2
                | CBreak                      -- break out of a surrounding switch
                | CRaise  Exp                 -- raise an exception
                | CWhile  Exp Cmd Cmd         -- run $2 while $1 is non-zero, then execute tail $3
                | CCont                       -- start next turn of enclosing while loop
                deriving (Eq,Show)

-- Note 1: command (CRun e c) is identical to (CBind False [(x,e)] c) if x is a fresh name not used anywhere else
-- Note 2: the Cmd alternatives CSeq and CBreak are intended to implement the Fatbar and Fail primitives 
-- of the pattern-matching datatype PMC used in Core.

data Alt        = ACon  Name [Name] ATEnv Cmd -- if switch value has struct type variant $1, execute tail $4 with
                                              -- $3 bound to the actual struct fields and $2 bound to the struct
                                              -- type arguments not statically known
                | ALit  Lit Cmd               -- if switch value matches literal $1, execute tail $2 
                | AWild Cmd                   -- execute tail $1 as default alternative
                deriving (Eq,Show)

-- Simple expressions that can be RH sides of value definitions as well as function arguments.
data Exp        = EVar   Name                   -- local or global value name, enum constructor or function parameter
                | EThis                         -- the surrounding struct itself 
                                                -- (only valid within function fields, illegal in local/global functions)
                | ELit   Lit                    -- literal
                | ESel   Exp Name               -- selection of value field $2 from struct $1
                | ENew   Name [AType] Binds     -- a new struct of type $1 with type args $2, (partially) initialized by $3
                | ECall  Name [AType] [Exp]     -- calling local or global function $1 with type/term arguments $2/$3
                | EEnter Exp Name [AType] [Exp] -- entering function $2 of struct $1 with type/term arguments $3/($1:$4)
                | ECast  AType Exp              -- unchecked cast of value $2 to type $1
                deriving (Eq,Show)

-- Note: Kindle allows free variables to occur inside local functions and function-valued struct fields.  A
-- Kindle implementation must either handle this correctly at run-time, or such variable occurrences must be 
-- transformed away at compile-time.  The latter can be done using lambda-lifting for local functions and
-- explicitly closing the struct functions via extra value fields accessed through "this".

tId x                                   = tCon (prim x)

tCon n                                  = TCon n []
tVar n                                  = TVar n []

tPOLY                                   = tId POLY
tWORD                                   = tId WORD

tUNIT                                   = tCon (tuple 0)

tTime                                   = tId Time
tMsg                                    = tId Msg
tOID                                    = tId OID
tInt                                    = tId Int
tFloat                                  = tId Float
tChar                                   = tId Char
tBool                                   = tId Bool
tBITS8                                  = tId BITS8
tBITS16                                 = tId BITS16
tBITS32                                 = tId BITS32
tTimer                                  = tId TIMERTYPE

tRef a                                  = TCon (prim Ref) [a]
tLIST a                                 = TCon (prim LIST) [a]
tArray a                                = TCon (prim Array) [a]

maxSmallCLOS                            = 3 :: Int
smallCLOS 1                             = CLOS1
smallCLOS 2                             = CLOS2
smallCLOS 3                             = CLOS3

encodeCLOS [] t ts                      = t:ts
encodeCLOS vs t ts                      = t:ts ++ tCon (prim CLOS) : map tVar vs

decodeCLOS ts0                          = (vs, ts, t)
  where (t:ts,tvs)                      = span (/=(tCon (prim CLOS))) ts0
        vs                              = [ v | TVar v _ <- tvs ]

tClos t ts                              = tClos2 [] t ts

tClos2 [] t ts
  | a <= maxSmallCLOS                   = TCon (prim (smallCLOS a)) (t:ts)
  where a                               = length ts
tClos2 vs t ts                          = TCon (prim CLOS) (encodeCLOS vs t ts)

openClosureType (TCon (Prim p _) ts)
  | p `elem` [CLOS1,CLOS2,CLOS3,CLOS]   = decodeCLOS ts
openClosureType t                       = internalError "Kindle.openClosureType" t

closure t te c                          = closure2 [] t te c

closure2 [] t te c
  | a <= maxSmallCLOS                   = ENew (prim (smallCLOS a)) (t:rng te) [(prim Code, Fun [] t te c)]
  where a                               = length te
closure2 vs t te c                      = ENew (prim CLOS) (encodeCLOS vs t (rng te)) [(prim Code, Fun vs t te c)]

closBind ts0                            = [(prim Code, FunT vs ts t)]
  where (vs,ts,t)                       = decodeCLOS ts0

enter e ts es                           = EEnter e (prim Code) ts es

multiEnter [] [] e                      = e
multiEnter (ts:tss) es e                = multiEnter tss es2 (enter e [] es1)
  where (es1,es2)                       = splitAt (length ts) es
                                        
tupleDecl (Tuple n _)                   = Struct ids (map f ids) Top
  where ids                             = take n abcSupply
        f n                             = (n, ValT (TVar n []))

equants (Extends _ _ vs)                = vs
equants _                               = []


litType (LInt _ _)                      = tInt
litType (LRat _ _)                      = tFloat
litType (LChr _ _)                      = tChar
litType (LStr _ _)                      = tLIST tChar

a                                       = name0 "a"
b                                       = name0 "b"
c                                       = name0 "c"
d                                       = name0 "d"
ta                                      = TVar a []
tb                                      = TVar b []
tc                                      = TVar c []
td                                      = TVar d []
        
primDecls                               = (prim Bool,      Struct []    []                      Union) :
                                          (prim FALSE,     Struct []    []                      (Extends (prim Bool) [] [])) :
                                          (prim TRUE,      Struct []    []                      (Extends (prim Bool) [] [])) :
                                          (prim LIST,      Struct [a]   []                      Union) :
                                          (prim NIL,       Struct [a]   []                      (Extends (prim LIST) [ta] [])) :
                                          (prim CONS,      Struct [a]   [(a, ValT ta), 
                                                                         (b, ValT (tLIST ta))]  (Extends (prim LIST) [ta] [])) :
                                          (prim Ref,       Struct [a]   [(prim STATE, ValT ta)] Top) :
                                          (prim EITHER,    Struct [a,b] []                      Union) :
                                          (prim LEFT,      Struct [a,b] [(a,ValT ta)]           (Extends (prim EITHER) [ta,tb] [])) :
                                          (prim RIGHT,     Struct [a,b] [(a,ValT tb)]           (Extends (prim EITHER) [ta,tb] [])) :

                                          (prim Msg,       Struct []        [(prim Code, FunT [] [] tUNIT),
                                                                             (prim Baseline, ValT (tId AbsTime)),
                                                                             (prim Deadline, ValT (tId AbsTime)),
                                                                             (prim Next, ValT (tId Msg))]         Top) :
                                          (prim TIMERTYPE, Struct []        [(prim Reset,  FunT [] [tInt] tUNIT),
                                                                             (prim Sample, FunT [] [tInt] tTime)] Top) : 

                                          (prim CLOS1,     Struct [a,b]     [(prim Code, FunT [] [tb] ta)]        Top) :
                                          (prim CLOS2,     Struct [a,b,c]   [(prim Code, FunT [] [tb,tc] ta)]     Top) :
                                          (prim CLOS3,     Struct [a,b,c,d] [(prim Code, FunT [] [tb,tc,td] ta)]  Top) :
                                          []
                                          
isInfix (Prim p _)                      = p `elem` [MIN____KINDLE_INFIX .. MAX____KINDLE_INFIX]
isInfix _                               = False

isUnaryOp (Prim p _)                    = p `elem` [IntNeg, FloatNeg, NOT8, NOT16, NOT32]
isUnaryOp _                             = False

primKindleTerms                         = map prim [ MIN____VAR .. MAX____KINDLEVAR ]

primTEnv                                = primTEnv0 ++ map cv (Env.primTypeEnv `restrict` primKindleTerms)
  where 
    cv (x,Core.Scheme r [] ke)          = (x, cv0 r (dom ke))
    cv0 (Core.F ts t) vs                = FunT vs (map cv1 ts) (cv2 t)
    cv0 (Core.R t) []                   = ValT (cv3 t)
    cv0 (Core.R t) vs                   = FunT vs [] (cv3 t)
    cv1 (Core.Scheme r [] [])           = cv2 r
    cv2 (Core.R t)                      = cv3 t
    cv3 t
      | isCon n                         = TCon n ts'
      | otherwise                       = TVar n ts'
      where (Core.TId n, ts)            = Core.tFlat t
            ts'                         = map cv3 ts
    -- N.B.:  This type/scheme conversion algorithm is partial; it is only intended to cover the cases that appear
    -- when converting Env.primTypeEnv restricted to primKindleTerms (note the exceptions for TIMERTERM and Abort).


-- Primitive names only visible after translation into Kindle
primTEnv0                               = (prim TIMERTERM,  FunT []  [tInt] (tId TIMERTYPE)) :
                                          (prim Abort,      FunT [a] [tMsg, tRef ta] tUNIT) :
                                          
                                          (prim NEWREF,     FunT [a] [ta] (tRef ta)) :
                                          (prim STATEOF,    FunT [a] [tRef ta] ta) :
                                          (prim ASYNC,      FunT []  [tMsg,tTime,tTime] tUNIT) :
                                          (prim LOCK,       FunT []  [tOID] tUNIT) :
                                          (prim UNLOCK,     FunT []  [tOID] tUNIT) :
                                          (prim Inherit,    ValT tTime) :
                                          (prim EmptyArray, FunT [a] [tInt] (tArray ta)) :
                                          (prim CloneArray, FunT [a] [tArray ta, tInt] (tArray ta)) :
                                          []

okRec (ValT t)                          = okRec' t
okRec (FunT vs ts t)                    = True          -- Good: recursive function
okRec' (TVar _ _)                       = False         -- Bad: statically unknown representation
okRec' (TThis _)                        = False         -- Bad: statically unknown representation
okRec' (TCon n _) | n `elem` scalars    = False         -- Bad: type that can't fit placeholder
                  | otherwise           = True          -- Good: heap allocated data

scalars                                 = tuple 0 : map prim [Int, Float, Char, Bool, BITS8, BITS16, BITS32, AbsTime]

smallTypes                              = tuple 0 : map prim [Char, Bool, BITS8, BITS16]

isVal (_, Val _ _)                      = True
isVal (_, Fun _ _ _ _)                  = False

isFunT (_, ValT _)                      = False
isFunT (_, FunT _ _ _)                  = True

isTVar (TVar _ _)                       = True
isTVar _                                = False

isArray (TCon (Prim Array _) _)         = True
isArray _                               = False

isEVar (EVar _)                         = True
isEVar _                                = False

typeOf (Val t e)                        = ValT t
typeOf (Fun vs t te c)                  = FunT vs (rng te) t

rngType (ValT t)                        = t
rngType (FunT vs ts t)                  = t

typeOf' b                               = rngType (typeOf b)

declsOf (Module _ _ _ ds _)             = ds

unions ds                               = [ n | (n,Struct _ _ Union) <- ds ]
        
variants ds n0                          = [ n | (n,Struct _ _ (Extends n' _ _)) <- ds, n' == n0 ]

typeOfSel ds l                          = head [ (k,vs,t) | (k,Struct vs te _) <- ds, (l',t) <- te, l'==l ]

unit                                    = ENew (tuple 0) [] []

cBind [] c                              = c
cBind bs c                              = CBind False bs c

cBindR r [] c                           = c
cBindR r bs c                           = CBind r bs c

flatBinds bf                            = flat (bf CBreak)
  where flat (CBind r bs c)             = bs ++ flat c
        flat _                          = []

cUpd [] [] c                            = c
cUpd (x:xs) (e:es) c                    = CUpd x e (cUpd xs es c)

simpleExp (EVar _)                      = True
simpleExp (ELit _)                      = True
simpleExp (ECast _ e)                   = simpleExp e
simpleExp _                             = False

lock t e                                = ECast t (ECall (prim LOCK) [] [ECast tOID e])

unlock x c                              = cMap (CRun (ECall (prim UNLOCK) [] [e0]) . CRet) c
  where e0                              = ECast tOID (EVar x)


cMap f (CRet e)                         = f e
cMap f (CRun e c)                       = CRun e (cMap f c)
cMap f (CBind r bs c)                   = CBind r bs (cMap f c)
cMap f (CUpd y e c)                     = CUpd y e (cMap f c)
cMap f (CUpdS e y e' c)                 = CUpdS e y e' (cMap f c)
cMap f (CUpdA e i e' c)                 = CUpdA e i e' (cMap f c)
cMap f (CSwitch e alts)                 = CSwitch e (clift (cMap f) alts)
  where g (ACon y vs te c)              = ACon y vs te (cMap f c)
        g (ALit l c)                    = ALit l (cMap f c)
        g (AWild c)                     = AWild (cMap f c)
cMap f (CSeq c c')                      = CSeq (cMap f c) (cMap f c')
cMap f (CBreak)                         = CBreak
cMap f (CRaise e)                       = CRaise e
cMap f (CWhile e c c')                  = CWhile e (cMap f c) (cMap f c')
cMap f (CCont)                          = CCont


cMap' f = cMap (CRet . f)


class CLift a where
    clift                               :: (Cmd -> Cmd) -> a -> a

instance CLift Cmd where
    clift f                             = f

instance CLift Alt where
    clift f (ACon y vs te c)            = ACon y vs te (f c)
    clift f (ALit l c)                  = ALit l (f c)
    clift f (AWild c)                   = AWild (f c)

instance CLift a => CLift [a] where
    clift f                             = map (clift f)


cmap f                                  = clift (cMap (CRet . f))


findStruct s0 []                        = Nothing
findStruct s0 ((n,s):ds)
  | equalStructs (s0,s)                 = Just n
  | otherwise                           = findStruct s0 ds


equalStructs (Struct [] te1 l1, Struct [] te2 l2)
                                        = l1 == l2 && ls1 == ls2 && all equalTypes (ts1 `zip` ts2)
  where (ls1,ts1)                       = unzip te1
        (ls2,ts2)                       = unzip te2
equalStructs (Struct vs1 te1 l1, Struct vs2 te2 l2)
                                        = l1 == l2 && length vs1 == length vs2 &&
                                          ls1 == ls2 && all equalTypes (subst s ts1 `zip` ts2)
  where s                               = vs1 `zip` map tVar vs2
        (ls1,ts1)                       = unzip te1
        (ls2,ts2)                       = unzip te2

equalTypes (ValT t1, ValT t2)           = t1 == t2
equalTypes (FunT vs1 ts1 t1, FunT vs2 ts2 t2)
                                        = length vs1 == length vs2 && (t1:ts1) == subst s (t2:ts2)
  where s                               = vs2 `zip` map tVar vs1
equalTypes _                            = False


subexps (CRet e)                        = [e]
subexps (CRun e _)                      = [e]
subexps (CBind _ bs _)                  = [ e | (_,Val _ e) <- bs ]
subexps (CUpd _ e _)                    = [e]
subexps (CUpdS e _ e' _)                = [e,e']
subexps (CUpdA e _ e' _)                = [e,e']
subexps (CSwitch e _)                   = [e]
subexps (CWhile e _ _)                  = [e]
subexps _                               = []

raises es                               = [ e | ECall (Prim Raise _) _ [e] <- es ]


class RefThis a where
    refThis                             :: a -> Bool

instance RefThis a => RefThis [a] where
    refThis xs                          = any refThis xs

instance RefThis a => RefThis (Name,a) where
    refThis (_,x)                       = refThis x

instance RefThis Exp where
    refThis (EVar _)                    = False
    refThis (EThis)                     = True
    refThis (ELit _)                    = False
    refThis (ESel e _)                  = refThis e
    refThis (ENew _ ts bs)              = refThis ts || refThis bs
    refThis (ECall _ ts es)             = refThis ts || refThis es
    refThis (EEnter e _ ts es)          = refThis e || refThis ts || refThis es
    refThis (ECast t e)                 = refThis t || refThis e

instance RefThis Cmd where
    refThis (CRet e)                    = refThis e
    refThis (CRun e c)                  = refThis e || refThis c
    refThis (CBind _ bs c)              = refThis bs || refThis c
    refThis (CUpd _ e c)                = refThis e || refThis c
    refThis (CUpdS e _ e' c)            = refThis [e,e'] || refThis c
    refThis (CUpdA e e' e'' c)          = refThis [e,e',e''] || refThis c
    refThis (CSwitch e alts)            = refThis e || refThis alts
    refThis (CSeq c c')                 = refThis c || refThis c'
    refThis (CBreak)                    = False
    refThis (CRaise e)                  = refThis e
    refThis (CWhile e c c')             = refThis e || refThis c || refThis c'
    refThis (CCont)                     = False

instance RefThis Alt where
    refThis (ACon _ _ te c)             = refThis te || refThis c
    refThis (ALit l c)                  = refThis c
    refThis (AWild c)                   = refThis c

instance RefThis Bind where
    refThis (Val t e)                   = refThis t || refThis e
    refThis (Fun vs t te c)             = refThis t || refThis te

instance RefThis AType where
    refThis (TCon _ ts)                 = refThis ts
    refThis (TVar _ ts)                 = refThis ts
    refThis (TThis i)                   = True



-- Free variables ------------------------------------------------------------------------------------

instance Ids Exp where
    idents (EVar x)                     = [x]
    idents (EThis)                      = []
    idents (ELit l)                     = []
    idents (ESel e l)                   = idents e
    idents (ENew x ts bs)               = idents bs
    idents (ECall x ts es)              = x : idents es
    idents (EEnter e x ts es)           = idents e ++ idents es
    idents (ECast t e)                  = idents e

instance Ids Cmd where
    idents (CRet e)                     = idents e
    idents (CRun e c)                   = idents e ++ idents c
    idents (CBind False bs c)           = idents bs ++ (idents c \\ dom bs)
    idents (CBind True bs c)            = (idents bs ++ idents c) \\ dom bs
    idents (CUpd x e c)                 = idents e ++ idents c
    idents (CUpdS e x e' c)             = idents e ++ idents e' ++ idents c
    idents (CUpdA e i e' c)             = idents e ++ idents i ++ idents e' ++ idents c
    idents (CSwitch e alts)             = idents e ++ idents alts
    idents (CSeq c c')                  = idents c ++ idents c'
    idents (CBreak)                     = []
    idents (CRaise e)                   = idents e
    idents (CWhile e c c')              = idents e ++ idents c ++ idents c'
    idents (CCont)                      = []

instance Ids Alt where
    idents (ACon x vs te c)             = idents c \\ dom te
    idents (ALit l c)                   = idents c
    idents (AWild c)                    = idents c

instance Ids Bind where
    idents (Val t e)                    = idents e
    idents (Fun vs t te c)              = idents c \\ dom te

instance Ids AType where
    idents (TCon c ts)                  = c : idents ts
    idents (TVar v ts)                  = v : idents ts
    idents (TThis i)                    = []

instance Ids Type where
    idents (ValT t)                     = idents t
    idents (FunT vs ts t)               = idents (t:ts) \\ vs


class TypeVars a where
    typevars :: a -> [Name]

instance TypeVars a => TypeVars [a] where
    typevars xs                         = concatMap typevars xs

instance TypeVars a => TypeVars (Name,a) where
    typevars (_,x)                      = typevars x

instance TypeVars Bind where
    typevars (Val t e)                  = typevars t ++ typevars e
    typevars (Fun vs t te c)            = (typevars t ++ typevars te ++ typevars c) \\ vs

instance TypeVars Exp where
    typevars (EVar _)                   = []
    typevars (EThis)                    = []
    typevars (ELit _)                   = []
    typevars (ESel e _)                 = typevars e
    typevars (ENew _ ts bs)             = typevars ts ++ typevars bs
    typevars (ECall _ ts es)            = typevars ts ++ typevars es
    typevars (EEnter e _ ts es)         = typevars ts ++ typevars (e:es)
    typevars (ECast t e)                = typevars t ++ typevars e

instance TypeVars Cmd where
    typevars (CRet e)                   = typevars e
    typevars (CRun e c)                 = typevars e ++ typevars c
    typevars (CBind _ bs c)             = typevars bs ++ typevars c
    typevars (CUpd _ e c)               = typevars e ++ typevars c
    typevars (CUpdS e _ e' c)           = typevars [e,e'] ++ typevars c
    typevars (CUpdA e i e' c)           = typevars [e,i,e'] ++ typevars c
    typevars (CSwitch e alts)           = typevars e ++ typevars alts
    typevars (CSeq c c')                = typevars c ++ typevars c'
    typevars (CBreak)                   = []
    typevars (CRaise e)                 = typevars e
    typevars (CWhile e c c')            = typevars e ++ typevars [c,c']
    typevars (CCont)                    = []

instance TypeVars Alt where
    typevars (ACon _ vs te c)           = (typevars c ++ typevars te) \\ vs
    typevars (ALit _ c)                 = typevars c
    typevars (AWild c)                  = typevars c

instance TypeVars AType where
    typevars (TCon _ ts)                = typevars ts
    typevars (TVar n ts)                = n : typevars ts
    typevars (TThis i)                  = []
    
-- Substitutions ------------------------------------------------------------------------------------------

instance Subst Exp Name Exp where
    subst s (EVar v)                    = case lookup v s of
                                            Just e -> e
                                            _      -> EVar v
    subst s (EThis)                     = EThis
    subst s (ELit l)                    = ELit l
    subst s (ESel e l)                  = ESel (subst s e) l
    subst s (ENew x ts bs)              = ENew x ts (subst s bs)
    subst s (ECall x ts es)             = ECall x ts (subst s es)
    subst s (EEnter e x ts es)          = EEnter (subst s e) x ts (subst s es)
    subst s (ECast t e)                 = ECast t (subst s e)

instance Subst Cmd Name Exp where
    subst s (CRet e)                    = CRet (subst s e)
    subst s (CRun e c)                  = CRun (subst s e) (subst s c)
    subst s (CBind r bs c)              = CBind r (subst s bs) (subst s c)
    subst s (CUpd x e c)                = CUpd x (subst s e) (subst s c)
    subst s (CUpdS e x e' c)            = CUpdS (subst s e) x (subst s e') (subst s c)
    subst s (CUpdA e i e' c)            = CUpdA (subst s e) (subst s i) (subst s e') (subst s c)
    subst s (CSwitch e alts)            = CSwitch (subst s e) (subst s alts)
    subst s (CSeq c c')                 = CSeq (subst s c) (subst s c')
    subst s (CBreak)                    = CBreak
    subst s (CRaise e)                  = CRaise (subst s e)
    subst s (CWhile e c c')             = CWhile (subst s e) (subst s c) (subst s c')
    subst s (CCont)                     = CCont

instance Subst Alt Name Exp where
    subst s (ACon x vs te c)            = ACon x vs te (subst s c)              -- NOTE: no alpha-conversion!!
    subst s (ALit l c)                  = ALit l (subst s c)
    subst s (AWild c)                   = AWild (subst s c)
    
instance Subst Bind Name Exp where
    subst s (Val t e)                   = Val t (subst s e)
    subst s (Fun vs t te c)             = Fun vs t te (subst s c)


instance Subst Exp Name Name where
    subst [] e                          = e
    subst s (EVar v)                    = EVar (subst s v)
    subst s (EThis)                     = EThis
    subst s (ELit l)                    = ELit l
    subst s (ESel e l)                  = ESel (subst s e) (subst s l)
    subst s (ENew x ts bs)              = ENew (subst s x) ts (subst s bs)
    subst s (ECall x ts es)             = ECall (subst s x) ts (subst s es)
    subst s (EEnter e x ts es)          = EEnter (subst s e) (subst s x) ts (subst s es)
    subst s (ECast t e)                 = ECast t (subst s e)

instance Subst Cmd Name Name where
    subst [] c                          = c
    subst s (CRet e)                    = CRet (subst s e)
    subst s (CRun e c)                  = CRun (subst s e) (subst s c)
    subst s (CBind r bs c)              = CBind r (subst s bs) (subst s c)
    subst s (CUpd x e c)                = CUpd (subst s x) (subst s e) (subst s c)
    subst s (CUpdS e x e' c)            = CUpdS (subst s e) (subst s x) (subst s e') (subst s c)
    subst s (CUpdA e i e' c)            = CUpdA (subst s e) (subst s i) (subst s e') (subst s c)
    subst s (CSwitch e alts)            = CSwitch (subst s e) (subst s alts)
    subst s (CSeq c c')                 = CSeq (subst s c) (subst s c')
    subst s (CBreak)                    = CBreak
    subst s (CRaise e)                  = CRaise (subst s e)
    subst s (CWhile e c c')             = CWhile (subst s e) (subst s c) (subst s c')
    subst s (CCont)                     = CCont

instance Subst Alt Name Name where
    subst s (ACon x vs te c)            = ACon (subst s x) vs te (subst (prune s vs) c)      -- NOTE: no alpha-conversion!!
    subst s (ALit l c)                  = ALit l (subst s c)
    subst s (AWild c)                   = AWild (subst s c)
    
instance Subst Bind Name Name where
    subst s (Val t e)                   = Val t (subst s e)
    subst s (Fun vs t te c)             = Fun vs t te (subst (prune s (dom te)) c)


instance Subst Type Name AType where
    subst s (ValT t)                    = ValT (subst s t)
    subst s (FunT vs t ts)              = FunT vs (subst s t) (subst s ts)      -- NOTE: no alpha-conversion!!

instance Subst AType Name AType where
    subst s (TCon c ts)                 = TCon c (subst s ts)
    subst s (TVar v ts)                 = case lookup v s of
                                            Just t -> appargs t
                                            _      -> TVar v ts'
      where ts'                         = subst s ts
            appargs (TCon c ts)         = norm c (ts++ts')
            appargs (TVar v ts)         = TVar v (ts++ts')
            appargs (TThis i)           = TThis i

            norm (Prim Class _) [t]     = tClos t [tInt]
            norm (Prim Request _) [t]   = tClos t [tInt]
            norm (Prim Cmd _) [s,t]     = tClos t [tRef s]
            norm c ts                   = TCon c ts
    subst s (TThis i)                   = TThis i

instance Subst Exp Name AType where
    subst s (ESel e l)                  = ESel (subst s e) l
    subst s (ENew x ts bs)              = ENew x (subst s ts) (subst s bs)
    subst s (ECall x ts es)             = ECall x (subst s ts) (subst s es)
    subst s (EEnter e f ts es)          = EEnter (subst s e) f (subst s ts) (subst s es)
    subst s (ECast t e)                 = ECast (subst s t) (subst s e)
    subst s e                           = e

instance Subst Bind Name AType where
    subst s (Val t e)                   = Val (subst s t) (subst s e)
    subst s (Fun vs t te c)             = Fun vs (subst s t) (subst s te) (subst s c)   -- NOTE: no alpha-conversion!!
    
instance Subst Cmd Name AType where
    subst s (CRet e)                    = CRet (subst s e)
    subst s (CRun e c)                  = CRun (subst s e) (subst s c)
    subst s (CBind r bs c)              = CBind r (subst s bs) (subst s c)
    subst s (CUpd x e c)                = CUpd x (subst s e) (subst s c)
    subst s (CUpdS e x e' c)            = CUpdS (subst s e) x (subst s e') (subst s c)
    subst s (CUpdA e i e' c)            = CUpdA (subst s e) (subst s i) (subst s e') (subst s c)
    subst s (CSwitch e alts)            = CSwitch (subst s e) (subst s alts)
    subst s (CSeq c c')                 = CSeq (subst s c) (subst s c')
    subst s (CBreak)                    = CBreak
    subst s (CRaise e)                  = CRaise (subst s e)
    subst s (CWhile e c c')             = CWhile (subst s e) (subst s c) (subst s c')
    subst s (CCont)                     = CCont
    
instance Subst Alt Name AType where
    subst s (ACon x vs te c)            = ACon x vs (subst s te) (subst s c)
    subst s (ALit l c)                  = ALit l (subst s c)
    subst s (AWild c)                   = AWild (subst s c)
    

-- Tentative concrete syntax ------------------------------------------------------------------------------

instance Pr Module where
    pr (Module m ns es ds bs)            =text "module" <+> prId2 m <+> text "where" $$
                                          text "import" <+> hpr ',' ns $$
                                          text "extern" <+> vpr es $$
                                          vpr ds $$ 
                                          vpr bs


instance Pr (Module,a) where
    pr (m,_)                            = pr m


instance Pr (Name, Decl) where
    pr (c, Struct vs te lnk)            = text "struct" <+> prId2 c <+> prTyvars vs <+> text "{" $$
                                          nest 4 (vpr te) $$
                                          text "}" <+> pr lnk

prTyvars []                             = empty
prTyvars vs                             = text "<" <> commasep pr vs <> text ">"

instance Pr Link where
    pr Top                              = empty
    pr Union                            = text "union"
    pr (Extends n ts [])                = text "extends" <+> pr (TCon n ts)
    pr (Extends n ts vs)                = text "extends" <+> pr (TCon n ts) <+> text "hiding" <+> prTyvars vs


instance Pr (Name, Type) where
    pr (x, ValT t)                      = pr t <+> prId2 x <> text ";"
    pr (x, FunT vs ts t)                = prTyvars vs <+> pr t <+> prId2 x <> parens (commasep pr ts) <> text ";"


instance Pr AType where
    pr (TCon (Prim CLOS _) ts0)         = prTyvars vs <+> prId2 (prim CLOS) <> prTyargs (t:ts)
      where (vs,ts,t)                   = decodeCLOS ts0
    pr (TCon c ts)                      = prId2 c <> prTyargs ts
    pr (TVar v ts)                      = prId2 v <> prTyargs ts
    pr (TThis i)                        = text ('#' : show i)

prTyargs []                             = empty
prTyargs ts                             = text "<" <> commasep pr ts <> text ">"

instance Pr (Name, AType) where
    pr (x, t)                           = pr t <+> prId2 x


instance Pr (Name, Bind) where
    pr (x, Val t e)                     = pr t <+> prId2 x <+> text "=" <+> pr e <> text ";"
    pr (x, Fun vs t te c)               = prTyvars vs <+> pr t <+> prId2 x <+> parens (commasep pr te) <+> text "{" $$
                                          nest 4 (pr c) $$
                                          text "}"

instance Pr Cmd where
    pr (CRet e)                         = text "return" <+> pr e <> text ";"
    pr (CRun e c)                       = pr e <> text ";" $$
                                          pr c
    pr (CBind r bs c)                   = vpr bs $$
                                          pr c
    pr (CUpd x e c)                     = prId2 x <+> text "=" <+> pr e <> text ";" $$
                                          pr c
    pr (CUpdS e x e' c)                 = pr (ESel e x) <+> text "=" <+> pr e' <> text ";" $$
                                          pr c
    pr (CUpdA e i e' c)                 = pr (ECall (prim IndexArray) [] [e,i]) <+> text "=" <+> pr e' <> text ";" $$
                                          pr c
    pr (CSwitch e alts)                 = text "switch" <+> parens (pr e) <+> text "{" $$
                                          nest 2 (vpr alts) $$
                                          text "}"
    pr (CSeq c1 c2)                     = pr c1 $$
                                          pr c2
    pr (CBreak)                         = text "break;"
    pr (CRaise e)                       = text "RAISE" <> parens (pr e) <> text ";"
    pr (CWhile e c c')                  = text "while" <+> parens (pr e) <+> text "{" $$
                                          nest 4 (pr c) $$
                                          text "}" $$
                                          pr c'
    pr (CCont)                          = text "continue;"



prScope (CRaise e)                      = pr (CRaise e)
prScope (CBreak)                        = pr CBreak
prScope (CCont)                         = pr CCont
prScope (CRet x)                        = pr (CRet x)
prScope c                               = text "{" <+> pr c $$
                                          text "}"

instance Pr Alt where
    pr (ACon x vs [] c)                 = prId2 x <+> prTyvars vs <> text ":" <+> prScope c
    pr (ACon x vs te c)                 = prId2 x <+> prTyvars vs <+> parens (commasep pr te) <> text ":" <+> prScope c
    pr (ALit l c)                       = pr l <> text ":" <+> prScope c
    pr (AWild c)                        = text "default:" <+> prScope c


instance Pr Exp where
    prn 0 (ECall x [] [e1,e2])
      | isInfix x && isSym x            = prn 1 e1 <+> prId2 x <+> prn 1 e2
    prn 0 (ELit l)                      = prn 0 l
    prn 0 e                             = prn 1 e


    prn 1 (ECall x [] [e])
      | isUnaryOp x && isSym x          = prId2 x <> prn 1 e
    prn 1 (ECast t e)                   = parens (pr t) <> prn 1 e
    prn 1 e                             = prn 2 e

    prn 2 (EVar x)                      = prId2 x
    prn 2 (EThis)                       = text "this"
    prn 2 (ELit l)                      = prn 1 l
    prn 2 (ENew x ts bs)
      | all isVal bs                    = text "new" <+> prId2 x <> prTyargs ts <+> text "{" <> commasep prInit bs <> text "}"
    prn 2 (ENew x ts bs)                = text "new" <+> prId2 x <> prTyargs ts <+> text "{" $$
                                          nest 4 (vpr bs) $$
                                          text "}"
    prn 2 (ECall x ts es)               = prId2 x <> prTyargs ts <> parens (commasep pr es)
    prn 2 (ESel e l)                    = prn 2 e <> text "->" <> prId2 l
    prn 2 (EEnter e x ts es)            = prn 2 e <> text "->" <> prId2 x <> prTyargs ts <> parens (commasep pr es)
    prn 2 e                             = parens (prn 0 e)


prInit (x, Val t e)                     = prId2 x <+> text "=" <+> pr e
prInit b                                = pr b


-- HasPos --------------------

instance HasPos AType where
  posInfo (TCon n ts)           = between (posInfo n) (posInfo ts)
  posInfo (TVar n ts)           = between (posInfo n) (posInfo ts)
  posInfo (TThis i)             = Unknown


-- Binary --------------------
{-
instance Binary Module where
  put (Module a b c d) = put a >> put b >> put c >> put d
  get = get >>= \a -> get >>= \b -> get >>= \c -> get >>= \d -> return (Module a b c d)
-}
instance Binary Decl where
  put (Struct a b c) = putWord8 0 >> put a >> put b >> put c
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> get >>= \c -> return (Struct a b c)
      _ -> fail "no parse"
      
instance Binary Link where
    put Top = putWord8 0
    put Union = putWord8 1
    put (Extends a b c) = putWord8 2 >> put a >> put b >> put c
    get = do
      tag_ <- getWord8
      case tag_ of
        0 -> return Top
        1 -> return Union
        2 -> get >>= \a -> get >>= \b -> get >>= \c -> return (Extends a b c)
        _ -> fail "no parse"
{-
instance Binary Bind where
  put (Val a b) = putWord8 0 >> put a >> put b
  put (Fun a b c) = putWord8 1 >> put a >> put b >> put c
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (Val a b)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (Fun a b c)
      _ -> fail "no parse"
-}
instance Binary Type where
  put (ValT a) = putWord8 0 >> put a
  put (FunT a b c) = putWord8 1 >> put a >> put b >> put c
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (ValT a)
      1 -> get >>= \a -> get >>= \b -> get >>= \c -> return (FunT a b c)
      _ -> fail "no parse"

instance Binary AType where
  put (TCon a b) = putWord8 0 >> put a >> put b
  put (TVar a b) = putWord8 1 >> put a >> put b
  put (TThis a) = putWord8 2 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> get >>= \b -> return (TCon a b)
      1 -> get >>= \a -> get >>= \b -> return (TVar a b)
      2 -> get >>= \a -> return (TThis a)
      _ -> fail "no parse"

