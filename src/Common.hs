{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, DeriveDataTypeable, OverlappingInstances, FlexibleContexts #-}

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

module Common (module Common, module Name, isDigit) where

import PP
import qualified List
import qualified Maybe
import Monad(liftM2)
import Control.Exception
import Char
import Config
import Name
import Data.Typeable
import Data.Binary
import Debug.Trace



fromJust                        = Maybe.fromJust
isJust                          = Maybe.isJust
listToMaybe                     = Maybe.listToMaybe

fst3 (a,b,c)                    = a
snd3 (a,b,c)                    = b
thd3 (a,b,c)                    = c

dom                             = map fst

rng                             = map snd

partition p xs                  = List.partition p xs

nub xs                          = List.nub xs

xs \\ ys                        = filter (`notElem` ys) xs

xs `intersect` ys               = xs `List.intersect` ys

xs `union` ys                   = xs `List.union` ys

disjoint xs ys                  = xs `intersect` ys == []

overlaps xs ys                  = not (disjoint xs ys)

intersperse x xs                = List.intersperse x xs

duplicates xs                   = filter (`elem` dups) xs1
  where xs1                     = nub xs
        dups                    = foldl (flip List.delete) xs xs1

rotate n xs                     = let (xs1,xs2) = splitAt n xs in xs2++xs1

separate []                     = ([],[])
separate (Left x : xs)          = let (ls,rs) = separate xs in (x:ls,rs)
separate (Right x : xs)         = let (ls,rs) = separate xs in (ls,x:rs)

showids vs                      = concat (intersperse ", " (map show vs))

fmapM f g xs                    = do ys <- mapM g xs
                                     return (f ys)

mapFst f xs                     = [ (f a, b) | (a,b) <- xs ]

mapSnd f xs                     = [ (a, f b) | (a,b) <- xs ]

zipFilter (f:fs) (x:xs)
  | f                           = x : zipFilter fs xs
  | otherwise                   = zipFilter fs xs
zipFilter _ _                   = []

zip4                            = List.zip4
                                    
noDups mess vs
  | not (null dups)             = errorIds mess dups
  | otherwise                   = vs
  where dups                    = duplicates vs

uncurry3 f (x,y,z)              = f x y z


-- String manipulation -----------------------------------------------------

rmSuffix                        :: String -> String -> String
rmSuffix suf                    = reverse . rmPrefix (reverse suf) . reverse

rmPrefix                        :: String -> String -> String
rmPrefix pre str 
  | pre `List.isPrefixOf` str   = drop (length pre) str
  | otherwise                   = internalError0 $ "rmPrefix: " ++ str ++ " is not a prefix of " ++ show pre

rmDirs                          :: String -> String
rmDirs                          = reverse . fst . span (/='/') . reverse 

dropPrefix [] s                 = (True, s)
dropPrefix (x:xs) (y:ys)
  | x == y                      = dropPrefix xs ys
dropPrefix xs ys                = (False, ys)


dropDigits xs                   = drop 0 xs
  where drop n (x:xs)
          | isDigit x           = drop (10*n + ord x - ord '0') xs
        drop n xs               = (n, xs)



-- Error reporting ---------------------------------------------------------

errorIds mess ns                = compileError (unlines ((mess++":") : map pos ns))
  where pos n                   = case loc n of
                                    Just (r,c) -> rJust 15 (show n) ++ "  at line " ++ show r ++ ", column " ++ show c
                                    Nothing ->    rJust 15 (show n) ++ modInfo n
        loc n                   = location (annot n)
        rJust w str             = replicate (w-length str) ' ' ++ str
        modInfo (Name _ _ (Just m) a) = " defined in " ++ m
        modInfo _               = " (unknown position)"
errorTree mess t                = compileError (errorMsg mess t)

compileError mess               = throw (CompileError mess)

errorMsg mess t                 = mess ++ pos ++ (if length (lines str) > 1 then "\n"++str++"\n" else str)
  where str                     = render (pr t)
        pos                     = " ("++ show (posInfo t) ++"): "
 
internalError mess t            = internalError0 (errorMsg mess t)

internalError0 mess             = throw (Panic mess)


-- PosInfo ---------------------------------------------------------
   

data PosInfo                    = Between {start :: (Int,Int), end :: (Int,Int)}
                                | Unknown

instance Show PosInfo where
   show (Between (l1,c1) (l2,c2)) 
                                = case l1==l2 of
                                    True ->  case c1 == c2 of
                                              True -> "close to line "++show l1++", column "++show c1
                                              False -> "close to line "++show l1++", columns "++show c1++" -- "++show c2
                                    False -> "close to lines "++show l1++" -- "++show l2
   show Unknown                 = "at unknown position"



between (Between s1 e1) (Between s2 e2) 
                                = Between (min s1 s2) (max e1 e2)
between b@(Between _ _) Unknown = b
between Unknown b@(Between _ _) = b
between Unknown Unknown         = Unknown 

startPos (Between s _)          = Just s
startPos Unknown                = Nothing

class HasPos a where
  posInfo :: a -> PosInfo

instance HasPos a => HasPos [a] where
  posInfo xs = foldr between Unknown (map posInfo xs)

instance (HasPos a, HasPos b) => HasPos (a,b) where
  posInfo (a,b) = between (posInfo a) (posInfo b)

instance HasPos a => HasPos (Maybe a) where
  posInfo Nothing  = Unknown
  posInfo (Just a) = posInfo a

instance HasPos Bool where
  posInfo _ = Unknown

instance HasPos Name where
   posInfo n = case location (annot n) of
                 Just (l,c)
                    |l==0 && c==0 -> Unknown  -- artificially introduced name
                    |otherwise ->    Between (l,c) (l,c+len n-1)
                 Nothing    -> Unknown
               where len(Name s _ _ _) = length s
                     len(Prim p _)     = length (strRep p)
                     len(Tuple n _)    = n+2

-- Literals ----------------------------------------------------------------

data Lit                        = LInt  (Maybe (Int,Int)) Integer
                                | LRat  (Maybe (Int,Int)) Rational
                                | LChr  (Maybe (Int,Int)) Char
                                | LStr  (Maybe (Int,Int)) String
--                                deriving  (Eq)

instance Eq Lit where
  LInt _ m == LInt _ n = m == n
  LRat _ m == LRat _ n = m == n
  LChr _ m == LChr _ n = m == n
  LStr _ m == LStr _ n = m == n
  _ == _ = False

instance Show Lit where
    show (LInt _ i)             = "LInt " ++ show i
    show (LRat _ r)             = "LRat " ++ show r
    show (LChr _ c)             = "LChr " ++ show c
    show (LStr _ s)             = "LStr " ++ show s
    
instance Pr Lit where
    prn _ (LInt _ i)            = integer i
    prn 0 (LRat _ r)            = rational r
    prn _ (LRat _ r)            = parens (rational r)
    prn _ (LChr _ c)            = litChar c
    prn _ (LStr _ s)            = litString s


instance HasPos Lit where
    posInfo (LInt (Just (l,c)) i) = Between (l,c) (l,c+length(show i)-1) 
    posInfo (LRat (Just (l,c)) r) = Between (l,c) (l,c) -- check length of rationals) 
    posInfo (LChr (Just (l,c)) _) = Between (l,c) (l,c) 
    posInfo (LStr (Just (l,c)) cs) = Between (l,c) (l,c+length cs+1)
    posInfo _                     = Unknown

lInt n                            = LInt Nothing (toInteger n)
lRat r                            = LRat Nothing r
lChr c                            = LChr Nothing c
lStr s                            = LStr Nothing s

-- Underlying monad ----------------------------------------------------------------------

newtype M s a                   = M ((Int,[s]) -> Either String ((Int,[s]), a))


instance Functor (M s) where
    fmap f x                    = x >>= (return . f)

instance Monad (M s) where
    M m >>= f                   = M $ \k -> 
                                    case m k of 
                                        Right (k',a) -> m' k' where M m' = f a
                                        Left s -> Left s
    return a                    = M $ \k -> Right (k,a)
    fail s                      = M $ \k -> Left s

handle (M m) f                  = M $ \k ->
                                    case m k of
                                        Right r -> Right r
                                        Left s  -> m' k where M m' = f s

expose (M m)                    = M $ \k ->
                                    case m k of
                                        Right (k',a) -> Right (k',Right a)
                                        Left s       -> Right (k, Left s)

unexpose (Right a)              = return a
unexpose (Left b)               = fail b

runM (M m)                      = case m (1,[]) of 
                                    Right (_,x) -> x
                                    Left s      -> compileError s

newNum                          = M $ \(n,s) -> Right ((n+1,s), n)

currentNum                      = M $ \(n,s) -> Right ((n,s), n)

addToStore x                    = M $ \(n,s) -> Right ((n,x:s), ())

currentStore                    = M $ \(n,s) -> Right ((n,s), s)

localStore (M m)                = M $ \(n0,s0) ->
                                    case m (n0,[]) of
                                      Right ((n,s), x) -> Right ((n,s0), x)
                                      Left s           -> Left s

newNameMod m s                  = do n <- newNum
                                     return (Name s n m ann)
  where ann                     = if s `elem` explicitSyms then suppAnnot { explicit = True } else suppAnnot
        suppAnnot               = genAnnot { suppress = True }

newNameModPub m pub s           = do n <- newNameMod m s
                                     return (n {annot = (annot n) {public = pub}})
newName s                       = newNameMod Nothing s

newNames s n                    = mapM (const (newName s)) [1..n]

newNamesPos s ps                = mapM (newNamePos s) ps

newNamePos s p                  = do n <- newName s
                                     return (n {annot = (annot n) {location = startPos(posInfo p)}})

renaming vs                     = mapM f vs
  where f v | tag v == 0        = do n <- newNum
                                     return (v, v { tag = n })
            | otherwise         = return (v, v)


-- Merging renamings ------------------------------------------------------

-- Here we cannot use equality of names (Eq instance), since two unqualified
-- imported names with the same string will have non-zero tags and hence not be compared
-- for str equality.

-- remove pairs from rn2 that are shadowed by rn1; return also shadowed names
deleteRenamings [] rn2           = (rn2,[])
deleteRenamings ((n,_):rn1) rn2
  | not (isQualified n)         = (rn',if b then n:ns else ns)
  | otherwise                   = (rn,ns)
  where (rn,ns)                 = deleteRenamings rn1 rn2
        (b,rn')                 = deleteName n rn
        
deleteName _ []                 = (False,[])
deleteName n ((Name s t Nothing a,_):rn) 
  | str n == s                  = (True,rn)
deleteName n (p:rn)             = let (b,rn') = deleteName n rn
                                  in  (b,p:rn')

-- for merging renaming for locally bound names with ditto for imported names;
-- removes unqualified form of imported name
mergeRenamings1 rn1 rn2         = rn1 ++ rn2' 
  where (rn2',_)                = deleteRenamings rn1 rn2

-- for merging renamings from two imported modules;
-- removes both occurrences when two unqualified names clash
mergeRenamings2 rn1 rn2         = rn1' ++ rn2'
  where (rn2',ns)               = deleteRenamings rn1 rn2
        rn1'                    = deleteNames ns rn1
        ns'                     = filter (not . isGenerated) ns
        deleteNames [] rn       = rn
        deleteNames (n:ns) rn   = deleteNames ns (snd (deleteName n rn))

-- Assertions -----------------------------------------------------------------------

assert e msg ns
  | e                           = return ()
  | otherwise                   = errorIds msg ns

assert1 e msg ts
  | e                           = return ()
  | otherwise                   = errorTree msg ts


-- Poor man's exception datatype ------------------------------------------------------

encodeError msg ids             = msg ++ ": " ++ concat (intersperse " " (map packName ids))

decodeError str
  | msg `elem` encodedMsgs      = Just (msg, map unpackName (words rest))
  | otherwise                   = Nothing
  where (msg,_:rest)            = span (/=':') str

encodedMsgs                     = [circularSubMsg, ambigInstMsg, ambigSubMsg]

circularSubMsg                  = "Circular subtyping"
ambigInstMsg                    = "Ambiguous instances"
ambigSubMsg                     = "Ambiguous subtyping"

assert0 e msg
  | e                           = return ()
  | otherwise                   = fail msg


-- Tracing -----------------------------------------------------------------------------

tr m                            = trace (m++"\n") (return ())

tr' m e                         = trace ("\n"++m++"\n") e

trNum str                       = do n <- currentNum
                                     tr ("At "++show n++": "++str)

-- Free variables -----------------------------------------------------------------------

class Ids a where
    idents :: a -> [Name]

instance Ids a => Ids [a] where
    idents xs                   = concatMap idents xs

instance Ids a => Ids (Name,a) where
    idents (v,a)                = idents a

tycons x                        = filter isCon (idents x)

tyvars x                        = filter isVar (idents x)

evars x                         = filter isVar (idents x)

svars x                         = filter isState (idents x)


vclose vss vs
  | null vss2                   = nub vs
  | otherwise                   = vclose vss1 (concat vss2 ++ vs)
  where (vss1,vss2)             = partition (null . intersect vs) vss



-- Bound variables -----------------------------------------------------------------------

class BVars a where
    bvars  :: a -> [Name]
    
    bvars _                     = []


-- Mappings -----------------------------------------------------------------------------

infixr 4 @@

type Map a b = [(a,b)]

lookup' assoc x                 = case lookup x assoc of
                                    Just e -> e
                                    Nothing -> internalError "lookup': did not find" x

lookup'' s assoc x              = case lookup x assoc of
                                    Just e -> e
                                    Nothing -> internalError ("lookup' (" ++ s ++ "): did not find") x

inv assoc                       = map (\(a,b) -> (b,a)) assoc

delete k []                     = []
delete k (x:xs)
  | fst x == k                  = xs
  | otherwise                   = x : delete k xs


delete' ks xs                   = foldr delete xs ks


insert k x []                   = [(k,x)]
insert k x ((k',x'):assoc)
  | k == k'                     = (k,x) : assoc
  | otherwise                   = (k',x') : insert k x assoc


update k f []                   = internalError0 "Internal: Common.update"
update k f ((k',x):assoc)
  | k == k'                     = (k, f x) : assoc
  | otherwise                   = (k',x) : update k f assoc


search p []                     = Nothing
search p (a:assoc)
  | p a                         = Just a
  | otherwise                   = search p assoc


insertBefore kx ks []           = [kx]
insertBefore kx ks ((k,x'):assoc)
  | k `elem` ks                 = kx:(k,x'):assoc
  | otherwise                   = (k,x') : insertBefore kx ks assoc




(@@)                            :: Subst b a b => Map a b -> Map a b -> Map a b
s1 @@ s2                        = [(u,subst s1 t) | (u,t) <- s2] ++ s1


merge                           :: (Eq a, Eq b) => Map a b -> Map a b -> Maybe (Map a b)
merge [] s'                     = Just s'
merge ((v,t):s) s'              = case lookup v s' of
                                    Nothing         -> merge s ((v,t):s')
                                    Just t' | t==t' -> merge s s'
                                    _               -> Nothing

nullSubst                       = []

a  +-> b                        = [(a,b)]

restrict s vs                   = filter ((`elem` vs) . fst) s

prune s vs                      = filter ((`notElem` vs) . fst) s

class Subst a i e where
    subst :: Map i e -> a -> a


substVars s xs                  = map (substVar s) xs

substVar s x                    = case lookup x s of
                                    Just x' -> x'
                                    Nothing -> x


instance Subst Name Name Name where
    subst s x                           = case lookup x s of
                                            Just x' -> x'
                                            _       -> x

instance Subst a i e => Subst [a] i e where
    subst [] xs                 = xs
    subst s xs                  = map (subst s) xs

instance Subst a Name Name => Subst (Name,a) Name Name where
    subst s (v,a)               = (subst s v, subst s a)

instance Subst a i e => Subst (Name,a) i e where
    subst s (v,a)               = (v, subst s a)

instance Subst a i e => Subst (Maybe a) i e where
    subst s Nothing             = Nothing
    subst s (Just a)            = Just (subst s a)


newEnv x ts                     = do vs <- mapM (const (newName x)) ts
                                     return (vs `zip` ts)

newEnvPos x ts e                = do vs <- mapM (const (newNamePos x e)) ts
                                     return (vs `zip` ts)



-- Kinds ---------------------------------------------------------------------------------

data Kind                       = Star
                                | KFun    Kind Kind
                                | KWild
                                | KVar    Int
                                deriving (Eq,Show)

type KEnv                       = Map Name Kind

instance HasPos Kind where
  posInfo _                     = Unknown

newKVar                         = do n <- newNum
                                     return (KVar n)

kvars Star                      = []
kvars (KVar n)                  = [n]
kvars (KFun k1 k2)              = kvars k1 ++ kvars k2

kArgs (KFun k k')               = k : kArgs k'
kArgs k                         = []

kFlat k                         = (kArgs k, kRes k)

kRes (KFun k k')                = kRes k'
kRes k                          = k


instance Subst Kind Int Kind where
    subst s Star                = Star
    subst s k@(KVar n)          = case lookup n s of
                                    Just k' -> k'
                                    Nothing -> k
    subst s (KFun k1 k2)        = KFun (subst s k1) (subst s k2)

instance Subst (Kind,Kind) Int Kind where
    subst s (a,b)               = (subst s a, subst s b)

instance Pr (Name,Kind) where
    pr (n,k)                    = prId n <+> text "::" <+> pr k

instance Pr Kind where
    prn 0 (KFun k1 k2)          = prn 1 k1 <+> text "->" <+> prn 0 k2
    prn 0 k                     = prn 1 k
    prn 1 Star                  = text "*"
    prn 1 (KVar n)              = text ('_':show n)
    prn 1 KWild                 = text "_"
    prn 1 k                     = parens (prn 0 k)


-- Defaults ------------------------------------------

data Default a = Default Bool Name Name  -- First arg is True if declaration is in public part
               | Derive Name a
             deriving (Eq, Show)

instance Pr a => Pr(Default a) where
  pr (Default _ a b)            = pr a <+> text "<" <+> pr b
  pr (Derive v t)               = pr v <+> text "::" <+> pr t

instance HasPos a => HasPos (Default a) where
  posInfo (Default _ a b)       = between (posInfo a) (posInfo b)
  posInfo (Derive v t)          = between (posInfo v) (posInfo t)

-- Externals -----------------------------------------

data Extern a = Extern Name a
             deriving (Eq, Show)

instance Pr a => Pr (Extern a) where
  pr (Extern n t)               = prId n <+> text "::" <+> pr t

instance BVars [Extern a] where
    bvars (Extern n _ : es)     = n : bvars es
    bvars []                    = []

instance Binary a => Binary (Extern a) where
  put (Extern a b) =  put a >> put b 
  get = get >>= \a -> get >>= \b -> return (Extern a b) 

extsMap es = map (\(Extern v t) -> (v,t)) es


-- Pattern matching ---------------------------------------------------------------

 -- The M prefix allows coexistence with older PMC primites
data Match p e bs r = MCommit r
                    | MFail
                    | MFatbar (Match p e bs r) (Match p e bs r)
                    | Case e [(p,Match p e bs r)]
                    | Let bs (Match p e bs r)
                    deriving (Eq,Show)

alt p rhs = (p,rhs)

foldMatch commit fail fatbar mcase mlet = f
  where f m = case m of
                MCommit r     ->  commit r
                MFail         ->  fail
                MFatbar m1 m2 ->  fatbar (f m1) (f m2)
                Case e alts   -> mcase e (mapSnd f alts)
                Let bs m      -> mlet bs (f m)

mapMatch pf ef bsf rf = foldMatch commit fail fatbar mcase mlet
  where
    commit r      = MCommit (rf r)
    fail          = MFail
    fatbar        = MFatbar
    mcase  e alts = Case (ef e) (mapFst pf alts)
    mlet  bs e    = Let (bsf bs) e

mapMMatch pf ef bsf rf = foldMatch commit fail fatbar mcase mlet
  where
    commit r      = MCommit `fmap` rf r
    fail          = return MFail
    fatbar        = liftM2 MFatbar
    mcase  e alts = liftM2 Case (ef e) (sequence [liftM2 (,) (pf p) m|(p,m)<-alts])
    mlet  bs e    = liftM2 Let (bsf bs) e

instance (Ids p,Ids e, Ids bs,Ids r,BVars bs) => Ids (Match p e bs r) where
  idents m =
    case m of
      MCommit r     -> idents r
      MFail         -> []
      MFatbar m1 m2 -> idents m1 ++ idents m2
      Case e alts   -> idents e ++ concat [idents m\\idents p|(p,m)<-alts]
      Let bs m      -> idents bs ++ (idents m \\ bvars bs)

instance (Subst e n e,Subst bs n e,Subst r n e) => Subst (Match p e bs r) n e where
  subst s = mapMatch id (subst s) (subst s) (subst s)

instance (Pr p, Pr e,Pr bs,Pr r) => Pr (Match p e bs r) where
    prn  0 (MFatbar m1 m2) = text "Fatbar" <+> prn 12 m1 <+> prn 12 m2
    prn  0 (Case e alts)   = text "case" <+> pr e <+> text "of" $$
                             nest 2 (vcat (map pralt alts))
      where pralt (p,m)    = pr p <+> text "->" <+> pr m
    prn  0 (Let bs m)      = text "let" <+> pr bs $$ text "in" <+> pr m
    prn  0 m               = prn 11 m
    prn 11 (MCommit r)     = text "Commit" <+> prn 12 r
    prn 11 m               = prn 12 m
    prn 12 m               = prn 13 m
    prn 13 MFail           = text "Fail"
    prn 13 m               = parens (prn 0 m)

    prn  n e               = prn 11 e

instance (HasPos p, HasPos e,HasPos bs,HasPos r) => HasPos (Match p e bs r) where
  posInfo m =
    case m of
      MCommit r     -> posInfo r
      MFail         -> Unknown
      MFatbar m1 m2 -> posInfo (m1,m2)
      Case e alts   -> posInfo (e,alts)
      Let bs m      -> posInfo (bs,m)

-- Binary --------------------------------------------

instance Binary Lit where
  put (LInt _ a) = putWord8 0 >> put a
  put (LRat _ a) = putWord8 1 >> put a
  put (LChr _ a) = putWord8 2 >> put a
  put (LStr _ a) = putWord8 3 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> get >>= \a -> return (lInt (a::Integer))
      1 -> get >>= \a -> return (lRat a)
      2 -> get >>= \a -> return (lChr a)
      3 -> get >>= \a -> return (lStr a)
      _ -> fail "no parse"

instance Binary Kind where
  put Star       = putWord8 0
  put (KFun a b) = putWord8 1 >> put a >> put b
  put KWild      = putWord8 2
  put (KVar a)   = putWord8 3 >> put a
  get = do
    tag_ <- getWord8
    case tag_ of
      0 -> return Star
      1 -> get >>= \a -> get >>= \b -> return (KFun a b)
      2 -> return KWild
      3 -> get >>= \a -> return (KVar a)
      _ -> fail "no parse"

instance Binary a => Binary (Default a) where
  put (Default a b c) = putWord8 0 >> put a >> put b >> put c
  put (Derive a b) = putWord8 1 >> put a >> put b
 
  get = do
    tag_ <- getWord8
    case tag_ of
       0 -> get >>= \a -> get >>= \b -> get >>= \c -> return (Default a b c)
       1 -> get >>= \a ->  get >>= \b -> return (Derive a b)
