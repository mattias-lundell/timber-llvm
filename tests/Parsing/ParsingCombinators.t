module ParsingCombinators where

-- self-optimizing parsing combinators

import POSIX
import State

type Token = Char
type Bounds = (Token,Token)

-- parser type

struct P a where
  arr :: TArray Token (Maybe (State [Token] (Maybe a)))
  emptyParse :: Maybe a

-- run a parser

parseP :: P a -> [Token] -> ([Token],Maybe a)
parseP p s = runState (parse' p) s

-- elementary combinators

token :: Token -> P Token
token t = struct
  arr = tarray t [Just (return (Just t))]
  emptyParse = Nothing

accept :: (Token -> Bool) -> P Token
accept p = struct
  arr = tarray t0 [ if p t then Just (return (Just t)) else Nothing
                  | t <- [t0..t1] ]
     where t0 = ' '
           t1 = 'z'
  emptyParse = Nothing

instance functorP :: Functor P where
  g $^ a = struct
    arr = (((g $^) $^) $^) $^ a.arr
    emptyParse = g $^ a.emptyParse

instance applicativeP :: Applicative P where
  ($^) = functorP.($^)
  f $* a = struct
    arr = combineArraySeq f a
    -- The following needs to be expanded so that we
    -- demand 'a' only if 'f' can accept an empty parse.
    -- emptyParse = f.emptyParse $* a.emptyParse
    -- (When we have the delay rule, we don't need this expansion.)
    emptyParse = case f.emptyParse of Nothing -> Nothing
                                      Just f' -> f' $^ a.emptyParse
  pure a = struct
    arr = tarray ' ' []
    emptyParse = Just a

-- choice

($+) :: P a -> P a -> P a
a $+ b = struct
  arr = combineArrayPar a b
  emptyParse = if isNothing a.emptyParse || isNothing b.emptyParse 
               then a.emptyParse `mappend` b.emptyParse
               else raise ambiguousParse

-- derived combinators

many :: P a -> P [a]
many p = mp where mp = pure (F$ \a -> F$ \l -> a:l) $** p $** mp
                    $+ pure []

choice :: [P a] -> P a
choice (x:xs) = foldr ($+) x xs

parens :: P a -> P a
parens p = pure (F$ \_ -> F$ \a -> F$ \_ -> a) $** 
           token '(' $** p $** token ')'

-- workaround for compiler limitation.

data F a = F a
unF (F a) = a

($**) :: m (F (a -> b)) -> m a -> m b \\ Applicative m
a $** b = unF $^ a $* b

-- helper functions

parse' :: P a -> State [Token] (Maybe a)
parse' p =  
  get >>= \ts ->
  case ts of 
    t:ts' -> case tindex p.arr t of
               Just (Just f) -> 
                 set ts' >>
                 f >>= \ma ->
                 case ma of
                   Just a -> pure (Just a)
                   Nothing -> set ts >>
                              pure p.emptyParse
               _ -> return p.emptyParse
    [] -> pure p.emptyParse

combineArraySeq :: P (a -> b) -> P a -> 
                   TArray Token (Maybe (State [Token] (Maybe b)))
combineArraySeq fp ap = 
  case fp.emptyParse of
    Nothing -> ((\ft -> 
                  ft            >>= \mf ->
                  case mf of 
                    Nothing -> pure Nothing
                    Just f -> 
                      parse' ap >>= \ma ->
                      pure (f $^ ma))$^) $^ fp.arr
    Just f -> combineTArray cf fp.arr ap.arr where
      cf Nothing    Nothing   = Nothing
      cf Nothing    (Just at) = Just ((f$^) $^ at)
      cf (Just ft') Nothing   = (\a -> (($a) $^) $^ ft') $^ ap.emptyParse
      cf (Just ft') (Just at) = Just ((ft' >>= \mf ->
                                      case mf of 
                                        Nothing -> pure Nothing
                                        Just f -> (f$^) $^ at)
                                    `altState`
                                      ((f$^) $^ at))

combineTArray :: (Maybe a -> Maybe b -> Maybe c) -> 
                 TArray t (Maybe a) -> 
                 TArray t (Maybe b) -> 
                 TArray t (Maybe c)                 \\ Enum t, Ord t
combineTArray f aa ba = 
  tarray c1 [ f (join (tindex aa t))
                (join (tindex ba t))
            | t <- [c1..c2] ]
  where (c1,c2) = (min a1 b1, max a2 b2)
        (a1,a2) = bounds aa
        (b1,b2) = bounds ba

ambiguousParse = 0

combineArrayPar :: P a -> P a -> TArray Token (Maybe (State [Token] (Maybe a)))
combineArrayPar ap bp = combineTArray cf ap.arr bp.arr where
  cf Nothing Nothing     = Nothing
  cf (Just at) Nothing   = Just at
  cf Nothing (Just bt)   = Just bt
  cf (Just at) (Just bt) = Just (at `altState` bt)

altState :: State s (Maybe a) -> State s (Maybe a) -> State s (Maybe a)
altState m1 m2 = 
  get >>= \s ->
  m1 >>= \ma ->
  case ma of
    Just a -> return (Just a)
    Nothing -> set s >> m2

-- arrays indexed with any t in Enum.

struct TArray t e where
  ar :: Array e
  t0 :: t

instance functorTArray :: Functor (TArray t) where
  f $^ a = TArray {..} where t0 = a.t0
                             ar = f $^ a.ar

tarray :: t -> [e] -> TArray t e
tarray t es = struct
  ar = array es 
  t0 = t

tsize :: TArray t e -> Int
tsize t = size t.ar

bounds :: TArray t e -> (t,t) \\ Enum t
bounds a = (a.t0,toEnum (fromEnum a.t0 + tsize a - 1))

mindex :: Array e -> Int -> Maybe e
mindex a o | o >= 0 && o < size a = Just (a!o)
           | otherwise            = Nothing

tindex :: TArray t e -> t -> Maybe e \\ Enum t
tindex a o = mindex a.ar (fromEnum o - fromEnum a.t0)

