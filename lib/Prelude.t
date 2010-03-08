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

module Prelude where

-- IntLiteral ----------------------------------------------

typeclass IntLiteral a where
 fromInt :: Int -> a

instance intInt :: IntLiteral Int where
  fromInt n = n
  
instance intFloat ::IntLiteral Float where
  fromInt = primIntToFloat

default intInt < intFloat

-- Num ----------------------------------------------------- 

typeclass Num a where
 (+),(-),(*) :: a -> a -> a
 negate :: a -> a

instance numInt :: Num Int where
  (+) = primIntPlus
  (-) = primIntMinus
  (*) = primIntTimes
  negate = primIntNeg

instance numFloat :: Num Float where
  (+) = primFloatPlus
  (-) = primFloatMinus
  (*) = primFloatTimes
  negate = primFloatNeg

instance numTime :: Num Time where
  (+) = primTimePlus
  (-) = primTimeMinus
  _ * _ = raise 1
  negate _ = sec 0

-- Eq ------------------------------------------------------

typeclass Eq a where
  (==),(/=) :: a -> a -> Bool

instance eqInt :: Eq Int where
  (==) = primIntEQ
  (/=) = primIntNE

instance eqFloat :: Eq Float where
  (==) = primFloatEQ
  (/=) = primFloatNE

instance eqTime :: Eq Time where
  (==) = primTimeEQ
  (/=) = primTimeNE

instance eqOID :: Eq OID where
  (==) = primPidEQ
  (/=) = primPidNE

instance eqChar :: Eq Char where
  a == b = ord a == ord b
  a /= b = ord a /= ord b

instance eqUnit :: Eq () where
  _ == _ = True
  _ /= _ = False

instance eqList :: Eq [a] \\ Eq a where
    [] == []              = True
    a : as == b : bs  = a == b && as == bs
    _ == _               = False
    xs /= ys             = not ( xs == ys)

instance eqEither :: Eq (Either a b) \\ Eq a, Eq b where
  Left x  == Left y  = x == y
  Right x == Right y = x == y
  _       == _       = False
  x       /= y       = not (x == y)

instance eqPair :: Eq (a,b) \\ Eq a, Eq b where
  (a,b) == (c,d) = a==c && b==d
  x /= y = not (x==y)

instance eqBool :: Eq Bool where
  False == False = True
  True  == True  = True
  _     == _     = False
  x     /= y     = not (x==y)

-- Ord -----------------------------------------------------

typeclass Ord a < Eq a where
  (<),(<=),(>),(>=) :: a -> a -> Bool

instance ordInt :: Ord Int = Ord {..}
  where Eq {..} = eqInt
        (<)  = primIntLT
        (<=) = primIntLE
        (>)  = primIntGT
        (>=) = primIntGE

instance ordFloat :: Ord Float = Ord {..}
  where Eq {..} = eqFloat
        (<)  = primFloatLT
        (<=) = primFloatLE
        (>)  = primFloatGT
        (>=) = primFloatGE

instance ordChar :: Ord Char = struct
        a <  b = ord a <  ord b
        a <= b = ord a <= ord b
        a >  b = ord a >  ord b
        a >= b = ord a >= ord b
        (==) = eqChar.(==)
        (/=) = eqChar.(/=)

instance ordTime :: Ord Time = Ord {..}
  where Eq {..} = eqTime
        (<)  = primTimeLT
        (<=) = primTimeLE
        (>)  = primTimeGT
        (>=) = primTimeGE

instance ordUnit :: Ord () = Ord{..}
  where Eq{..} = eqUnit
        _ < _  = False
        _ <= _ = True
        _ > _  = False
        _ >= _ = True
    
-- Show ----------------------------------------------------
     
typeclass Show a where
  show :: a -> String

instance showInt :: Show Int where
  show 0            = "0"
  show n
    |n < 0          = '-' : show (negate n)
    | otherwise     = reverse (digs n)
    where dig n     = chr (n + ord '0')
          digs n
           | n < 10    = [dig n]
           | otherwise = dig (n `mod` 10) : digs (n `div` 10)
 
instance showFloat :: Show Float where
   show = primShowFloat

instance showBool :: Show Bool where
  show False = "False"
  show True  = "True"

instance showChar :: Show Char where
  show c = [c]

instance showMaybe :: Show (Maybe a) \\ Show a where
  show Nothing      = "Nothing"
  show (Just x)     = "Just (" ++ show x ++ ")"

instance showString :: Show String where
  show s = '"' : s ++ "\""

instance showList :: Show [a] \\ Show a where
  show [] = "[]"
  show (x : xs) = '[' : show x ++ concat (map (\x -> ',' : show x) xs) ++ "]"

instance showTuple :: Show (a,b) \\ Show a, Show b where
  show (a,b) = "("++show a++","++show b++")"

instance showUnit :: Show () where
  show () = "()"

-- Parse ---------------------------------------------------

typeclass Parse a where
  parse :: String -> Either String a

instance parseInt :: Parse Int where
  parse str = p (strip str)
    where p('-':cs) = case q (strip (reverse cs)) of
                        Left err -> Left err
                        Right n -> Right (-n)
          p cs = q (strip (reverse cs))
          q cs
            | all isDigit cs = Right (r cs)
            | otherwise = Left "parseInt: no Parse"
          r (c:cs) = ord c - ord '0' + 10*r cs
          r [] = 0
          strip cs = dropWhile (== ' ') cs
          isDigit c = c >= '0' && c <= '9'

-- Enum ----------------------------------------------------

typeclass Enum a where
  fromEnum :: a -> Int
  toEnum :: Int -> a

instance enumInt :: Enum Int where
  fromEnum n = n
  toEnum n = n

instance enumChar :: Enum Char where
   fromEnum = primCharToInt
   toEnum = primIntToChar

instance enumUnit :: Enum () where
  fromEnum () = 0
  toEnum 0 = ()

instance enumEither :: Enum (Either () a) \\ Enum a where
  fromEnum (Left ()) = 0
  fromEnum (Right a) = 1 + fromEnum a
  toEnum 0 = Left ()
  toEnum n = Right (toEnum (n-1))

-- Functor, Monad and friends ------------------------------

typeclass Functor m where
  ($^)   :: (a -> b) -> m a -> m b

typeclass Applicative m < Functor m where
  ($*)   :: m (a -> b) -> m a -> m b
  pure   :: a -> m a

typeclass Monad m where
  (>>=)  :: m a -> (a -> m b) -> m b
  return :: a -> m a

typeclass MPlus m where
  mempty :: m a
  mappend :: m a -> m a -> m a

instance functorMaybe :: Functor Maybe where
  f $^ Nothing = Nothing
  f $^ Just a  = Just (f a)

instance applicativeMaybe :: Applicative Maybe where
--    Functor {..} = functorMaybe
    ($^) = functorMaybe.($^)
    Just f $* Just a = Just (f a)
    _      $* _      = Nothing
    pure a = Just a

instance monadMaybe :: Monad Maybe where
    Just a  >>= f = f a
    Nothing >>= _ = Nothing
    return a = Just a

instance mPlusMaybe :: MPlus Maybe where
  mempty = Nothing
  Just a  `mappend` _ = Just a
  Nothing `mappend` a = a

instance functorArray :: Functor Array where
  f $^ a = array [f (a!i) | i <- [0..size a-1]]


(>>) :: m a -> m b -> m b \\ Monad m
ma >> mb = ma >>= \_ -> mb

join :: m (m a) -> m a \\ Monad m
join m = m >>= id

sequence []         = return []
sequence (x : xs)   = x >>= \y -> sequence xs >>= \ys -> return (y:ys)

mapM f []           = return []
mapM f (x : xs)     = f x >>= \y -> mapM f xs >>= \ys -> return (y:ys)

instance monadCmd :: Monad (Cmd s) where
    return a = do result a
    a >>= b  = do x <- a
                  y <- b x
                  result y

instance monadClass :: Monad Class where
    return a = class result a
    a >>= b  = class x = new a
                     y = new b x
                     result y

-- Prelude support for forall statement --------------------

forallList f []       = do result []
forallList f (x : xs) = do r <- f x
                           rs <- forallList f xs
                           result (r : rs)

forallSeq :: (a -> Cmd b c) -> a -> a -> Cmd b [c] \\ Enum a
forallSeq f a b = fS (fromEnum a) (fromEnum b)
  where fS ai bi
         | ai>bi = do result []
         | otherwise = do r <- f (toEnum ai)
                          rs <- fS (ai+1) bi
                          result (r : rs)

forallSeq1 :: (a -> Cmd b c) -> a -> a -> a -> Cmd b [c] \\ Enum a
forallSeq1 f a b c = fE ai (bi-ai) ci
  where ai = fromEnum a
        bi = fromEnum b
        ci = fromEnum c
        fE ai bi ci 
          | (if bi > 0 then ai > ci else ai < ci) = do result []
          | otherwise = do r <- f (toEnum ai)
                           rs <- fE (ai+bi) bi ci
                           result (r : rs)

forallListUnit f [] = do result ()
forallListUnit f (x : xs) = do f x
                               rs <- forallListUnit f xs
                               result rs

forallSeqUnit :: (a -> Cmd b c) -> a -> a -> Cmd b () \\ Enum a
forallSeqUnit f a b = fS (fromEnum a) (fromEnum b)
  where fS ai bi
         | ai>bi = do result ()
         | otherwise = do f (toEnum ai)
                          rs <- fS (ai+1)  bi
                          result rs

forallSeq1Unit :: (a -> Cmd b c) -> a -> a -> a -> Cmd b () \\ Enum a
forallSeq1Unit f a b c = fE ai (bi-ai) ci
  where ai = fromEnum a
        bi = fromEnum b
        ci = fromEnum c
        fE ai bi ci 
          | (if bi > 0 then ai > ci else ai < ci) = do result ()
          | otherwise = do f (toEnum ai)
                           rs <- fE (ai+bi) bi ci
                           result rs


-- Prelude support for arithmetic sequences ----------------

enumFromTo          :: a -> a -> [a] \\ Enum a
enumFromTo a b      = map toEnum (fromToInt (fromEnum a) 1 (fromEnum b))

enumFromThenTo a b c = map toEnum (fromToInt ai (bi-ai) ci)
  where  ai = fromEnum a
         bi = fromEnum b
         ci = fromEnum c

fromToInt :: Int -> Int -> Int -> [Int]
fromToInt m s n
       | s > 0     = up m n
       | otherwise = down m n
  where up m n
         | m > n      = []
         | otherwise  = m : up (m+s) n
        down m n
         | m < n      = []
         | otherwise  = m : down (m+s) n

-- Maybe ---------------------------------------------------

data Maybe a = Nothing | Just a

isNothing          :: Maybe a -> Bool
isNothing Nothing   = True
isNothing (Just _)  = False

isJust              :: Maybe a -> Bool
isJust Nothing      = False
isJust (Just _)     = True

fromJust            :: Maybe a -> a
fromJust Nothing    = raise 2
fromJust (Just a)   = a

-- Either --------------------------------------------------

isLeft (Left _)     = True
isLeft _            = False

isRight (Right _)   = True
isRight _           = False

fromRight (Right x) = x

fromLeft (Left x)   = x

-- String --------------------------------------------------

type String         = [Char]

-- Tuples --------------------------------------------------

fst                :: (a,b) -> a
fst (x,_)           = x

snd                :: (a,b) -> b
snd (_,x)           = x

-- List functions ------------------------------------------

head               :: [a] -> a
head (x : _)        = x

tail               :: [a] -> [a]
tail (_ : xs)       = xs

init               :: [a] -> [a]
init [x]           = []
init (x : xs)      = x : init xs

last               :: [a] -> a
last [x]           = x
last (x : xs)      = last xs

(++)               :: [a] -> [a] -> [a]
[] ++ ys            = ys
(x:xs) ++ ys        = x : xs ++ ys

length             :: [a] -> Int
length []           = 0
length (_ : xs)     = 1 + length xs

reverse            :: [a] -> [a]
reverse xs          = rev xs []
  where rev [] ys   = ys
        rev (x : xs) ys
                    = rev xs (x : ys)

map                :: (a -> b) -> [a] -> [b]
map f []            = []
map f (x : xs)      = f x : map f xs

filter             :: (a -> Bool) -> [a] -> [a]
filter p []         = []
filter p (x : xs) 
    | p x           = x : filter p xs
    | otherwise     = filter p xs


foldr              :: ( a -> b -> b) -> b -> [a] -> b
foldr f u []        = u
foldr f u (x : xs)  = f x (foldr f u xs)

foldl              :: (a -> b -> a) -> a -> [b] -> a
foldl f u []        = u 
foldl f u (x : xs)  = foldl f (f u x) xs

concat              = foldr (++) []

zip (x:xs) (y:ys)   = (x,y) : zip xs ys
zip _ _             = []

elem               :: a -> [a] -> Bool \\ Eq a
elem x []           = False
elem x (y : ys)     = x == y || elem x ys

lookup              :: a -> [(a,b)] -> Maybe b \\ Eq a
lookup x []          = Nothing
lookup x ((a,b) : xs) 
     | x == a        = Just b
     | otherwise     = lookup x xs

replicate :: Int -> a -> [a]
replicate n x
  | n <= 0          = []
  | otherwise       = x : replicate (n-1) x

take, drop          :: Int -> [a] -> [a]
take 0 xs           = []
take n []           = []
take n (x : xs) 
        | n > 0     = x : take (n-1) xs

drop 0 xs           = xs
drop n []           = []
drop n (x : xs)
       | n > 0      = drop (n-1) xs
         
takeWhile p []      = []
takeWhile p (x:xs)
  | p x             = x : takeWhile p xs
  | otherwise       = []

dropWhile p []      = []
dropWhile p (x:xs)
  | p x             = dropWhile p xs
  | otherwise       = x:xs

all p []            = True
all p (x : xs)      = p x && all p xs

any p []            = False
any p (x : xs)      = p x || any p xs

-- Combinators ---------------------------------------------

($) :: (a -> b) -> a -> b
f $ a = f a

const :: a -> b -> a
const a _ = a

id :: a -> a
id a = a

flip               :: (a -> b -> c) -> b -> a -> c
flip f x y          = f y x

curry              :: ((a,b) -> c) -> a -> b -> c 
curry f x y         = f (x,y)

uncurry            :: (a -> b -> c) -> (a,b) -> c
uncurry f (x,y)     = f x y

f @ g               = \x -> f (g x)

-- Boolean and numeric functions ---------------------------

not                :: Bool -> Bool
not True            = False
not False           = True

otherwise          :: Bool
otherwise           = True

ord                :: Char -> Int
ord                 = primCharToInt

chr                :: Int -> Char
chr                 = primIntToChar

a ^ 0               = 1
a ^ n 
  | even n          = (a * a) ^ (n `div` 2)
  | otherwise       = a * (a * a) ^ (n `div` 2)

div, mod           :: Int -> Int -> Int
div                 = primIntDiv
mod                 = primIntMod

(/)                :: Float -> Float -> Float
(/)                 = primFloatDiv

even, odd          :: Int -> Bool
even x              = x `mod` 2 == 0
odd x               = x `mod` 2 == 1

gcd                :: Int -> Int -> Int
gcd x y             = gcd' (abs x) (abs y)
  where gcd' a 0
         | a > 0    = a
        gcd' a b    = gcd' b (a `mod` b)

abs x  = case x < 0 of
             True  -> -x
             False -> x


max, min           :: a -> a -> a \\ Ord a
max x y
  | x >= y          = x
  | otherwise       = y

min x y
  | x <= y          = x
  | otherwise       = y

floor               = primFloatToInt

round x             = floor (x + 0.5)

