module Derive where

{-
data A a = C1 T1 a (A a)
         | C2 (U1 a) (U2 -> U3)
         | C3 a
         | C4


data T  =  T
data T1 = T1

data U1 a = U1 a
data U2 = U2
data U3 = U3

implicit eqPair :: Eq (a,b) \\ Eq a, Eq b
eqPair = struct
  (a,b) == (c,d) = a==c && b==d
  x     /= y     = not (x == y)

implicit eqEither :: Eq (Either a b) \\ Eq a, Eq b
eqEither = struct
  Left x == Left y = x==y
  Right x == Right y = x==y
  _ == _ = False
  x /= y = not(x == y)
-}

data F = F1 | F2

-- data D a b = C1 (Int->Int) a b | C2 b

data E a = C5 | C6 Int a | C7 Int a Int a

--data G = G Int Int

default eqE :: Eq (E a) \\ Eq a


default enumF :: Enum F
xs = [F1 .. F2]
