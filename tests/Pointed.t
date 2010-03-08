module Pointed where

typeclass Pointed a where
  p :: a

instance pointedUnit :: Pointed ()
pointedUnit = { p = () }

instance pointedPair :: Pointed (a,b) \\ Pointed a, Pointed b
pointedPair = struct
  p = (p,p)

instance pointedEitherL :: Pointed (Either a b) \\ Pointed a
pointedEitherL = struct
  p = Left p

instance pointedChar :: Pointed Char
pointedChar = { p = '0' }

data D a = D1 a Char Char 
         | D2
         | D3 Char Char Int

default pointedD :: Pointed (D a) \\ Pointed a

