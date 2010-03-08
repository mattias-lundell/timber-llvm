module Synstest where


-- Tests for unfolding of type synonyms

k :: H Int (List C) (Rec D)
k = H 0 [] []

type B = (Int,A)

f :: B -> A
f (x,y) = y

type List a = [a]

type Lista = []

nil :: Lista Int
nil = []

type C = List Int

g :: C -> List B
g [x] = [(x,x)]

type Pair a b = (a,b)

type D = Pair Int Int

h :: D -> C
h (x,y) = [x]

typeclass Num2 a where
  (.+) :: Fun a a B


type String = [Char]

type Rec a = [Circ a]
data Circ a = Tag [Rec a]

type Fun a b c = a -> b -> c

map :: Fun (a -> b) (List a) [b]
map f [] = []
map f (x : xs) = f x : map f xs

x = (1,2) :: B

type A = Int

struct F where
  x :: G

type G = F

t = F {x = t}

data H a b c = H a b c

type K = Lista (Rec F)



{- Erroneous cases:

Recursive type synonym
type E = E

Mutual recursion
type R = T
type T = R

Partial Application
type E = Pair Int

Duplicate defs
type A = Bool


-}

