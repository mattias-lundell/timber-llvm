module Guards where

f x = let (a,b) = x
      in a+b

g 'c' = 'd'
g x
  | x == 'a'  = 'a'
  | otherwise = 'b'
