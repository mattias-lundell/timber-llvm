module FloatFoldr where

foldr f u []        = u
foldr f u (x : xs)  = f x (foldr f u xs)

n = foldr (+) 0.0 [1.0]

