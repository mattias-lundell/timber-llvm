module Fac where

fac 0 = 1
fac n = n * fac (n-1)

root [] = show (fac 4)
root (arg:args) = arg ++ "\n" ++ root args


