module Rank2 where

ff :: (a -> a \\ a) -> Int -> Char -> (Int,Char)
ff g i c = ff g i c

main = ff id 2 'c'

gg :: b -> b \\ b
gg x = gg x

m2 = gg 'x'
