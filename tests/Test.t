module Test where

-- f x = let (a,b) = x
--      in a+b

-- g 'c' = 'd'
-- g x
--  | x == 'a'  = 'a'
--  | otherwise = 'b'
{-  
split p [] = ([],[])
split p (x:xs)
  | p x = (x:a,b)
  | otherwise = (a,x:b)
  where (a,b) = split p xs


main f = a
  where (a,b) = split f (a++b)
-}
{-
fff xs = (ys,ys)
  where ys = xs ++ xs
-}
{-
ff :: (a -> a \\ a) -> Int -> Char -> (Int,Char)
ff g i c = ff g i c

main = ff id 2 'c'

gg :: b -> b \\ b
gg x = gg x

m2 = gg 'x'
-}

apa = 9
