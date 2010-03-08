module PatBinds where

split p [] = ([],[])
split p (x:xs)
  | p x = (x:a,b)
  | otherwise = (a,x:b)
  where (a,b) = split p xs


main f = a
  where (a,b) = split f (a++b)
