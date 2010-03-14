module Primes where

import POSIX 

root :: World -> Cmd () ()
root w = do
   env = new posix w

   limit :: Int
   limit = fromRight (parse (env.argv!1))

   primesCounter = new primes limit
   ct <- primesCounter
   env.stdout.write (show ct++"\n")
   env.exit 0


primes limit = class
   primesBound = limit `div` log3 limit

   primes := uniarray primesBound 0
   count  := 0

   isPrime k = loop 0
      where loop n = do 
              p = primes!n
              if p*p > k then
                 result True
              elsif k `mod` p  == 0 then
                 result False
              else
                 r <- loop (n+1)
                 result r

   checkFrom k = do
     p <- isPrime k
     if p then 
        primes!count := k
        count := count + 1
     if k < limit then checkFrom (k+1)

   result request
     primes!0 := 2
     count := 1
     checkFrom 3
     result count


log3 :: Int -> Int
log3 n  
  | n < 3       = 0
  | otherwise   = 1 + log3 (n `div` 3)
