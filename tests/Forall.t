module Forall where

import POSIX

struct Cell where
  depth :: Request Int
  setNb :: Cell -> Request ()

cell = class
          nb := Nothing
  
          depth = request
                   case nb of
                     Nothing -> result 1
                     Just c -> d <- c.depth
                               result d+1

          setNb c = request nb := Just c

          result Cell{..}

root :: World -> Cmd () ()
root w = do
   env = new posix w
   x : xs = forall _ <- [1..80000] new cell 
   forall (c,l) <- zip (x : xs) xs do c.setNb l
   d <- x.depth
   env.stdout.write (show d ++ " cells linked\n")
   zs = forall z <- [1..10] new class
          result request result z
   q <- last zs
   env.stdout.write (show q ++ "\n")
   ws <- forall n <- [9..11] do
       r <- env.stdout.write ("Hej " ++ show n ++ "\n")
       result r
   env.stdout.write (show ws ++ "\n")  
   rs <- forall q <- take 4 xs do
      d <- q.depth
      result d  
   env.stdout.write (show rs ++ "\n")  
   ts <- forall i <- [1..3], j <- [4..6] do
      result (i,j)
   env.stdout.write (show ts ++ "\n")
   ts = forall i <- [1..3] new
               forall j <- [4..6] class
                  s := 0
                  result request
                          forall p <- [i..j] do
                            s := s + p
                          result s
   a <- head(last ts)
   env.stdout.write (show a++ "\n")
   env.exit 0