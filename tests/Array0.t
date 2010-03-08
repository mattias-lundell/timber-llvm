module Array0 where

import POSIX

a :: Array Int 
a = array [1..10000]

foldlArray f u a = iter 0 u
  where iter k ack
          | k < size a = iter (k+1) (f ack (a!k))
          | otherwise = ack

root :: RootType
root world = do
    env = new posix world
    env.stdout.write (show (foldlArray (+) 0 a) ++ "\n")
    env.exit 0