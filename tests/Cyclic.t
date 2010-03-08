module Cyclic where

import POSIX

struct S where
    val :: Int
    str :: String
    ref :: S
    
f r 0 = { val = 1, str = "A", ref = r }
f r n = f r (n-1)

g r 0 = { val = 2, str = r.str ++ "B", ref = r }
g r n = g r (n-1)

s1 = f s2 3
s2 = g s1 3
--s1 = f s2 3


cl env =
    class
       s := s1
       io _ = action
               env.stdout.write (show s.val)
               s := s.ref
       result action
         env.stdin.installR io
         env.stdout.write (s2.str)


root :: RootType
root w = do
  env = new posix w
  obj = new cl env
  obj

ggg n = let apa = 'a':bepa
            bepa = 'b':cepa
            cepa = 'c':apa
        in apa
