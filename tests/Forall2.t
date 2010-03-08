module Forall2 where

import POSIX

test env = class
   result \i -> action
       env.stdout.write ("i = " ++ show i ++ "\n")

root :: RootType
root w = do
    env = new posix w
    to = new test env
--       to 7
--       list1 = [to, to]
--       forall m <- list1 do
--           m 1
    list2 = [(3, to), (4, to)]
    forall (i, mm) <- list2 do
        mm i