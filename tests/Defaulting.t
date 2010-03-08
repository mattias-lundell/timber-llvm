module Defaulting where

import POSIX

--y :: [Int]
y = 7:8:y

root :: RootType
root w = do
    env = new posix w
    x = show (head (tail y))
    env.stdout.write (x ++ "\n")
    env.exit 0