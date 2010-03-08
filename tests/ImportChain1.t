module ImportChain1 where

data A1 = A1

f1 x = A1

struct R where
  sel :: Int

r = R { sel = 1 }
