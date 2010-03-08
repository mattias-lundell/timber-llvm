module ImportChain2 where

import ImportChain1

data  A2 > A1 = A2

a2 = A1

f2 x = f1 x

struct R2 < R where
  sel2 :: Int

r2 = R2 {sel = 1, sel2 = 3 }
