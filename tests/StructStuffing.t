module StructStuffing where

struct A where
    a :: Int
    b :: Int
    
f x = A {b = x, ..}
  where a = 1
