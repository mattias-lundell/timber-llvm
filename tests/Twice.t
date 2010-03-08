module Twice where

twice :: (a -> b) -> a -> b \\ b < a
twice f x = f (f x)
