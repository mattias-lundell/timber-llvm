module ActionParameter where

struct A where
    act1 :: Int -> Action
    act2 :: Action

cl :: Int -> (Int->Action) -> Class A
cl x a = class

            act1 = a

            act2 = action
              if x>0 then
                a 1
              else
                a 2

            result A{..}
