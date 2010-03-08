module AbstractStackClient where
 
import AbstractStack

push2 x y s = push x (push y s)

singleStack x = push x emptyStk

{-
-- the following is illegal since Stack is abstract
top :: Stack a -> a
top (Stack (x : _)) = x
-}