module AbstractStack where


  Stack :: * -> *

  emptyStk :: Stack a

  push :: a -> Stack a -> Stack a

  pop :: Stack a -> Maybe (a, Stack a)

private

  data Stack a = Stk [a]

  emptyStk = Stk []

  push x (Stk xs) = Stk (x : xs)

  pop (Stk []) = Nothing
  pop (Stk (x : xs)) = Just (x, Stk xs)


{-

Using a type synonym as below works in the sense that it compiles and clients
can use it, but clients can also see the implementation type.

  type Stack = []

  emptyStk = []

  push = (:)

  pop [] = Nothing
  pop (x : xs) = Just (x, xs)

-}

