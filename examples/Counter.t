module Counter where

struct Counter where
  incr  :: Action
  decr  :: Action
  value :: Request Int

counter = class

   n := 0

   incr  = action n := n+1

   decr  = action n := n-1
   
   value = request result n

   result Counter{..}