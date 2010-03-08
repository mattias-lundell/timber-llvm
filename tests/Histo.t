module Histo where

struct Histogram a where
   addObs    :: a -> Action
   getResult :: Request (Array Int)

  
histo bds = class
   
   cn  = size bds
   obs := uniarray (cn + 1) 0
   
   classIndex x k
       | k >= cn || x < bds!k   = k
       | otherwise = classIndex x (k+1)


   addObs x = action
      k = classIndex x 0
      obs!k := obs!k + 1

   getResult = request
       result obs

   result Histogram {..}

