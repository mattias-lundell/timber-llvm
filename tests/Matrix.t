module Matrix where

struct Matrix a where
  setElem :: Int -> Int -> a -> Action
  getElem :: Int -> Int -> Request a
  setRow  :: Int -> Array a -> Action
  getRow  :: Int -> Request (Array a)


a = uniarray 10 (uniarray 10 0)

l = array [array [1,2,3], array [4,5,6]]

b = a!3!4

xyz aaa = aaa!3!14

c = a!5


-- x!2 = 3    Illegal; captured in Rename

m a = class

       x := a 
       
       setElem i j v = action
        x!i!j := v

       getElem i j = request
         result x!i!j

       setRow n a = action
         x!n := a

       getRow k = request
         result x!k 

       result Matrix {..}




   
