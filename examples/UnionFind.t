module UnionFind where

struct EqRel where
  equal   :: Int -> Int -> Request Bool
  mkEqual :: Int -> Int -> Action

unionFind :: Int -> Class EqRel
unionFind n = class
   a := uniarray n (-1)

   find k = do 
     if a!k < 0 then
        result k
     else
        s <- find(a!k)
        a!k := s
        result s

   equal i j = request
     si <- find i
     sj <- find j
     result (si==sj)

   mkEqual i j = action
     si <- find i 
     sj <- find j
     if si/=sj then
        asi = a!si
        asj = a!sj
        if asj < asi then
           a!si := sj
        else 
           a!sj := si
           if asi==asj then
              a!si := asi-1

   result EqRel{..}
