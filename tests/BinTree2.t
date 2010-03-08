module BinTree2 where

import POSIX

data Tree = Nil | Node Int Tree Tree

minN = 4

root :: RootType
root world = do
    env = new posix world
    act = new main env
    act

main env = class
  pr s n t = env.stdout.write (s++" of depth "++show n++"\t check: "++show t++"\n")

  depthLoop d m long
    | d <= m    = do pr (show (2*n) ++ "\ttrees") d (sumT d n 0) 
                     after (millisec 1000) action depthLoop (d+2) m long
                     result ()
    | otherwise = do     -- confirm that the long-lived binary tree still exists
                     pr "long lived tree" m (check long)
                     env.exit 0

   where n :: Int
         n = 2 ^ (m - d + minN)

  result action
      if size env.argv < 2 then
        env.stdout.write "Usage: BinTree2 <number>"
        env.exit 0
      temp = parse (env.argv!1)
      if isLeft temp then
        env.stdout.write "Usage: BinTree2 <number>"
        env.exit 0      
      n :: Int
      n = fromRight temp
      maxN     = max (minN + 2) n
      stretchN = maxN + 1

    -- stretch memory tree
      c = check (make 0 stretchN)
      pr "stretch tree" stretchN c

    -- allocate a long lived tree
      long    = make 0 maxN

    -- allocate, walk, and deallocate many bottom-up binary trees
      depthLoop minN maxN long

-- allocate and check lots of trees
sumT :: Int -> Int -> Int -> Int
sumT d 0 t = t
sumT d i t = sumT d (i-1) (t + a + b)
  where a = check (make i    d)
        b = check (make (-i) d)

-- traverse the tree, counting up the nodes
check :: Tree -> Int
check Nil          = 0
check (Node i l r) = i + check l - check r

-- build a tree
make :: Int -> Int -> Tree
make i 0 = Node i Nil Nil
make i d = Node i (make (i2-1) d2) (make i2 d2)
  where i2 = 2*i; d2 = d-1