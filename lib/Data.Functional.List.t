-- The Timber compiler <timber-lang.org>
--
-- Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the names of the copyright holder and any identified
--    contributors, nor the names of their affiliations, may be used to 
--    endorse or promote products derived from this software without 
--    specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
-- OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

module List where

delete             :: a -> [a] -> [a] \\ Eq a
delete              = deleteBy (==)

deleteBy           :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy eq x []    = []
deleteBy eq x (y:ys)= if x `eq` y then ys else y:deleteBy eq x ys


(\\)               :: [a] -> [a] -> [a] \\ Eq a
(\\)                = foldl (flip delete)

elemBy, notElemBy  :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy eq _ []      = False
elemBy eq x (y:ys)  = x `eq` y || elemBy eq x ys

notElemBy eq x xs   = not (elemBy eq x xs)

lookupBy           :: (a -> a -> Bool) -> a -> [(a, b)] -> Maybe b
lookupBy eq key []  = Nothing
lookupBy eq key ((x,y):xys)
    | key `eq` x    = Just y
    | otherwise     = lookupBy eq key xys

partition          :: (a -> Bool) -> [a] -> ([a],[a])
partition p         = foldr select ([],[])
		      where select x (ts,fs) | p x       = (x:ts,fs)
		  			     | otherwise = (ts,x:fs)

nub                :: [a] -> [a] \\ Eq a
nub                 = nubBy (==)

nubBy              :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq []         = []
nubBy eq (x:xs)     = x : nubBy eq (filter (\y -> not (eq x y)) xs)

sort               :: [a] -> [a] \\ Ord a
sort []             = []
sort (x : xs)       = sort small ++ x : sort big
  where (small,big) = partition (\y -> y <= x) xs

span, break         :: (a -> Bool) -> [a] -> ([a],[a])
span p []            = ([],[])
span p (x:xs)
         | p x       = let (ys,zs) = span p xs in (x:ys,zs)
         | otherwise = ([],x:xs)

break p              = span (\x -> not (p x))

lines               :: String -> [String]
lines []            = []
lines s             = let (l,s') = break ('\n'==) s
                      in l : case s' of 
                               []      -> []
                               (_:s'') -> lines s''

words               :: String -> [String]
words s             = case dropWhile isSpace s of
                        [] -> []
                        s' -> w : words s''
                           where (w,s'') = acc s' []
                                 acc [] w = (reverse w,[])
                                 acc (c:cs) w
                                  |isSpace c = (reverse w,cs)
                                  |otherwise = acc cs (c:w) 
                         
isSpace c           = elem c " \t\n"

unlines             :: [String] -> String
unlines ls          = concat (map (\l -> l ++ "\n") ls)

unwords             :: [String] -> String
unwords []          = []
unwords ws          = foldr1 (\w s -> w ++ ' ':s) ws

foldr1 f [x]        = x
foldr1 f (x:xs)     = f x (foldr1 f xs)

sum                 :: [a] -> a \\ IntLiteral a, Num a
sum                 = foldl (+) 0

null []             = True
null _              = False
