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

module Dictionary where

  struct Dictionary a b where
    insert :: a -> b -> Request ()
    lookup :: a -> Request (Maybe b)

  listDict :: Class (Dictionary a b) \\ Eq a
  listDict = class
    dict := []
  
    insert a b = request
      dict := insL a b dict

    lookup a = request
      result Prelude.lookup a dict

    result Dictionary {..}


  treeDict :: Class (Dictionary a b) \\ Ord a
  treeDict = class
    dict := Nil
    
    insert a b = request
      dict := insT a b dict

    lookup a = request
      result look a dict

    result Dictionary {..}


  hashDict :: (a -> Int -> Int) -> Class (Dictionary a b) ->
              Int -> Class (Dictionary a b)
  hashDict hash dictC n = class

     ds = new mapM (\_ ->  dictC) [1..n]
     dict = array ds
  
     insert a b = (dict!(hash a n)).insert a b

     lookup a = (dict!(hash a n)).lookup a
   
     result Dictionary {..}

private
  
  insL a b []            = [(a,b)]
  insL a b ((x,y) : xs) 
           | a == x      = (a,b) : xs
           | otherwise   = (x,y) : insL a b xs

  data Tree a = Nil | Node (Tree a) a (Tree a)

  insT a b Nil                 = Node Nil (a,b) Nil
  insT a b (Node l (x,y) r)  
         | a == x              = Node l (a,b) r
         | a < x               = Node (insT a b l) (x,y) r
         | a > x               = Node l (x,y) (insT a b r)

  look a Nil                   = Nothing
  look a (Node l (x,y) r)     
          | a == x             = Just y
          | a < x              = look a l
          | a > x              = look a r
