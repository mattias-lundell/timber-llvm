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

module Depend where

import Common
import Core
import Syntax
import PP

type Graph a                    = Map a [a]

graph nbors ps                  = map f ps
  where f (i,d)                 = (i,[v | v <- nbors d, v/=i, v `elem` domain])
        domain                  = dom ps
 
scc                             :: Eq a => Graph a -> [[a]]
scc g                           = dfs g' (concat (dfs g (dom g)))
  where g'                      = [(i,[x | (x,ys) <- g, i `elem` ys]) | (i,_) <- g]

dfs g is                        = snd (dfs' ([],[]) is)
   where dfs' p []              = p
         dfs' p@(vs,ns) (x:xs)
           | x `elem` vs        = dfs' p xs
           | otherwise          = dfs' (vs', (x:concat ns'):ns) xs
           where (vs',ns')      = dfs' (x:vs,[]) (nbors x)
         nbors i                = fromJust (lookup i g)

order ms ns                     = ns `zip` map (fromJust . flip lookup ms) ns
  
group ms nss                    = map (order ms) nss

topSort                         :: Eq a  => (b -> [a]) -> [(a,b)] -> Either [a] [(a,b)]
topSort nbors ms                = case dropWhile (null . tail) ns of
                                    []     -> Right (order ms (concat ns))
                                    xs : _ -> Left xs
          where ns              = scc (graph nbors ms)

groupBinds (Binds _ te es)      = map f ess
  where gs                      = scc (graph evars es)
        ess                     = group es gs
        f [(x,e)]               = Binds (x `elem` evars e) (restrict te [x]) (restrict es [x])
        f es'                   = Binds True (restrict te xs) (restrict es xs)
          where xs              = dom es'


groupTypes (Types ke ts)        = map f tss
  where gs                      = scc (graph tycons ts)
        tss                     = group ts gs
        f ts'                   = Types (restrict ke cs) (restrict ts cs)
          where cs              = dom ts'


groupMap bs                     = map f bss
  where gs                      = scc (graph evars bs)
        bss                     = group bs gs
        f bs@[(x,b)]            = (x `elem` evars b, restrict bs [x])
        f bs'                   = (True, restrict bs xs)
          where xs              = dom bs'
         
-- Dependency analysis on Syntax bindlists -------------------------------------------

isFunEqn v (BEqn (LFun v' _) _) = v == v'
isFunEqn v _                    = False

graphInfo []                    =  []
graphInfo (s@(BSig [v] _) : bs)
                                = (s:bs1, [v], nub (idents bs1)) : graphInfo bs2
  where (bs1,bs2)               = span (isFunEqn v) bs
graphInfo (e@(BEqn (LFun v _) rh) : bs) 
                                = (e:bs1, [v], nub (idents (e:bs1))) : graphInfo bs2
  where (bs1,bs2)               = span (isFunEqn v) bs
graphInfo (e@(BEqn (LPat p) rh) : bs)  
                                = ([e], nub (idents p), nub (idents rh)) : graphInfo bs
graphInfo (BSig _ _ : bs)       = graphInfo bs

buildGraph                      :: [([Bind],[Name],[Name])] -> Graph Int
buildGraph vs                   = zip ns (map findDeps fvss)
  where (_,bvss,fvss)           = unzip3 vs
        ns                      = [1..]
        dict                    = concatMap mkDict (zip bvss ns)
        mkDict (vs,n)           = map (\v -> (v,n)) vs
        findDeps xs             = [n | Just n <- map (flip lookup dict) xs]

groupBindsS                     :: [Bind] ->  [[Bind]]
groupBindsS bs                  = map f gs
  where infos                   = graphInfo bs
        indexedInfos            = [1..] `zip` infos
        gs                      = scc (buildGraph infos)
        f indices               = concat (map (fst3 . snd) (filter ((`elem` indices) . fst) indexedInfos))
