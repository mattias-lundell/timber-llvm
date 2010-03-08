module HistoClient where

import POSIX
import Histo

root :: RootType
root w = do
    env = new posix w
    act = new main env
    act
    
main env = class
             bds :: Array Int
             bds = array [10, 20, 30]
             h = new histo bds
             
             io d = action
                if head d == 'q' then
                   rs <- h.getResult
                   showResults rs
                   env.exit 0
                else
                   h.addObs(fromRight (parse d))
                   
             showResults rs = do
                   forall i <- [0..size bds-1] do
                     env.stdout.write ("   -- "++show(bds!i))
                     env.stdout.write ("  " ++ show (rs!i)++"\n")
                   env.stdout.write ("   >  "++show(bds!(size bds - 1)))
                   env.stdout.write ("  " ++ show (rs!(size bds))++"\n")
                  

             result action
               env.stdin.installR io
               env.stdout.write "Welcome!\n"