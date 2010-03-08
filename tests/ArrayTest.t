module ArrayTest where

import POSIX

root :: RootType
root world = do
    env = new posix world
    act = new class
        a := uniarray 10 True
        result action
            a!1 := False
            env.stdout.write (show a ++ "\n")
            env.exit 0
    act

instance showArray :: Show (Array a) \\ Show a where
    show x = "{" ++ (show0 x 0 (size x - 1)) ++ "}"
        where
        show0 x curr last 
            | curr == last  = show (x!curr)
            | otherwise     = show (x!curr) ++ "," ++ show0 x (curr+1) last