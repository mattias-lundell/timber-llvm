module File where

import POSIX

root :: RootType
root w = do
    env = new posix w
    fname = env.argv!1
    h1 <- env.openR fname
    case h1 of
        Just f -> 
            inp <- f.read
            env.stdout.write inp
            h2 <- env.openW (fname ++ "2")
            case h2 of
                Just f2 -> 
                    f2.write inp
                    f2.close
                Nothing -> 
                    env.stdout.write ("Cannot open "++fname++"2\n")
            env.exit 0
        Nothing -> 
            env.stdout.write ("Cannot open "++fname++"\n")
            env.exit 0