module LocalSig where

import POSIX

root :: RootType
root w = do
    env = new posix w
    act = new main env
    act
    
main env = class
            result action
              print :: a->Action
              print s = action
                              env.stdout.write ("OK\n")
                              env.exit 0
              print False