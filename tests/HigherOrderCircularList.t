module HigherOrderCircularList where

import POSIX

root :: RootType
root w = do
    env = new posix w
    act = new main env
    act

main env = class
    t := []
    start = action
        env.stdout.write "Tick\n"
        t := (after (sec 1) start) : t
        head t
    result start