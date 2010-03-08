module BeforeConstraint where

import POSIX

root :: RootType
root world = do
    env = new posix world
    act = new class
        a = action
            env.stdout.write "Hello\n"
            after (sec 1) a
        result action
            after (sec 2) before (sec 1) a
    act