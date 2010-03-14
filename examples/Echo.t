module Echo where

import POSIX

root :: World -> Cmd () ()
root w = do

   env = new posix w

   echo str = action
      env.stdout.write str

   env.stdin.installR echo

