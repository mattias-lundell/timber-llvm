module Echo where

import POSIX

root :: RootType
root world = do

   env = new posix world

   echo str = action
      env.stdout.write str

   env.stdin.installR echo
