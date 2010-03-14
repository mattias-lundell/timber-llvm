module Echo2 where

import POSIX

echo2 env = class
   count := 1

   prompt = do
      env.stdout.write (show count++"> ")
      count := count+1

   echo str = action
      env.stdout.write str
      prompt

   init = action 
      env.stdin.installR echo
      env.stdout.write "Welcome to Echo2!\n"
      prompt

   result init

root :: World -> Cmd () ()
root w = do
  env  = new posix w
  init = new echo2 env
  init


