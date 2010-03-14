module Echo3 where

import POSIX

struct Echo3 where
  save :: String -> Action
  tick :: Action

echo3 env = class

   current := "Hello!\n"

   save str = action
      current := str

   tick = action
      env.stdout.write current
      after (sec 1) tick

   result
     Echo3 {..}

root :: World -> Cmd () ()
root w = do
   env = new posix w
   echo = new echo3 env

   env.stdin.installR echo.save
   echo.tick
