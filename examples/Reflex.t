module Reflex where

import POSIX
import RandomGenerator

data State = Idle | Waiting Msg | Counting

reflex print gen = class
  
   tmr = new timer

   state := Idle
  
   enter _ = action
      case state of
        Idle ->         r <- gen.next
                        waitingTime = sec 2 + millisec (r `mod` 2000)
                        msg <- after waitingTime action
                           tmr.reset
                           print "Go!"
                           state := Counting 
                        print "Wait..."
                        state := Waiting msg
                 
        Waiting msg ->  abort msg
                        print "Cheat!!!"
                        state := Idle

        Counting ->     t <- tmr.sample
                        print (format t)
                        state := Idle

   result enter

root :: World -> Cmd () ()
root w = do
   env = new posix w
   print str = env.stdout.write (str ++ "\n")
   gen = new baseGen (microsecOf env.startTime)
   enter = new reflex print gen
   env.stdin.installR enter
   print "Press return to start"
   


format t = show (secOf t) ++ '.' : fracs ++ " secs"
  where t100  = microsecOf t `div` 10000
        fracs = if t100<10 then '0':show t100 else show t100 
