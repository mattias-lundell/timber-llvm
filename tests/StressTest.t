module StressTest where

import POSIX
import Data.Functional.List

port = Port 12345 


check env k sock = class
    n := 1
    deliver str = action
        env.stdout.write (show k ++ ", mess " ++ show n ++ " >>> " ++ str ++ "\n")
        if n < 50 then
          after (millisec 100) action 
             sock.outFile.write "Hi!"
        else
          env.stdout.write (show k ++ " done!!")
          sock.close
        n := n+1
          
    established = action
       sock.inFile.installR deliver
       sock.outFile.write "Hi!"
       

    neterror str = action
          env.stdout.write (show k ++ " error: " ++ str)

    close = request result ()

    result Connection {..}
 

root :: RootType
root w = do
    env = new posix w
    act = new main env
    act
    
main env = class

    args = [1..fromRight$parse (env.argv!1)] 

    finish = action 
       env.stdout.write "Done!\n"
       env.exit 0
    
    result action
       forall i <- args do
          env.inet.tcp.connect (Host "localhost") port (check env i)
       after (sec 20) finish 