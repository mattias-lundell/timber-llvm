module EchoServer2 where

import POSIX
import Counter

port = Port 12345

root :: World -> Cmd () ()
root w = do
 
    env = new posix w
    clients = new counter

    log str = action
       env.stdout.write ('[':str ++ "]\n")

    case parse (env.argv!1) of
       Right n -> env.inet.tcp.listen port (server n clients log)
       _ -> env.stdout.write "usage: EchoServer n, where n is number of concurrent clients\n"
            env.exit 1

server :: Int -> Counter -> (String -> Action) -> Socket -> Class Connection
server maxClients clients log sock = class

   n := 1

   p = show sock.remoteHost

   echo str = action
      sock.outFile.write (show n ++"> "++str)
      n := n+1

   close = request
      clients.decr
      log (p ++ " closing")
      result ()

   neterror str = log ("Neterror: "++str)

   established = action
      cl <- clients.value
      if cl < maxClients then
         clients.incr
         log ("Connected from " ++ p)
         sock.inFile.installR echo
      else
         sock.outFile.write "Server busy"
         log ("Refused " ++ p)
         sock.close

   result Connection{..}