module EchoServer where

import POSIX

port = Port 12345

root :: World -> Cmd () ()
root w = do
    env = new posix w

    log str = action
       env.stdout.write ('[':str ++ "]\n")

    env.inet.tcp.listen port (server log)

server log sock = class

   n := 1

   p = show sock.remoteHost

   echo str = action
      sock.outFile.write (show n ++"> "++str)
      n := n+1

   close = request
      log (p ++ " closing")
      result ()

   neterror str = log ("Neterror: "++str)

   established = action
      log ("Connected from " ++ p)
      sock.inFile.installR echo

   result Connection{..}