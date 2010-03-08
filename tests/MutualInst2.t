module MutualInst2 where

import POSIX

memoryObj x = class
   m := x
   result False

wormObj _ = class
   mem = new memoryObj "x"
   result "y"

root :: RootType
root w = do
   env = new posix w
   w1 = new wormObj w2  
   w2 = new wormObj w1 
   env.stdout.write "OK!\n"
   env.exit 0