module BitTest where

import POSIX
import BitOps

c32a, c32b, c32c, c32d, c32e :: BITS32
c32a = 0xF0F00000
c32b = 0x0000F0FF
c32c = (c32a .|. c32b) .&. 0xFFFFFFF0
c32d = c32c .>>. 1
c32e = c32c `bsra` 1

c16a, c16b, c16c, c16d, c16e :: BITS16
c16a = 0xF000
c16b = 0x00FF
c16c = (c16a .|. c16b) .&. 0xFFF0
c16d = c16c .>>. 1
c16e = c16c `bsra` 1

c8a, c8b, c8c, c8d, c8e :: BITS8
c8a = 0xF0
c8b = 0x0F
c8c = (c8a .|. c8b) .&. 0xF3
c8d = c8c .>>. 1
c8e = c8c `bsra` 1

root :: RootType     
root world = do
    env = new posix world
    env.stdout.write ( (show c32a) ++ " or " ++ (show c32b) ++ " & 0xFFFFFFF0 = " ++ (show c32c) ++ " >> 1 = " ++ (show c32d)  ++ " >> (arith) 1 = " ++ (show c32e)++ "\n" )
    env.stdout.write ( (show c16a) ++ " or " ++ (show c16b) ++ " & 0xFFF0 = " ++ (show c16c) ++ " >> 1 = " ++ (show c16d)  ++ " >> (arith) 1 = " ++ (show c16e)++ "\n" )
    env.stdout.write ( (show c8a) ++ " or " ++ (show c8b) ++ " & 0xF3 = " ++ (show c8c) ++ " >> 1 = " ++ (show c8d)  ++ " >> (arith) 1 = " ++ (show c8e)++ "\n" )
    env.exit 0