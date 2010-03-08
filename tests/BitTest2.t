module BitTest2 where

import POSIX
import BitOps

root :: RootType
root world = do
        env = new posix world
        
        a = 0x12345678 :: BITS32
                
        env.stdout.write ( "a =" ++ (show a) ++ ",  " ++ (show (a `bxor` 0xFFFF)) ++ "\n")
        env.stdout.write ( "btst a 0 = " ++ show (btst a 0) ++ "\n" )
        
        b = bset a 1
        env.stdout.write ( "b =" ++ (show b) ++ "\n")
        
        env.stdout.write ( "b =" ++ (showhex b) ++ "\n")
        
        env.stdout.write ( "0x1234FFFF =" ++ (showhex (0x1234FFFF::BITS32)) ++ "\n")
        
        env.stdout.write ( "0x12FF =" ++ (showhex (0x12FF::BITS16)) ++ "\n")
            
        env.stdout.write ( "0xFF =" ++ (showhex (0xFF::BITS8)) ++ "\n")
        env.exit 0