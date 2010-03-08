module Debug where

import ARM
import MyString

struct Debug where
    printstr    :: String -> Request ()
    printvardec :: String -> Int -> Request ()
    printvarhex :: String -> Int -> Request ()
    printvarbin :: String -> Int -> Request ()

debug printer = 
  class
    printvardec str i = request
       printer str
       printer ": "
       printer (inttostr i 10)
       printer "\n"
    printvarhex str i = request
       printer str
       printer ": 0x"
       printer (inttostr i 16)
       printer "\n"
    printvarbin str i = request
       printer str
       printer ": 0b"
       printer (inttostr i 2)
       printer "\n"
    printstr s = printer s
    result Debug {..}
