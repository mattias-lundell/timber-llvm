module A where

data A = A

instance intA :: IntLiteral A where
   fromInt _ = A

default intInt < intA


a = "a"
