module KindleArities where

import POSIX

struct Counter where
        display :: Action

acts = map (.display) []


test True  = class
               result (\y->y) 
test False = class
               result (\_->action)
