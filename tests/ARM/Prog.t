module Prog where
 
import ARM
import MyString
import Graphic
import Debug

import Buttons

struct Ticker where
	startme :: Action

counter :: (String -> Int -> Request ()) -> Time -> Class Ticker
counter printfunc interval = class
    loop tal = action
        printfunc "tal" tal
        after interval loop (tal+1)
    startme = before (millisec 100) loop 0
    result Ticker {..}


root :: Env -> TFT -> Class Prog
root env tft =
    class
        g = new graphic tft
        d1 = new debug env.debug
        d2 = new debug g.print
        c1 = new counter (d1.printvardec) (millisec 200)
        c2 = new counter (d2.printvardec) (sec 1)
        buttons1 = new buttons env
    
        start = action
            g.clear
            d2.printstr "Hello World!\n"
            -- c1.startme
            c2.startme
            buttons1.init
        result start
