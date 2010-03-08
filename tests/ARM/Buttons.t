module Buttons where

import ARM
import BitOps

import Debug

struct Buttons where
  init :: Request ()
  handle :: Int -> Action -> Request ()

aEXTINT         = 0xE01FC140
aIO2IntEnR      = 0xE00280B0
aIO2IntEnF      = 0xE00280B4
aIO2IntStatR    = 0xE00280A4
aIO2IntStatF    = 0xE00280A8
aIO2IntClr      = 0xE00280AC

joyBits = (1 .<<. 27) .|. (1 .<<. 26) .|. (1 .<<. 25) .|. (1 .<<. 23) .|. (1 .<<. 22)


buttons env = 
  class
    defaulthandler = action 

    joy1handler := defaulthandler 
    joy2handler := defaulthandler 
    joy3handler := defaulthandler 
    joy4handler := defaulthandler 
    joy5handler := defaulthandler 

    handle i meth = request
                        case i of
                          1 -> joy1handler := meth
                          2 -> joy2handler := meth
                          3 -> joy3handler := meth
                          4 -> joy4handler := meth
                          5 -> joy5handler := meth
                          _ ->
                        result ()

  
    d = new debug env.debug

    handler sr sf = before (millisec 5) action
                        d.printvarhex "StatR" (toInt sr)
                        d.printvarhex "StatF" (toInt sf)

                        case toInt sf of
                          0x08000000 -> joy1handler
                          0x04000000 -> joy2handler
                          0x02000000 -> joy3handler
                          0x00800000 -> joy4handler
                          0x00400000 -> joy5handler
                          _          ->

                        env.debug "\n"

    init = request
      env.portwrite aIO2IntEnR joyBits
      env.portwrite aIO2IntEnF joyBits
      env.install 17 (ack env handler)
      result ()

    result Buttons {..}
    
ack env f = do sr <- env.portread aIO2IntStatR
               sf <- env.portread aIO2IntStatF
               env.portset aIO2IntClr (sr .|. sf)   -- Important to acknowledge here and not in f
               env.portwrite aEXTINT 0x08           -- Precaution in case of spurious EINT3 interrupt
               f sr sf
