module Graphic where

import ARM

struct Graphic where 
   print :: String -> Request ()
   clear :: Request ()

graphic :: TFT -> Class Graphic
graphic tft =
  class
    screenXpos := 0
    screenYpos := 0
    printchar_i '\n' = do
        screenXpos := 0
        if screenYpos == 31 then
          screenYpos := 0
        else
          screenYpos := screenYpos + 1
          
        if screenYpos == 31 then
          tft.drawbox 22 0 0 tft.xsize tft.charheight -- wipe the _next_ line to increase readability
        else
          tft.drawbox 22 0 ((screenYpos+1)*tft.charheight) tft.xsize tft.charheight -- wipe the _next_ line to increase readability
    
    printchar_i c = do
      t <- tft.drawchar c (screenXpos*tft.charwidth) (screenYpos*tft.charheight)
      screenXpos := screenXpos + t
      if screenXpos > 26 then 
        printchar_i '\n'
      result ()
    printstr_i [] = do
      result ()
    printstr_i (chr:str) = do
      printchar_i chr
      printstr_i str
      result ()
    print str = request printstr_i str

    clear = request 
      tft.drawbox 22 0 0 tft.xsize tft.ysize
      screenYpos := 0
      screenXpos := 0
      result ()


    result Graphic {..}
