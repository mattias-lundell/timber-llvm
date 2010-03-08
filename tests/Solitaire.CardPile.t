module CardPile where

import Data.Objects.Stack
import Solitaire.Card

struct CardPile < Stack Card where
  canTake :: Card -> Request Bool

translateY :: Pos -> Int -> Pos
translateY (x,y) dy = (x,y+dy)

cardPile :: (Card -> Card -> Bool) -> Pos -> (Pos -> Int -> Pos) -> Class CardPile
cardPile ct p relPos = class

  Stack {push = push0 ..} = new stk

  push c = action
    sz <- size
    push0 c
    c.setPos (relPos p sz)

  canTake c = request
    t <- top 
    result (ct c t)

  result CardPile{..}

