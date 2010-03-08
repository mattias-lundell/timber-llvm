module Card where

data Rank = Ace | King | Queen | Jack | Num Int

data Suit = Spades | Hearts | Diamonds | Clubs

instance showRank :: Show Rank
showRank = struct
  show Ace     = "Ace"
  show King    = "King"
  show Queen   = "Queen"
  show Jack    = "Jack"
  show (Num n) = "Num " ++ show n

default showSuit :: Show Suit 

type Pos = (Int,Int)

struct Card where
  rank :: Rank
  suit :: Suit

  faceUp :: Request Bool
  flip   :: Action
  setPos :: Pos -> Action
  getPos :: Request Pos

instance showCard :: Show Card 
showCard = struct
  show c = show c.rank ++ " " ++ show c.suit

card :: Rank -> Suit -> Pos -> Class Card
card rank suit p = class
  up  := False
  pos := p 

  flip     = action up := not up
  faceUp   = request result up

  setPos p = action pos := p
  getPos   = request result pos

  result Card{..}

