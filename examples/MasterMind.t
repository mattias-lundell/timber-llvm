module MasterMind where

import Data.Functional.List
import RandomGenerator
import POSIX

{-
Standard Mastermind, where the program does the guessing. Answers are given as two integers (separated by space(s)) on 
one row, indicating number of bulls and cows. 
-}

data Colour = Red | Blue | Green | Yellow | Black | White

default eqColour :: Eq Colour
        showColour :: Show Colour 
        parseColour :: Parse Colour
        eqAnswer :: Eq Answer

type Guess = [Colour]

data Answer = Answer Int Int

instance showAnswer :: Show Answer where
   show (Answer e n) = show e ++ " " ++ show n

instance showGuess :: Show Guess where
  show ss = unwords (map show ss)

type Board = [(Guess,Answer)]

allColours = [Red, Blue, Green, Yellow, Black, White]

allCodes :: Int -> [Guess]
allCodes 0 = [[]]
allCodes n = concat [[c:cs | c <- allColours] | cs <- allCodes (n-1)]

answer :: Guess -> Guess -> Answer
answer guess code = Answer e n
   where e = equals guess code

         n  = sum [min (count c guess) (count c code) | c <- allColours] - e

         count c xs = length [x | x <- xs, x==c]

         equals [] [] = 0
         equals (x:xs) (y:ys)
           |x==y      = 1 + equals xs ys
           |otherwise = equals xs ys


contradictions :: Board -> Guess -> Board
contradictions board c = [(g,r) | (g,r) <- board, answer g c /= r]

consistent :: Board -> [Guess] -> [Guess]
consistent board cs = [ c | c <- cs, null (contradictions board c)]

data State = Idle | JustGuessed | GameOver | GetSecret
           
mastermind env = class
  
  gen = new baseGen(microsecOf env.startTime)

  board := []
  cs := [] 
  state := Idle

  shift = do
     r <- gen.next
     n = r `mod` (length cs)
     ys = take n cs
     zs = drop n cs
     cs := zs ++ ys
  
  startGame = do
    board := []
    cs := []
    env.stdout.write "Choose your secret. Press return when ready.\n"
    state := Idle

  mkGuess = do
     if null cs then
        env.stdout.write "Contradictory answers!\n"
        env.stdout.write "Tell me your secret: "
        state := GetSecret
     else
        shift
        env.stdout.write ("My guess: "++ show (head cs) ++"\n")
        env.stdout.write "Answer (two integers): "
        state := JustGuessed

  checkQuit = do
     env.stdout.write "Do you want to play again? (y/n) "
     state := GameOver

  inpHandler inp = action
    case state of
      Idle ->           cs := allCodes 4
                        mkGuess

      JustGuessed ->    case map parse (words inp)  of
                          [Right e, Right n] ->
                             if e == 4 then
                                env.stdout.write "Yippee!\n"
                                checkQuit 
                             else
                                c:cs' = cs
                                board := (c,Answer e n) : board
                                cs := consistent board cs'
                                mkGuess
                          _ -> env.stdout.write "Answer must be two integers separated by spaces; try again\n"
      
      GameOver ->       if head inp == 'y' then
                           startGame
                        else
                           env.exit 0

      GetSecret ->     case map parse (words inp) of
                         es | length es==4 && all isRight es ->
                           ss = map fromRight es
                           (g',r'):_ = contradictions board ss
                           env.stdout.write ("When I guessed "++show g'++ ", you answered " ++ show r'++".\n")
                           env.stdout.write ("Correct answer should have been "++show (answer g' ss)++".\n")
                           checkQuit
                         _ -> env.stdout.write "Secret must be four colours separated by spaces; try again\n"

  init = action
     env.stdin.installR inpHandler
     env.stdout.write "Welcome to Mastermind!\n"
     startGame

  result init

root :: World -> Cmd () ()
root w = do
  env = new posix w
  init = new mastermind env
  init
