module Tail where

sumAck :: Int -> Int -> Int -> Int
sumAck n k ack
  |k > n = ack
  | otherwise = sumAck n (k+1) (ack+1)

sumTo :: Int -> Int
sumTo n = sumAck n 1 0
