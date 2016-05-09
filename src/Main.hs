module Main where

import System.Random
--import Rng


-- Create our own Types for Suit and CardValue
data Suit = Spade | Club | Heart | Diamond deriving (Show, Enum)
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
-- Define the Card value consisting of Suit and CardValue
data Card = Card Suit CardValue deriving (Show)

-- Create a deck of cards
allCards :: [Card]
allCards = [Card x y | x <- [Spade .. Diamond], y <- [Two .. Ace]]

--takeXCards :: (Card a) => x -> [a]
--takeXCards n
--  | n <= 0 = []
--  | n = 1 = let a = fst $ randomR (0, 51) $ newStdGen
--  | otherwise = takeXCards n - 1

takeCard :: StdGen -> (Card, StdGen)
takeCard g = (allCards !! (fst (randomR (0, length allCards) g)), snd (randomR (0, length allCards) g))

takeCards :: Int -> StdGen -> [(Card, StdGen)]
takeCards 1 g = takeCard g : []
takeCards n g = takeCards (n - 1) (snd ((takeCards (n - 1) g) !! (n - 1) )) ++ takeCards 1 g

main = do

  g <- newStdGen
  print $ takeCards 5 g

  -- print $ allCards
