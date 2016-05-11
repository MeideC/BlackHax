module Main where

import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
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


-- LUL no idea xD
shuffle' :: [a] -> StdGen -> ([a],StdGen)
shuffle' xs gen = runST (do
        g <- newSTRef gen
        let randomRST lohi = do
              (a,s') <- liftM (randomR lohi) (readSTRef g)
              writeSTRef g s'
              return a
        ar <- newArray n xs
        xs' <- forM [1..n] $ \i -> do
                j <- randomRST (i,n)
                vi <- readArray ar i
                vj <- readArray ar j
                writeArray ar j vi
                return vj
        gen' <- readSTRef g
        return (xs',gen'))
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs =  newListArray (1,n) xs

-- Creates a tuplet with the given cards in fts and remaining cards in snd.
-- Use it to deal cards.
takeCards :: [Card] -> Int -> ([Card], [Card])
takeCards cardList n = splitAt n cardList

initDeal :: [Card] -> ([Card], [Card], [Card])


main = do

  g <- newStdGen

  print $ takeCards (fst (shuffle' allCards g)) 51





  -- print $ takeCards 5 g





  --takeXCards :: (Card a) => x -> [a]
  --takeXCards n
  --  | n <= 0 = []
  --  | n = 1 = let a = fst $ randomR (0, 51) $ newStdGen
  --  | otherwise = takeXCards n - 1

  {-
  takeCard :: StdGen -> (Card, StdGen)
  takeCard g = (allCards !! (fst (randomR (0, length allCards) g)), snd (randomR (0, length allCards) g))h
  -}

  {-
  takeCards :: Int -> StdGen -> [(Card, StdGen)]
  takeCards 1 g = takeCard g : []
  takeCards n g = takeCards (n - 1) (snd ((takeCards (n - 1) g) !! (n - 1) )) ++ takeCards 1 g
  -}
