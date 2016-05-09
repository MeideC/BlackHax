module Deck where

data Suit = Spade | Club | Heart | Diamond deriving (Show)
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

data Card = Card Suit CardValue deriving (Show)
