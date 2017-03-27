module Main where

import System.Random
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Control.Monad.State


-- Create our own Types for Suit and CardValue
data Suit = Spade | Club | Heart | Diamond deriving (Show, Enum)
data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded, Show, Read)
-- Define the Card value consisting of Suit and CardValue
data Card = Card Suit CardValue deriving (Show)

data Deck = Deck { deck :: [Card] } deriving (Show)
type Hand = [Card]

data Game = Game {
    gDeck :: Deck,
    playerHand :: Hand,
    dealerHand :: Hand,
    message :: String } deriving (Show)

type DeckState a = State Deck a
type GameState a = StateT Game a

-- Create a deck of cards
allCards :: Deck
allCards = Deck { deck = [Card x y | x <- [Spade .. Diamond], y <- [Two .. Ace]] }

-- Draws a single card from the Deck
-- if run with runState, returns a tuple (Card, DeckState)
drawCard :: DeckState Card
drawCard = do
    state <- get
    -- get the first card and last
    let card    = head (deck state)
        newDeck = tail (deck state)
    -- save the reduced deck
    put state { deck = newDeck }
    return card

-- Get the deck from the state
-- TODO make the suffling
makeDeck :: DeckState Deck
makeDeck = do
    state <- get
    return state

-- Create a new Game
makeGame :: Game
makeGame = Game 
    -- identify the to-be deck with d'
    { gDeck = d',
    playerHand = pHand,
    dealerHand = dHand,
    message = "message" }
    -- make the Deck appear in the state with makeDeck
    -- and then use the 'deal' to return a ((Hand, Hand), DeckState)
    where d = execState makeDeck $ allCards
          ((pHand, dHand), d') = runState initDeal $ d 

initDeal :: DeckState (Hand, Hand)
initDeal = do
    -- draw 2 for each player, put them into a list and return them in a Tuple
    player <- drawCard
    dealer <- drawCard
    player' <- drawCard
    dealer' <- drawCard
    let p = [player, player']
        d = [dealer, dealer']
    return (p, d)

-- legacy
gameOver :: GameState IO ()
gameOver = do
    curr <- get
    put curr { message = "over" }
    let playerH = playerHand curr
        dealerH = dealerHand curr
    liftIO . putStrLn $ "ur hand: " ++ (show playerH)


handlePlayer :: GameState IO ()
handlePlayer = do
    curr <- get
    input <- liftIO $ do
        let pHand = playerHand curr
            dHand = dealerHand curr
        putStrLn $ "ur hadn: " ++ (show pHand)
        putStrLn $ "there hadn: " ++ (show dHand)
        putStrLn $ "u do?! (y/nu)"
        input <- getLine
        return input

    when (input == "y") $ do
       let (newCard, newDeck) = runState drawCard $ gDeck curr     
       put curr { gDeck = newDeck, playerHand = newCard : playerHand curr }

       newState <- get
       let newHand = playerHand newState
       liftIO . putStrLn $ "new haend: " ++ (show newHand)


runGame :: GameState IO ()
runGame = do
    handlePlayer
    runGame


-- remember the fugin T
main :: IO ()
main = do
    evalStateT runGame $ makeGame

