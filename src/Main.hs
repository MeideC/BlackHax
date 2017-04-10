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

-- Create a deck of cards and shuffle them
allCards :: StdGen -> Deck
allCards g = Deck { deck = fst(shuffle' [Card x y | x <- [Spade .. Diamond], y <- [Two .. Ace]] g)}

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
makeDeck :: DeckState Deck
makeDeck = do
    state <- get
    return state

-- Create a new Game
makeGame :: StdGen -> Game
makeGame g = Game 
    -- identify the to-be deck with d'
    { gDeck = d',
    playerHand = pHand,
    dealerHand = dHand,
    message = "message" }
    -- make the Deck appear in the state with makeDeck
    -- and then use the 'deal' to return a ((Hand, Hand), DeckState)
    where d = execState makeDeck $ allCards g
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
        putStrLn $ "Your current hand is: " ++ (show pHand)
        putStrLn $ "which values to: " ++ show (handValue pHand)
        putStrLn $ ""
        putStrLn $ "Dealer's hand: " ++ (show dHand)
        putStrLn $ "which values to: " ++ show (handValue dHand)
        putStrLn $ ""
        putStrLn $ "What will you do (hit/stay)"
        putStrLn $ ""
        input <- getLine
        return input

    when (input == "stay") $ do 
        handleDealer

    when (input == "hit") $ do
        let (newCard, newDeck) = runState drawCard $ gDeck curr     
        put curr { gDeck = newDeck, playerHand = newCard : playerHand curr }

        newState <- get
        let newHand = playerHand newState
        handlePlayer

-- dealer lifts card to the dealerHand
handleDealer :: GameState IO ()
handleDealer = do
    currentState <- get
    let (newCard, newDeck) = runState drawCard $ gDeck currentState
    put currentState { gDeck = newDeck, dealerHand = newCard : dealerHand currentState }

-- Calculate complete value of a hand, return Int
handValue :: Hand -> Int
handValue [] = 0
handValue (h:hs) = value h + handValue hs

-- evaluate the CardValue of a single Card
value :: Card -> Int
value (Card _ x) = intValue x

-- Map the CardValue's to Int
intValue :: CardValue -> Int
intValue Two = 2
intValue Three = 3
intValue Four = 4
intValue Five = 5
intValue Six = 6
intValue Seven = 7
intValue Eight = 8
intValue Nine = 9
intValue Ten = 10
intValue Jack = 10
intValue Queen = 10
intValue King = 10
intValue Ace = 11


-- TODO smart dealer
-- check winners after dealers turn
-- if player go over 21, you lose
runGame :: GameState IO ()
runGame = do
    handlePlayer
    runGame


-- remember the fugin T
main :: IO ()
main = do
    g <- newStdGen
    evalStateT runGame $ makeGame g




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
