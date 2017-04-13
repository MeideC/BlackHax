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

-- define a Deck of [Card]
data Deck = Deck { deck :: [Card] } deriving (Show)
-- Hand is a type(alias) for [Card], nothing else
type Hand = [Card]

-- the Game with the different things for the State
data Game = Game {
    gDeck :: Deck,
    playerHand :: Hand,
    dealerHand :: Hand } deriving (Show)

-- used for drawing cards
type DeckState a = State Deck a
-- Contains the actual GameState
type GameState a = StateT Game a

-- Create a deck of cards and shuffle them
-- uses the super cryptic "shufle'" method
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
    dealerHand = dHand }
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


-- prints the hands and their values to the screen
printGameState :: GameState IO ()
printGameState = do
    state <- get
    liftIO $ do
        let pHand = playerHand state
            dHand = dealerHand state
        putStrLn $ "Your current hand is: " 
        putStrLn $ show pHand
        putStrLn $ "which values to: " ++ show (smartValue pHand)
        putStrLn $ ""
        putStrLn $ "Dealer's hand: " 
        putStrLn $ show (head dHand) ++ " + X"
        putStrLn $ "which values to: " ++ show (smartValue (init dHand)) ++ " + X"


-- the main loop for the player
handlePlayer :: GameState IO ()
handlePlayer = do
    -- print the game's state first
    printGameState
    curr <- get
    -- ask the player's input
    input <- liftIO $ do
        putStrLn $ ""
        putStrLn $ "What will you do (hit/stay)"
        putStrLn $ ""
        input <- getLine
        -- return the input
        return input

    if (input == "stay")
        -- player chose to stay
        then liftIO . putStrLn $ "You stayed!"
        else if (input == "hit") 
            then do
                -- lift a card from the State Deck
                let (newCard, newDeck) = runState drawCard $ gDeck curr     
                -- set the new deck back to state and the new players hand
                put curr { gDeck = newDeck, playerHand = newCard : playerHand curr }
                -- make a check for insta-lose
                newState <- get
                let newHand = playerHand newState
                if smartValue newHand <= 21
                    -- if player didn't bust, loop
                    then handlePlayer
                    else liftIO . putStrLn $ "You went over!"
            -- if input is gibberish
            else do 
                liftIO . putStrLn $ "Didn't get that..."
                handlePlayer

-- dealer lifts card to the dealerHand
handleDealer :: GameState IO ()
handleDealer = do
    currentState <- get
    let dValue = smartValue (dealerHand currentState)
    -- dealer always lift when under 17
    if dValue <= 16
        then do 
            -- draw a card to dealers hand from the state deck
            let (newCard, newDeck) = runState drawCard $ gDeck currentState
            -- put the new back to state and the dealers hand too
            put currentState { gDeck = newDeck, dealerHand = newCard : dealerHand currentState }
            -- get the new state to show the dealers hand now
            newState <- get
            let dHand = dealerHand newState
            liftIO . putStrLn $ "Dealer draws: "
            liftIO . putStrLn $ show dHand
            liftIO . putStrLn $ "which values to: " ++ show (smartValue dHand)
            -- after drawing, loop
            handleDealer
        else do
            -- if dealers hand is over 16, the dealer stays
            liftIO . putStrLn $ "Dealer stays!"
  
-- return the hand value, if it's over 21, try to use the one with the small ace
smartValue :: Hand -> Int
smartValue h = do 
    let possibleValues = handValue h
    if (head possibleValues) <= 21
        -- this is the "normal" hand value
        then head possibleValues
        -- and this the one with a possible "small" Ace
        else last possibleValues


-- Calculate possible values of the hand, return [Int]
handValue :: Hand -> [Int]
handValue [] = [0, 0]
-- zipWith sums lists smartly, like [1, 2] + [1, 2] = [2, 4]
handValue (h:hs) = zipWith (+) (value h) (handValue hs)

-- evaluate the CardValue's of a single Card
value :: Card -> [Int]
value (Card _ x) = intValue x

-- Map the CardValue's to Int
-- Returns a [Int], where the first element is the value with a possible "big" Ace
-- and second element is with a "small" ace"
intValue :: CardValue -> [Int]
intValue Two = [2, 2]
intValue Three = [3, 3]
intValue Four = [4, 4]
intValue Five = [5, 5]
intValue Six = [6, 6]
intValue Seven = [7, 7]
intValue Eight = [8, 8]
intValue Nine = [9, 9]
intValue Ten = [10, 10]
intValue Jack = [10, 10]
intValue Queen = [10, 10]
intValue King = [10, 10]
-- here is the ace difference
intValue Ace = [11, 1]


-- checks from the State which party won
checkWin :: GameState IO ()
checkWin = do
    state <- get
    -- get the values
    let pValue = smartValue (playerHand state)
        dValue = smartValue (dealerHand state)
    -- check if player over
    if pValue > 21
        then handleEnd "Lose"
        -- check if dealer over
        else if dValue > 21
            then handleEnd "Win"
            -- none of them over, who bigger?!
            else if pValue > dValue
                then handleEnd "Win"
                else handleEnd "Lose"

-- Just tells if the player lost or won
-- Input either "Win" or "Lose"
handleEnd :: String -> GameState IO ()
handleEnd s = do
    state <- get
    let pHand = playerHand state
        dHand = dealerHand state
    liftIO . putStrLn $ "##### You " ++ s ++ "!  #####"
    liftIO . putStrLn $ "Your hand, value " ++ show (smartValue pHand)
    liftIO . putStrLn $ show pHand
    liftIO . putStrLn $ ""
    liftIO . putStrLn $ "Dealer's hand, value " ++ show (smartValue dHand)
    liftIO . putStrLn $ show dHand


-- simply runs the turns and checks the result
runGame :: GameState IO ()
runGame = do
    handlePlayer
    handleDealer
    checkWin


-- remember the fugin T
main :: IO ()
main = do
    g <- newStdGen
    -- start's the game with shuffling the deck and dealing the initial stuff
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
