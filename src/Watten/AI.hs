{-# LANGUAGE MultiWayIf, NamedFieldPuns, ViewPatterns #-}

module Watten.AI where

import Watten.GameTypes

import Utility
import Utility.Cond
import Prelude hiding ((||), (&&), not, or, and)

import Cards
import Cards.Shuffle

import Trick.Rules

import Watten.Score

import Data.List
import Control.Applicative
import System.Random (randomRIO)

data GameState = GameState
    {
        playerNames :: [String],    -- the player's names.
        playerCount :: Int,         -- the number of players.
        
        beginnerNo  :: Int,         -- the player number of the player who started the round.
        no          :: Int,         -- the player whose turn it is.
        hands       :: [Hand],      -- the player's hands.
        rechter     :: Card,        -- the "Hauptschlag"
        rule        :: TrickRule,   -- the (generated) TrickRule.
        takenTr     :: Score,       -- the number of the player's tricks in the current round.
        gameValue   :: Int,         -- the round's value.
        
        score       :: Score        -- the player's score (gotten by winning rounds.)
    }

instance Show GameState where
    show GameState
      {
        playerNames,
        beginnerNo = b,
        no         = n,
        rechter,
        takenTr,
        gameValue,
        score
      } = "Vorhand:     " ++ playerNames !! b ++ "\n" ++
          "Ausspieler:  " ++ playerNames !! n ++ "\n" ++
          "Hauptschlag: " ++ show rechter     ++ "\n" ++
          "Stiche:      " ++ show takenTr     ++ "\n" ++
          "Spielwert:   " ++ show gameValue   ++ "\n" ++
          "Punkte:      " ++ show score

defaultState :: Int -> GameState
defaultState 2           = (defaultState 3)
    {
        playerNames = ["Du", "Неффенеий"],
        playerCount = 2,
        
        hands       = [[], []],
        takenTr     = [ 0,  0],
        
        score       = [ 0,  0]
    }

defaultState playerCount = GameState
    {
        playerNames = take playerCount ["Du", "Anton", "Benita", "Clara", "Daniel", "Egon"],
        playerCount,
        
        beginnerNo  = 0,
        no          = 0,
        hands       = replicate playerCount [],
        rechter     = Card Hearts King,
        rule        = bidding King Hearts,
        takenTr     = replicate playerCount 0,
        gameValue   = 2,
        
        score       = replicate playerCount 0
    }

remove = flip (\\)

schlag :: Int -> GameState -> IO Rank
schlag n GameState { hands = (remove kriten . (!! n) -> h) } = return $ ranks !! i
  where
    rankList = countBy . (rank $==) <$> ranks <*> pure h
    i = head $ indicesOfMaxima rankList

farbe :: Rank -> Int -> GameState -> IO Suit
farbe r n GameState { playerCount, hands = (remove kriten . (!! n) -> h) }  =
    if Hearts `elem` bestSuits then return $ Hearts
    else case bestSuits of []  -> pick suits
                           [s] -> return s
                           _   -> pick $ bestSuits \\ [Leaves]
  where
    preH = filter (rank $/= r) h
    
    cc n = if playerCount >= n then 1 else 0
    v Seven = 0
    v Eight = 0
    v Nine  = 2
    v Ten   = 3
    v Under = 4 + cc 6
    v Over  = 5 + cc 4 + cc 6 + cc 6
    v King  = 5 + playerCount + cc 6 - cc 3 -- minus is correct here
    v Ace   = 7 + playerCount + cc 5 + cc 6
    
    rkCnt Seven = 2
    rkCnt King  = 3
    rkCnt _     = 4
    
    bonus s | Card s r `elem` h   = 2 * (rkCnt r - countBy (rank $== r) h)
            | otherwise           = 0
    
    sumValue = sumBy $ v . rank
    
    index :: Hand -> Suit -> Int
    index = flip index' where index' s (filter (suit $== s) -> hd) = sumValue hd + bonus s
    
    bestSuits = maxima (index preH) suits

schlagFarbe :: Int -> GameState -> IO Card
schlagFarbe n state = do
    r <- schlag n state
    s <- farbe r n state
    return $ Card s r

type Trick = [Card]

play, stupidPlay, betterPlay :: Trick -> Hand -> GameState -> IO Card

play t h s @ GameState { rechter, rule } = play' t h s
  where
    play' = if playerCount s <= 3 then betterPlay else stupidPlay

betterPlay [] h state = return $ head h         -- first player of trick
betterPlay t  h GameState { playerCount, rule } -- last player of trick
    | length t == playerCount - 1 = do
        tryOverbidWithLeast t h rule playerCount
betterPlay t  h GameState { playerCount = 3, rule, beginnerNo, no }
    | no == beginnerNo = do                     -- 3 player, first one --> solo player; from above --> middle one to play.
        tryOverbidWithLeast t h rule 3
betterPlay t  h GameState { playerCount, rule, beginnerNo, no } = do
    undefined

tryOverbidWithLeast (reverse -> t) h rule playerCount = do
    -- here lookup returns the first 
    return $ case lookup (playerCount - 1) takerAndCard of
        Just c  -> c
        Nothing -> head h
  where
    tTrR c = takesTrick rule $ reverse $ c : t
    takerAndCard = tTrR <$> h -- for each card in my hand, tell me who takes the trick.

stupidPlay [] h _ = return $ head h
stupidPlay _  h _ = return $ last h