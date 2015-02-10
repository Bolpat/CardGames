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
        playerNames :: [String],
        playerCount :: Int,
        
        beginnerNo  :: Int,
        no          :: Int,
        hands       :: [Hand],
        rule        :: TrickRule,
        score       :: Score,
        gameValue   :: Int,
        
        bilance     :: Score
    }

instance Show GameState where
    show GameState
      {
        playerNames,
        beginnerNo,
        no,
        score,
        gameValue,
        bilance
      } = "Vorhand:    " ++ playerNames !! beginnerNo ++ "\n" ++
          "Ausspieler: " ++ playerNames !! no ++ "\n" ++
          "Spielstand: " ++ show score ++ "\n" ++
          "Spielwert:  " ++ show gameValue ++ "\n" ++
          "Bilanz:     " ++ show bilance

defaultState :: Int -> GameState
defaultState playerCount = GameState
    {
        playerNames = ["Du", "Anton", "Benita", "Clara", "Daniel", "Egon"],
        playerCount,
        
        beginnerNo  = 0,
        no          = 0,
        hands       = replicate playerCount [],
        rule        = bidding King Hearts,
        score       = replicate playerCount 0,
        gameValue   = 2,
        
        bilance     = replicate playerCount 0
    }

remove = flip (\\)

schlag :: Int -> GameState -> IO Rank
schlag n GameState { hands = (remove kriten . (!! n) -> h) } = return $ ranks !! i
  where
    rankList = (countBy . (rank $==) <$> ranks) <*> pure h
    mr = maximum rankList
    Just i = elemIndex mr rankList

farbe :: Rank -> Int -> GameState -> IO Suit
farbe r n GameState { playerCount, hands = (remove kriten . (!! n) -> h) }  = do
    putStr "Indices: "
    putStrLn $ concatNatural $ zipWith (\a b -> show a ++ ": " ++ show b) suits $ (index preH) <$> suits
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

    bestSuits = maximaBy (index preH) suits

schlagFarbe :: Int -> GameState -> IO Card
schlagFarbe n state = do
    r <- schlag n state
    s <- farbe r n state
    return $ Card s r

type Trick = [Card]

play, stupidPlay, betterPlay :: Trick -> Hand -> GameState -> IO Card
play t h s @ GameState { playerCount = 2 } = betterPlay t h s
play t h s = stupidPlay t h s

betterPlay [] h state = return $ head h
betterPlay t  h GameState { playerCount, rule }
    | length t + 1 == playerCount = do
        print t
        print $ reverse t
        let tTrR c = takesTrick rule $ reverse $ c : t
        let takerAndCard = tTrR <$> h
        return $ case lookup (playerCount - 1) takerAndCard of
            Just c  -> c
            Nothing -> head h
    | otherwise                  = do
        
        undefined
stupidPlay [] h state = return $ head h
stupidPlay t  h state = return $ last h