{-# LANGUAGE NamedFieldPuns, ViewPatterns #-}

module Watten.AI where

import Watten.GameTypes

import Utility
import Utility.Cond
import Prelude hiding ((||), (&&), not, or, and)

import Cards

import Trick.Rules

import Watten.Score

import Data.List
import Control.Applicative

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
        gameValue
      } = "Vorhand:    " ++ playerNames !! beginnerNo ++ "\n" ++
          "Ausspieler: " ++ playerNames !! no ++ "\n" ++
          "Spielstand: " ++ show score ++ "\n" ++
          "Spielwert:  " ++ show gameValue

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

schlag :: Hand -> IO Rank
schlag (remove kriten -> h) = return $ ranks !! i
  where
    rankList = (countBy . (rank $==) <$> ranks) <*> pure h
    mr = maximum rankList
    Just i = elemIndex mr rankList

farbe :: Rank -> Hand -> IO Suit
farbe r (remove kriten -> h) = return $ case veryBestSuits of
    []    -> head bestSuits
    (s:_) -> s
  where
    preH = filter (rank $/= r) h
    
    rValue Ace   = 11
    rValue King  = 8
    rValue Over  = 6
    rValue Eight = 0
    rValue r     = fromEnum r
    sumValue     = sum . map ((10 +) . rValue . rank)
    
    suitCmp c d = suit c `compare` suit d
    suitEq  c d = suit c == suit d
    eqvGroups   = groupBy suitEq . sortBy suitCmp $ preH

    bestSuits     = suit . head <$> maximaBy sumValue eqvGroups
    veryBestSuits = suit <$> filter (rank $== r && (`elem` bestSuits) . suit) h

type Trick = [Card]

play, stupidPlay :: Trick -> Hand -> GameState -> IO Card
play = stupidPlay

stupidPlay [] h state = return $ head h
stupidPlay t  h state = return $ last h