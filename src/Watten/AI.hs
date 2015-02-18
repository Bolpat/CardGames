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
        isHuman     :: Int -> Bool, -- determines if the player with the given number exists and is human.
        
        beginnerNo  :: Int,         -- the player number of the player who started the round.
        no          :: Int,         -- the player whose turn it is.
        hands       :: [Hand],      -- the player's hands.
        rechter     :: Card,        -- the "Hauptschlag"
        rule        :: TrickRule,   -- the (generated) TrickRule.
        lastExpeIsF :: Maybe Bool,  -- states if last expeller is in first player's party (when there is an expeller).
        takenTr     :: Score,       -- the number of the player's tricks in the current round.
        gameValue   :: Int,         -- the round's value.
        
        score       :: Score,       -- the player's score (gotten by winning rounds.)
        finish      :: Int          -- the score necessary to win (2 below: agog)
    }

-- | determines if the player with the given number is in the same team with the beginner player.
isFstParty :: GameState -> Int -> Bool
isFstParty = flip isFstParty'
  where
    isFstParty' n GameState { playerCount,     beginnerNo } | even playerCount  = even $ (n + beginnerNo) `mod` playerCount
    isFstParty' n GameState { playerCount = 3, beginnerNo }                     = n == beginnerNo
    isFstParty' n GameState { playerCount = 5, beginnerNo }                     = n == beginnerNo || n == (beginnerNo - 1) `mod` 5
    isFstParty' _ _                                                             = False

sameParty :: GameState -> Int -> Int -> Bool
sameParty s m n = isFstParty s m == isFstParty s n

-- | determines if the player with the given number is "gespannt".
agog :: GameState -> Int -> Bool
agog = flip agog' where agog' n GameState { score, finish } = score !! n > finish - 3

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

defaultState :: Int -> Int -> IO GameState
defaultState f 2          = do
    b <- pick [0, 1]
    s <- defaultState f 3
    return s
      {
        playerNames = ["Du", "Неффения"],
        playerCount = 2,
        beginnerNo  = b,
        no          = b,
        
        hands       = [[], []],
        takenTr     = [ 0,  0],
        
        score       = [ 0,  0]
      }

defaultState f playerCount = do
    (subtract 1 -> b) <- pick [1 .. playerCount]
    return GameState
      {
        playerNames = take playerCount ["Du", "Anton", "Benita", "Clara", "Daniel", "Egon"],
        playerCount,
        isHuman     = (== 0),
        
        beginnerNo  = b,
        no          = b,
        hands       = replicate playerCount [],
        rechter     = Card Hearts King,
        rule        = bidding King Hearts,
        lastExpeIsF = Nothing,
        takenTr     = replicate playerCount 0,
        gameValue   = 2,
        
        score       = replicate playerCount 0,
        finish      = f
      }

remove = flip (\\)

schlag :: Int -> GameState -> IO Rank
schlag n GameState { hands = (remove kriten . (!! n) -> h) } = return $ ranks !! i
  where
    rankList = (countBy . (rank $==) <$> ranks) <*> pure h
    mr = maximum rankList
    Just i = elemIndex mr rankList

trumpf :: Rank -> Int -> GameState -> IO Suit
trumpf r n GameState { playerCount, hands = (remove kriten . (!! n) -> h) }  = --do
    --putStr "Indices: "
    --putStrLn $ concatNatural $ zipWith (\a b -> show a ++ ": " ++ show b) suits $ (index preH) <$> suits
    if Hearts `elem` bestSuits then return $ Hearts
    else case bestSuits of []  -> pick suits
                           [s] -> return s
                           _   -> pick $ bestSuits \\ [Leaves]
  where
    preH = filter (rank $/= r) h
    
    cc n = if playerCount >= n then 1 else 0
    v Seven = 10
    v Eight = 10
    v Nine  = 12
    v Ten   = 13
    v Under = 14 + cc 6
    v Over  = 15 + cc 4 + cc 6 + cc 6
    v King  = 15 + playerCount + cc 6 - cc 3 -- minus is correct here
    v Ace   = 17 + playerCount + cc 5 + cc 6
    
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
    s <- trumpf r n state
    return $ Card s r

expel :: Int -> GameState -> IO Bool
expel n _ = return False

expelled :: Int -> Int -> GameState -> IO Bool
expelled other n _ = return False

type Trick = [Card]

play, stupidPlay, betterPlay :: Trick -> Hand -> GameState -> IO Card

play t h s @ GameState { rechter, rule } = do
    --let h0' = filter (suit $== suit rechter || inList kriten) h
    --let h0  = if null h0' then h else h0'
    --let h'  = if null t || head t /= rechter || snd (takesTrick rule t) /= rechter then h else h0
    --play' t h' s
    play' t h s
  where
    play' = if playerCount s == 2 then betterPlay else stupidPlay

betterPlay [] h GameState { takenTr, rechter }
    | maximum takenTr == 0, rechter `elem` h  = return rechter
betterPlay [] h _ = return $ head h             -- first player of trick
betterPlay t  h GameState { playerCount, rule } -- last player of trick
    | length t == playerCount - 1 = do
        tryOverbidWithLeast t h rule playerCount
betterPlay t  h GameState { playerCount = 3, rule, beginnerNo, no }
    | no == beginnerNo = do                     -- 3 player, first one --> solo player; from above --> middle one to play.
        tryOverbidWithLeast t h rule 3
betterPlay t  h GameState { playerCount, rule, beginnerNo, no } = do
    undefined

tryOverbidWithLeast (reverse -> t) h rule playerCount = do
    return $ case lookup (playerCount - 1) takerAndCard of
        Just c  -> c
        Nothing -> head h
  where
    tTrR c = takesTrick rule $ reverse $ c : t
    takerAndCard = tTrR <$> h

stupidPlay [] h _ = return $ head h
stupidPlay _  h _ = return $ last h