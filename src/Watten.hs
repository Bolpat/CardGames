{-# LANGUAGE ViewPatterns, NamedFieldPuns #-}

module Watten where

import Utility
import Utility.Cond
import Prelude hiding ((||), (&&), not, or, and)

import Cards
import qualified Cards.Parse as Parse
import Cards.Shuffle

import Trick.Rules

import Watten.GameTypes
import Watten.Score
import Watten.AI (GameState(..), defaultState)
import qualified Watten.AI as AI

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative
import Control.Exception

suitChars = ['s', 'h', 'g', 'e']
rankChars = if Ten < Under then ['7', '8', '9', 'X', 'U', 'O', 'K', 'A']
                           else ['7', '8', '9', 'U', 'O', 'K', 'X', 'A']

parseHand = Parse.parseHand suitChars rankChars
readCard  = Parse.readCard suitChars rankChars

readInt :: String -> IO Int
readInt m = do
    putStrLn m
    s <- getLine
    case fmap fst . listToMaybe . reads $ s of
        Nothing -> putStrLn "Bitte Zahl eingeben." >> readInt m
        Just i  -> return i

mainWatten :: IO ()

mainWatten = do
    readInt "Bitte Spieleranzahl eingeben."
        `untilM` ((2 <=) && (<= 6), putStrLn "Spieleranzahl muss zwischen 2 und 6 liegen.")
    foldCM (defaultState 4) iterState playWatten (maximum . bilance $< 15)
    putStrLn ""
  where iterState state @ GameState { beginnerNo } = state { beginnerNo = beginnerNo + 1, no = beginnerNo +1 }

playWatten state @ GameState { playerCount } = do
    (map $ sortBy ordRank -> hands) <- getCards 1 playerCount 5 -- 1 deck, x players, 5 cards each
    mainWattenAnsage $ state { hands }

mainWattenAnsage state @ GameState { playerNames, playerCount, no, hands = hands @ (h0:_) } = do
    r <- getRank no
    putStrLn ""
    s <- getSuit r ((sub 1 mod playerCount) no)
    putStrLn ""
    let rule = bidding r s
    mainWattebPlay state { rule, hands = sortTRBy rule rank suit <$> hands }
  where
    getRank 0 = do -- 0 human Player
        putStrLn $ "Dein Blatt: " ++ showListNatural h0
        Parse.readRank rankChars "Du bist dran den Schlag anzusagen!"
    getRank n  = do
        r <- AI.schlag (hands !! n)
        putStrLn $ playerNames !! n ++ " sagt " ++ show r ++ " als Schlag an."
        return r
        
    getSuit r 0 = do
        putStrLn $ "Der angesagte Schlag ist " ++ show r ++ "."
        putStrLn $ "Dein Blatt :" ++ showListNatural h0
        Parse.readSuit suitChars "Du bist dran den Trumpf anzusagen!"
    getSuit r n = do
        s <- AI.farbe r (hands !! n)
        putStrLn $ playerNames !! n ++ " sagt " ++ show s ++ " als Trumpf an."
        return s
        
mainWattebPlay state = do
    state <- foldCM state id trick (maximum . score $< 3)
    putStrLn ""
    mainFinish state
  where
    trick state @ GameState { playerNames, playerCount, beginnerNo, no, rule, score, hands = hs@(h0:_) } = do
        (reverse -> playedCards) <- foldUM [] (:) giveBy [0 .. 3]
        let (crd, add no mod 4 -> plNo) = takesTrick rule playedCards
            newScore =
                case playerCount of
                    2  ->  incSc plNo score
                    3  ->  if plNo == beginnerNo then incSc plNo score else incScs ([0..2] \\ [plNo]) score
                    4  ->  incScs [plNo, (plNo + 2) `mod` 4] score
                    5  ->  let dealer = mod (beginnerNo - 1) 5 in
                               if plNo == beginnerNo || plNo == dealer
                                then incScs [beginnerNo, beginnerNo + 1] score
                                else incScs (flip mod 5 <$> [beginnerNo + 1 .. beginnerNo + 3]) score
                    6  ->  incScs [plNo, (plNo + 2) `mod` 6, (plNo + 4) `mod` 6] score
        putStrLn $ if plNo == 0 then "Deine Karte macht den Stich."
                                else show crd ++ " von " ++ playerNames !! plNo ++ " gewinnt den Stich."
        putStrLn ""
        return state
          {
            no    = plNo,
            score = newScore,
            hands = filter (`notElem` playedCards) <$> hs
          }
      where
        giveBy = giveBy' . add no mod 4
        giveBy' 0 t = do -- Player 0 is human player
                    putStrLn $ if null t then "Du darfst mit folgenden Karten herauskommen:" else "Du darfst die folgenden Karten ausspielen:"
                    putStrLn $ "Dein Blatt: " ++ showListNatural h0
                    readCard "Welche Karte soll es sein?"
                        `untilM` ((`elem` h0), putStrLn "Du hast diese Karte nicht.")
        giveBy' n t = do
            c <- AI.play t (hs !! n) state
            putStrLn $ (playerNames !! n) ++ (if null t then " kommt mit " ++ show c ++ " heraus." else " gibt " ++ show c ++ " zu.")
            return c
        
mainFinish state @ GameState
  {
    playerNames,
    playerCount,
    score,
    gameValue,
    bilance
  } = do
    let winner = indicesOfMaxima score
        newBilance = addScs winner gameValue bilance
    putStrLn $ concatNatural ((playerNames !!) <$> winner) ++ " haben die Runde mit " ++ show gameValue ++ " für sich entschieden."
    putStrLn "Punkte:"
    
    forM_ [0 .. playerCount-1] $
        putStrLn . \i -> playerNames !! i ++ ": " ++ show (newBilance !! i)
    return state { score = replicate playerCount 0, bilance = newBilance }