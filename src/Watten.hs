{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns   #-}

module Watten where

import           Prelude             hiding (and, not, or, (&&), (||))
import           Utility
import           Utility.Cond

import           Cards
import qualified Cards.Parse         as Parse
import           Cards.Shuffle

import           Trick.Rules

import           Watten.AI           (GameState (..), defaultState)
import qualified Watten.AI           as AI
import           Watten.GameTypes
import           Watten.Score

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Maybe

import           Debug.Trace

suitChars = ['s', 'h', 'g', 'e']
rankChars = if Ten < Under then ['7', '8', '9', 'X', 'U', 'O', 'K',      'A']
                           else ['7', '8', '9',      'U', 'O', 'K', 'X', 'A']

readHand = Parse.readHand suitChars rankChars
readCard = Parse.readCard suitChars rankChars
readRank = Parse.readRank           rankChars
readSuit = Parse.readSuit suitChars

readInt :: String -> IO Int
readInt m = do
    putStrLn m
    s <- getLine
    case fmap fst . listToMaybe . reads $ s of
        Nothing -> putStrLn "Bitte Zahl eingeben." >> readInt m
        Just i  -> return i

mainWatten :: IO ()

-- TODO: - Trumpf oder Kritisch
-- TODO: - Ausschaffen (mit ! vor Karte)

mainWatten = trace "" $ do
    playerCount <- readInt "Bitte Spieleranzahl eingeben."
        `untilM` (between (2, 6), putStrLn "Spieleranzahl muss zwischen 2 und 6 liegen.")
    finish <- readInt "Bitte Punkte bis Spielende eingeben."
        `untilM` (between (10, 21), putStrLn "Der Wert liegt sinnvollerweise zwischen 10 und 21.")
    GameState { bilance, playerNames } <- foldCM (defaultState playerCount) iterState playWatten (maximum . bilance $< finish)
    putStrLn ""
    putStrLn ""
    putStrLn $ "Gewinner:" ++ concatNatural (showListNatural <$> (playerNames !!! indicesOfMaxima bilance))
  where
    iterState state @ GameState { beginnerNo, playerCount } =
        let beg = (beginnerNo + 1) `mod` playerCount in state { beginnerNo = beg, no = beg }

playWatten state @ GameState { playerCount } = do
    hands <- getCards 1 playerCount 5 -- 1 deck, given count of players, 5 cards each
    mainWattenAnsage $ state { hands }

mainWattenAnsage state @ GameState { playerNames, playerCount = 3, no, hands = hs @ (h0 : _) } = do
    Card s r <- getCard no
    let rule = bidding r s
    mainWattebPlay state { rule, hands = sortTRBy rule rank suit <$> hs }
  where
    getCard 0 = do
        putStrLn $ "Dein Blatt: " ++ showListNatural (sortBy ordRank h0)
        readCard "Du bist dran den Hauptschlag bestimmen."
    getCard n = do
        c <- AI.schlagFarbe n state
        putStrLn $ playerNames !! n ++ " sagt " ++ show c ++ " an."
        return c

mainWattenAnsage state @ GameState { playerNames, playerCount, no, hands = hands @ (h0:_) } = do
    r <- getRank no
    putStrLn ""
    s <- getSuit r ((sub 1 mod playerCount) no)
    putStrLn ""
    let rule = bidding r s
    mainWattebPlay state { rule, hands = sortTRBy rule rank suit <$> hands }
  where
    getRank 0 = do -- 0 human Player
        putStrLn $ "Dein Blatt: " ++ showListNatural (sortBy ordRank h0)
        readRank "Du bist dran den Schlag anzusagen!"
    getRank n  = do
        r <- AI.schlag n state
        putStrLn $ playerNames !! n ++ " sagt " ++ show r ++ " als Schlag an."
        return r

    getSuit r 0 = do
        putStrLn $ "Der angesagte Schlag ist " ++ show r ++ "."
        putStrLn $ "Dein Blatt: " ++ showListNatural (sortBy ordSuit h0)
        readSuit "Du bist dran den Trumpf anzusagen!"
    getSuit r n = do
        s <- AI.farbe r n state
        putStrLn $ playerNames !! n ++ " sagt " ++ show s ++ " als Trumpf an."
        return s

mainWattebPlay state = do
    state <- foldCM state id trick (maximum . score $< 3)
    putStrLn ""
    mainFinish state
  where
    trick state @ GameState { playerNames, playerCount, beginnerNo, no, rule, score, hands = hs@(h0:_) } = do
        (reverse -> playedCards) <- foldUM [] (:) giveBy [0 .. playerCount-1]
        let (add no mod playerCount -> plNo, crd) = takesTrick rule playedCards
        putStrLn $ if plNo == 0 then "Deine Karte macht den Stich."
                                else show crd ++ " von " ++ playerNames !! plNo ++ " gewinnt den Stich."
        putStrLn ""
        return state
          {
            no    = plNo,
            score = newScore plNo,
            hands = filter (`notElem` playedCards) <$> hs
          }
      where
        giveBy = giveBy' . add no mod playerCount
        giveBy' 0 t = do -- Player 0 is human player
                    putStrLn $ if null t then "Du darfst mit folgenden Karten herauskommen:" else "Du darfst die folgenden Karten ausspielen:"
                    putStrLn $ "Dein Blatt: " ++ showListNatural h0
                    readCard "Welche Karte soll es sein?"
                        `untilM` ((`elem` h0), putStrLn "Du hast diese Karte nicht.")
        giveBy' n t = do
            c <- AI.play t (hs !! n) state
            putStrLn $ (playerNames !! n) ++ (if null t then " kommt mit " ++ show c ++ " heraus." else " gibt " ++ show c ++ " zu.")
            return c
        newScore plNo = case playerCount of
                    2  ->  incSc plNo score
                    3  ->  if plNo == beginnerNo then incSc plNo score else incScs ([0..2] \\ [plNo]) score
                    4  ->  incScs [plNo, (plNo + 2) `mod` 4] score
                    5  ->  let dealer = (beginnerNo - 1) `mod` 5 in
                               if plNo == beginnerNo || plNo == dealer
                                    then incScs [beginnerNo, beginnerNo + 1] score
                                    else incScs (flip mod 5 <$> [beginnerNo + 1 .. beginnerNo + 3]) score
                    6  ->  incScs [plNo, (plNo + 2) `mod` 6, (plNo + 4) `mod` 6] score
mainFinish state @ GameState
  {
    playerNames,
    playerCount,
    score,
    gameValue,
    bilance
  } = do
    let winner     = indicesOfMaxima score
        newBilance = addScs winner gameValue bilance
    putStrLn $ printW winner ++ " drei Stiche und damit die Runde mit " ++ show gameValue ++ " Punkten für sich entschieden."
    putStrLn "Punkte:"

    forM_ [0 .. playerCount-1] $
        putStrLn . \n -> playerNames !! n ++ ": " ++ show (newBilance !! n)
    putStrLn "\n"

    return state { score = replicate playerCount 0, bilance = newBilance }
  where
    printW [0]          = "Du hast"
    printW [n]          = playerNames !! n ++ " hat"
    printW winner@(0:_) = concatNatural (playerNames !!! winner) ++ ", ihr habt"
    printW winner       = concatNatural (playerNames !!! winner) ++ " haben"
