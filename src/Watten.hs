{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns   #-}

module Watten where

import           Prelude             hiding (and, not, or, (&&), (||))
import           Utility
import           Utility.Cond
import           Utility.Choice

import           Cards
import qualified Cards.Parse         as Parse
import           Cards.Shuffle

import           Trick.Rules

import           Watten.AI           (GameState (..), defaultState, isFstParty, sameParty, agog)
import qualified Watten.AI           as AI
import           Watten.GameTypes
import           Watten.Score

import           Data.List
import           Data.Maybe
import           Text.Printf
import           Control.Applicative
import           Control.Monad

suitChars, rankChars :: [Char]
suitChars = ['s', 'h', 'g', 'e']
rankChars = if Ten < Under then ['7', '8', '9', 'X', 'U', 'O', 'K',      'A']
                           else ['7', '8', '9',      'U', 'O', 'K', 'X', 'A']

readHand :: String -> IO Hand
readHand = Parse.readHand suitChars rankChars

readCard :: String -> IO Card
readCard = Parse.readCard suitChars rankChars

readRank :: String -> IO Rank
readRank = Parse.readRank           rankChars

readSuit :: String -> IO Suit
readSuit = Parse.readSuit suitChars

readInt :: String -> IO Int
readInt m = do
    putStrLn m
    s <- getLine
    case fmap fst . listToMaybe . reads $ s of
        Nothing -> putStrLn "Bitte Zahl eingeben." >> readInt m
        Just i  -> return i

mainWatten :: IO ()
shuffle, declaration, play, fin :: GameState -> IO GameState
-- TODO: - Ausschaffen (mit '!' vor Karte)

mainWatten = do
    playerCount <- readInt "Bitte Spieleranzahl eingeben."
        `untilM` (between (2, 6), putStrLn "Spieleranzahl muss zwischen 2 und 6 liegen.")
    finish <- readInt "Bitte Punkte bis Spielende eingeben."
        --`untilM` (between (11, 21), putStrLn "Der Wert liegt sinnvollerweise zwischen 11 und 21.")
    
    state <- defaultState finish playerCount
    GameState { score, playerNames } <- foldCM state iterState shuffle (maximum . score $< finish)
    putStrLn $ "\nGewinner: " ++ concatNatural (playerNames !!! indicesOfMaxima score)
  where
    iterState state @ GameState { beginnerNo, playerCount } = state { beginnerNo = beg, no = beg } where beg = (beginnerNo + 1) `mod` playerCount

shuffle state = do
    hands <- getCards 1 (playerCount state) 5 -- 1 deck, p players, 5 cards each
    declaration $ state { hands }

declaration state @ GameState
  {
    playerNames,
    playerCount = 3,
    isHuman,
    no,
    hands
  } = do
    Card s r <- getCard no
    let rule = bidding r s
    play state { rule, rechter = Card s r, hands = sortTRBy rule rank suit <$> hands }
  where
    getCard n
        | isHuman n = do
            let h = hands !! n
            putStrLn $ "Dein Blatt: " ++ showListNatural (sortBy ordRank h)
            readCard "Du bist dran den Hauptschlag bestimmen."
        | otherwise = do
            c <- AI.schlagFarbe n state
            putStrLn $ playerNames !! n ++ " sagt " ++ show c ++ " an."
            return c

declaration state @ GameState
  {
    playerNames,
    playerCount,
    isHuman,
    no,
    hands
  } = do
    r <- getRank no
    putStrLn ""
    s <- getSuit r $ dim no mod playerCount 1
    putStrLn ""
    let rule = bidding r s
    play state { rule, rechter = Card s r, hands = sortTRBy rule rank suit <$> hands }
  where
    getRank n
        | isHuman n = do
            let h = hands !! n
            putStrLn $ "Dein Blatt: " ++ showListNatural (sortBy ordRank h)
            readRank "Du bist dran den Schlag anzusagen!"
        | otherwise = do
            r <- AI.schlag n state
            putStrLn $ playerNames !! n ++ " sagt " ++ show r ++ " als Schlag an."
            return r

    getSuit r n
        | isHuman n = do
            let h = hands !! n
            putStrLn $ "Der angesagte Schlag ist " ++ show r ++ "."
            putStrLn $ "Dein Blatt: " ++ showListNatural (sortBy ordSuit h)
            readSuit "Du bist dran den Trumpf anzusagen!"
        | otherwise = do
            s <- AI.trumpf r n state
            putStrLn $ playerNames !! n ++ " sagt " ++ show s ++ " als Trumpf an."
            return s

play state = do
    state <- foldCM state id trick (maximum . takenTr $< 3)
    putStrLn ""
    fin state
  where
    -- Right: Es wurde ausgeschafft (Nothing: gegangen); Left nicht.
    trick state @ GameState { playerNames, playerCount, isHuman, beginnerNo, no, rechter, rule, lastExpeIsF, takenTr, hands, gameValue } = do
        playedCardsLR <- foldUM (Left []) cons action [0 .. playerCount-1]
        case compact playedCardsLR of
            Nothing          -> fin state
            Just playedCards ->
                do let (add no mod playerCount -> plNo, crd) = takesTrick rule playedCards
                   putStrLn $ (if plNo == 0 then "Deine Karte"
                                            else show crd ++ " von " ++ playerNames !! plNo)
                              ++ " gewinnt den Stich."
                   putStrLn ""
                   return state
                     {
                       no        = plNo,
                       takenTr   = newTakenTr plNo,
                       hands     = filter (notInList playedCards) <$> hands,
                       gameValue = if isLeft playedCardsLR then gameValue else gameValue + 1
                     }
      where
        compact (Left c)         = Just c
        compact (Right (Just c)) = Just c
        compact _                = Nothing
        
        cons (Left c)         (Left cs)         = Left $ c:cs
        cons (Right (Just c)) (Left cs)         = Right $ Just $ c : cs
        cons (Right Nothing)  _                 = Right Nothing
        cons (Left c)         (Right (Just cs)) = Right $ Just $ c : cs -- this should not occur
        cons (Right (Just c)) (Right (Just cs)) = Right $ Just $ c : cs
        
        action  n (Left  t)        = action' n t
        action  n (Right (Just t)) = action' n t
        action  n (Right Nothing)  = return $ Right Nothing
        action' n t = do
            --print n
            --putStrLn $ "Gespannt: " ++ (show $ agog state n)
            --putStrLn $ "Zuletzt ausgeschafft hat 1. P: " ++ show lastExpeIsF
            --putStrLn $ "Spieler in 1. P: " ++ (show $ isFstParty state n)
            a <- if | agog state n                                     -> return False
                    | Just b <- lastExpeIsF, isFstParty state n /= b   -> return False
                    | otherwise                                        -> wantExpel n
            b <- if a then answerExpel n $ fromJust $ findIndex (sameParty state n) $ [n + 1 .. n + playerCount] <**> pure (`mod` playerCount)
                      else return False
            if a then if b then return $ Right Nothing                -- expelled and gave up
                           else giveBy n t >>= return . Right . Just  -- expelled and continued
                 else           giveBy n t >>= return . Left          -- not expelled
        
        giveBy = giveBy' . add no mod playerCount
        giveBy' n (reverse -> t)
            | isHuman n = do -- Player 0 is human player
                let h = hands !! n
                putStrLn $ if null t then "Du darfst mit folgenden Karten herauskommen:" else "Du darfst die folgenden Karten ausspielen:"
                putStrLn $ "Dein Blatt: " ++ showListNatural h
                readCard "Welche Karte soll es sein?"
                    `untilM` (inList h, putStrLn "Du hast diese Karte nicht.")
                    `untilM` (cond t h, putStrLn "Trumpf oder Kritisch: Du darfst diese Karte nicht ausspielen.")
            | otherwise = do
                let h' = hands !! n
                    h  = filter (cond t h') h'
                c <- AI.play t h state
                putStrLn $ (playerNames !! n) ++ (if null t then " kommt mit " ++ show c ++ " heraus." else " gibt " ++ show c ++ " zu.")
                return c
        
        cond [] _ = true
        cond t@(r:_) h | r /= rechter                         = true -- first card of the trick is not the rechter
                       | any (/= 0) takenTr                   = true -- not first trick
                       | snd (takesTrick rule t) /= rechter   = true -- the rechter is not the current trick taking card
                       | null h'                              = true -- the player woild have no cards to play (salvator clause)
                       | otherwise                            = inList h'
          where h' = filter (suit $== suit rechter || inList kriten) h
        
        wantExpel n
            | isHuman n = choiceBool "Möchtest du ausschaffen?"
            | otherwise = AI.expel n state
        
        answerExpel other n
            | isHuman n = choiceBool $ playerNames !! other ++ " hat dich ausgeschafft. Aufgeben?"
            | otherwise = AI.expelled other n state
        
        newTakenTr plNo = case playerCount of
            2 -> incSc plNo takenTr
            3 -> if plNo == beginnerNo then incSc plNo takenTr else incScs ([0..2] \\ [beginnerNo]) takenTr
            4 -> incScs [plNo, (plNo + 2) `mod` 4] takenTr
            5 -> let dealer = (beginnerNo - 1) `mod` 1 in
                 if plNo == beginnerNo || plNo == dealer
                    then incScs [beginnerNo, beginnerNo + 1] takenTr
                    else incScs ((`mod` 5) <$> [beginnerNo + 1 .. beginnerNo + 3]) takenTr
            6 -> incScs [plNo, (plNo + 2) `mod` 6, (plNo + 4) `mod` 6] takenTr

fin state @ GameState
  {
    playerNames,
    playerCount,
    takenTr,
    gameValue,
    score
  } = do
    let winner   = indicesOfMaxima takenTr
        newScore = addScs winner gameValue score
    putStrLn $ listNames winner ++ " drei Stiche und damit die Runde mit " ++ show gameValue ++ " Punkten für sich entschieden."
    putStrLn "Punkte:"
    
    forM_ [0 .. playerCount - 1] $ printNameScore playerNames newScore
    putStrLn "\n"
    
    let newState = state { takenTr = replicate playerCount 0, score = newScore }
    return newState { gameValue = if any (agog newState) [0 .. playerCount - 1] then 3 else 2 }
  where
    printNameScore nms sc n = printf ("%s: "++ sp ++"%2d\n") name score
      where
        name  = nms !! n
        score = sc  !! n
        w  = maximum $ length <$> playerNames
        sp = replicate (w - length name) ' '
    
    listNames [0]          = "Du hast"
    listNames [n]          = playerNames !! n ++ " hat"
    listNames winner@(0:_) = concatNatural (playerNames !!! winner) ++ ", ihr habt"
    listNames winner       = concatNatural (playerNames !!! winner) ++ " haben"