{-# LANGUAGE CPP, ViewPatterns, MultiWayIf, TupleSections, NamedFieldPuns #-}

module Schafkopf where

import Utility
import Utility.Choice
import Utility.Cond

import Cards
import qualified Cards.Parse as Parse
import Cards.Shuffle

import Trick.Rules

import Schafkopf.GameTypes
import Schafkopf.Score
import Schafkopf.AI as AI

import Prelude hiding ((||), (&&), not, or, and)
import Data.List hiding (and, or)

import Control.Monad
import Control.Applicative
import Control.Exception

mainSchafkopf :: IO ()
mainGameChoice :: [Bool] -> GameState -> IO ()
mainPlay   :: GameState -> IO ()
mainFinish :: GameState -> IO ()
mainFinishSolo :: Bool -> Int -> Int -> GameState -> IO ()

mainSchafkopf = do
    (map $ sortTR normalTR -> hs @ (h0:_)) <- getCards 1 4 8 -- 1 deck, 4 players, 8 cards each
    
    putStrLn $ "Dein Blatt: " ++ showListNatural h0
    putStrLn ""
    
    case elemIndex officers hs of -- here we use that hs are sorted (sortTR)
        Just 0 -> putStrLn  "Du hast einen Sie. Herzlichen Glückwunsch!"            >> mainSchafkopf
        Just i -> putStrLn ("Spieler " ++ show i ++ " hat einen Sie. Neues Spiel.") >> mainSchafkopf
        Nothing ->  return ()
    
    let canCall = mayCallRS normalTrumps h0
        mayCall = or canCall
        games = ("Weiter", Ramsch) : option mayCall ("Rufspiel", Rufspiel Hearts)
    
    _g <- choiceNum "Was für ein Spiel soll es sein?" $ games ++
                    [
                        ("Farb-Wenz", Wenz $ Just Hearts),
                        ("Wenz",      Wenz Nothing),
                        ("Solo",      Solo Hearts)
                    ]

    let state = AI.overbid hs _g -- this + expr above can be (AI.overbid hs -> state) <- (expr above)
    
    case player state of
        Just 0  -> putStrLn   "Du darfst spielen."                                           >> mainGameChoice canCall state
        Just n  -> putStrLn ("Spieler " ++ show n ++ " spielt " ++ show (game state) ++ ".") >> mainPlay state
        Nothing -> putStrLn "Wir spielen einen Ramsch."                                      >> mainPlay state

-- Rufspiel
mainGameChoice canCall state @ AI.GameState { hands, game = Rufspiel _ } = do
    let sts = zipPred canCall suits
    calledSuit <- choiceAlpha "Wähle eine Farbe:" $ zip (show <$> sts) sts
    
    let newState = state
            {
                game    = Rufspiel calledSuit,
                rules   = playerRulesRS calledSuit hands,
                trRule  = normalTR
            }
    mainPlay newState

-- Farbwenz
mainGameChoice _ state @ (game -> Wenz (Just _)) = do
    calledSuit <- choiceAlpha "Wähle eine Farbe:" $ zipWith ((,) . show) suits suits
    let game = Wenz $ Just calledSuit
    
    let newState = state
            {
                game,
                rules   = replicate 4 $ const $ cardAllowed $ trumps game,
                trRule  = trickRule game
            }
    mainPlay newState

-- Farbsolo
mainGameChoice _ state @ (game -> Solo _) = do
    calledSuit <- choiceAlpha "Wähle eine Farbe:" $ zipWith ((,) . show) suits suits
    let game = Solo calledSuit
    let newState = state
            {
                game,
                rules   = replicate 4 $ const $ cardAllowed $ trumps game,
                trRule  = trickRule game
            }
    mainPlay newState

-- Alle anderen Spiele (Wenz, ...) nichts zu tun.
mainGameChoice _ state = mainPlay state

mainPlay st = do
    state <- foldUM st const (const trick) [1::Int .. 8]
    putStrLn ""
    mainFinish state
  where
    -- trick :: GameState -> IO GameState
    trick GameState { hands = [] }   = error "This shall not occur."
    trick GameState { rules = [] }   = error "This shall not occur."
    trick state @ GameState
            {
                playerNames,
                no,
                hands   = hs @ (h0:_),
                score,
                takenTr,
                rules   = rs @ (r0:_),
                trRule,
                condRS
            } = do
        (reverse -> playedCards) <- foldUM [] (:) giveBy [0 .. 3]
        let (add no mod 4 -> plNo, crd) = takesTrick trRule playedCards
            
        let scr = sum $ cardScore <$> playedCards
        let newScores = addSc plNo score scr
        let newTricks = addSc plNo takenTr 1
            
        putStrLn $ if plNo == 0 then "Deine Karte macht den Stich mit " ++ show scr ++ " Augen."
                                else show crd ++ " von " ++ playerNames !! plNo ++ " gewinnt den Stich mit " ++ show scr ++ " Augen."
        putStrLn ""
        putStrLn ""
            
        return state
            {
                no      = plNo,
                hands   = filter (`notElem` playedCards) <$> hs,
                score   = newScores,
                takenTr = newTricks,
                rules   = if condRS (head playedCards)
                            then replicate 4 $ const $ cardAllowed normalTrumps
                            else rs
            }
      where
        suitChars = ['s', 'h', 'g', 'e']
        rankChars = if Ten < Under then ['7', '8', '9', 'X', 'U', 'O', 'K',      'A']
                                   else ['7', '8', '9',      'U', 'O', 'K', 'X', 'A']
        readCard = Parse.readCard suitChars rankChars
            
        giveBy = giveBy' . add no mod 4
        giveBy' 0 t = do -- Player 0 is human player
            putStrLn   "Du hast die folgenden Karten:"
            putStrLn $ showListNatural h0
            putStrLn $ if null t then "Du darfst mit folgenden Karten herauskommen:" else "Du darfst die folgenden Karten ausspielen:"
            putStrLn $ showListNatural availableCards
            
            readCard "Welche Karte soll es sein?"
                `untilM` ((`elem` h0),             putStrLn "Du hast diese Karte nicht.")
                `untilM` ((`elem` availableCards), putStrLn "Diese Karte darfst du nicht ausspielen.")
          where
            availableCards = if null r then h0 else r
            r = filter (r0 h0 $ maybeLast t) h0
        giveBy' n t = do
            c <- AI.play t availableCards state
            putStrLn $ (playerNames !! n) ++ (if null t then " kommt mit " ++ show c ++ " heraus." else " gibt " ++ show c ++ " zu.")
            return c
          where
            hn = hs !! n
            rn = rs !! n
            av = filter (rn hn $ maybeLast t) hn
            availableCards = if null av then hn else av -- Salvatorische Klausel

mainFinish GameState
  {
    game    = Rufspiel _,
    
    playerNames,
    player  = Just p,
    mate    = Just mt,
    score
  } | p == 0 || mt == 0   = finalMessage     (p + mt) playerPartySc contraPartySc -- human is in player party
    | otherwise           = finalMessage (6 - p - mt) contraPartySc playerPartySc -- human is in contra party
      where
        playerParty = [p, mt]
        contraParty = [0 .. 3] \\ playerParty
        
        playerPartySc = (score !! p)  +  (score !! mt)
        contraPartySc = 120 - playerPartySc
        
        winnerParty = if contraPartySc < 60 then playerParty else contraParty
        schneider   = if playerPartySc <= 30 || contraPartySc <  30 then 2 else 0
        schwarz     = if playerPartySc ==  0 || contraPartySc ==  0 then 2 else 0
        
        finalMessage :: Int -> Int -> Int -> IO ()
        finalMessage m hs os = do -- human + mate score / other's score
            putStrLn $ "Du spieltest mit Spieler " ++ (playerNames !! m) ++ " zusammen."
            putStrLn $ "Ihr habt " ++ show hs ++ " Augen gemacht."
            putStrLn $ if   | hs == 0   -> "Schwarz... wohl arg Pech gehabt."
                            | hs < 32   -> "Ihr seid im Schneider; das kostet."
                            | hs < 61   -> "Einmal wird man vom Bären gefressen, ein andermal frisst einen der Bär."
                            | os == 0   -> "Schwarz! So ein Spiel sieht man nicht alle Tage."
                            | os < 30   -> "Sie sind Schneider! Alle Achtung."
                            | otherwise -> "Ein einfacher Sieg ist auch ein Sieg."
            mapM_ (putStrLn . payget) [0..3]
        payget :: Int -> String
        payget n | n `elem` winnerParty   = (if n == 0 then "Du bekommst " else playerNames !! n ++ " bekommt ")
                        ++ show ((2 :: Int) + schneider + schwarz) ++ "."
                 | otherwise              = (if n == 0 then "Du zahlst " else playerNames !! n ++ " zahlt ")
                        ++ show ((2 :: Int) + schneider + schwarz) ++ "."
mainFinish GameState
  {
    game = Ramsch,
    
    playerNames,
    score,
    takenTr = (elemIndex 8 -> durchmarsch)
  } | Just n <- durchmarsch   = do
        putStrLn $ if n == 0 then "Du hast einen Durchmarsch geschafft! Wow."
                             else  playerNames !! n ++ " hat einen Durchmarsch geschafft."
        mapM_ (putStrLn . payget) $ sort $ zip (snd <$> reverse orderedScore) (snd payment)
        
    | otherwise               = do
        putStrLn $ "Spielende:"
        putStrLn $ "Du hast " ++ show (head score) ++ " Augen."
        forM_ [1..3] (\n -> putStrLn $ (playerNames !! n) ++ " hat " ++ show (score !! n) ++ " Augen.")
        putStrLn $ "Es blieben " ++ (show $ fst payment) ++ " Spieler Jungfrau."
        
        mapM_ (putStrLn . payget) $ sort $ zip (snd <$> orderedScore) (snd payment)
      where
        orderedScore = sort $ zip score [0::Int ..]

        payget (   0, py) | py < 0     = "Du zahlst " ++ show (-py) ++ "."
                          | py > 0     = "Du bekommst " ++ show py ++ "."
        payget (plno, py) | py < 0     = playerNames !! plno ++ " zahlt "   ++ show (-py) ++ "."
                          | py > 0     = playerNames !! plno ++ " bekommt " ++ show   py ++ "."
        payget _                       = ""
        payment :: (Int, [Int]) -- virgins, payment
        payment      =  let vs = (fst $== 0) <$> orderedScore -- vs (Virgins) when trick == 0
                            vc = length $ filter (== True) vs
                            compare_fst (fst -> a) (fst -> b) = compare a b
                        in (vc,) $ (2^vc *) <$> case zipBetweenWith compare_fst orderedScore of
                            [LT, LT, LT] -> [3, 1, -1, -3]
                            [LT, LT, EQ] -> [3, 1, -2, -2]
                            [LT, EQ, LT] -> [3, 0,  0, -3]
                            [EQ, LT, LT] -> [2, 2, -1, -3]
                            [LT, EQ, EQ] -> [3, 1, -1, -1]
                            [EQ, LT, EQ] -> [2, 2, -2, -2]
                            [EQ, EQ, LT] -> [1, 1,  1, -3]
                            [EQ, EQ, EQ] -> [0, 0,  0,  0]
                            _            -> error "This shall not occur. The list is not 3 long or not ordered."
    
mainFinish state @ GameState
  {
    game   = Bettel,
    player = Just p,
    
    playerNames,
    takenTr = (elemIndex 8 -> bettel_sieger)
  } | Just n <- bettel_sieger, n == p   = do
        putStrLn $ show (playerNames !! p) ++ " hat das Bettel gewonnen."
        mainFinishSolo True  0 0 state
    | otherwise                         = do
        putStrLn $ show (playerNames !! p) ++ " hat das Bettel leider verloren."
        mainFinishSolo False 0 0 state
    
mainFinish state @ GameState
  {
    game,
    player = Just p,
    
    playerNames,
    score
  } = do
    putStrLn $
        (if p == 0 then "Du hast " else playerNames !! p ++ " hat ") ++
        show game ++
        (if | schwarz /= 0 -> " schwarz " | schneider /= 0 -> " mit Schneider " | otherwise -> "") ++
        (if playerWon then " gewonnen." else " verloren.")
    mainFinishSolo playerWon schneider schwarz state
  where
    playerScore = score !! p
    contraScore = 120 - playerScore
    
    playerWon   = playerScore > 60
    schneider, schwarz :: Int
    schneider   = if playerScore <= 30 || contraScore <  30 then 5 else 0
    schwarz     = if playerScore ==  0 || contraScore ==  0 then 5 else 0

mainFinish _ = error "Kein Spieler, aber kein Ramsch? Das kann nicht sein."

mainFinishSolo True schneider schwarz GameState
  {
    player = Just p,
    playerNames
  } = do
    putStrLn $ (if p == 0 then "Du bekommst " else playerNames !! p ++ " bekommt ") ++ show (3 * spielWert) ++ "."
    forM_ contraParty $ \i -> putStrLn $ (if i == 0 then "Du zahlst " else playerNames !! i ++ " zahlt ") ++ show spielWert ++ "."
    return ()
  where
    spielWert = 5 + schneider + schwarz
    contraParty = [0..3] \\ [p]
    
mainFinishSolo False schneider schwarz GameState
  {
    player = Just p,
    playerNames
  } = do
    putStrLn $ (if p == 0 then "Du zahlst " else playerNames !! p ++ " zahlt ") ++ show (3 * spielWert) ++ "."
    forM_ contraParty $ \i -> putStrLn $ (if i == 0 then "Du bekommst " else playerNames !! i ++ " bekommt ") ++ show spielWert ++ "."
    return ()
  where
    spielWert = 2 + schneider + schwarz
    contraParty = [0..3] \\ [p]

mainFinishSolo _ _ _ _ = error "Solo without player..."