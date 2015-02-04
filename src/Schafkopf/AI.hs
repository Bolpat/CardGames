{-# LANGUAGE NamedFieldPuns, ViewPatterns, MultiWayIf #-}

module Schafkopf.AI (GameState(..),
                     trumps,
                     trickRule,
                     
                     play,
                     overbid) where

import Utility
import Utility.Cond

import Cards

import Trick.Rules

import Schafkopf.Score
import Schafkopf.GameTypes

import Prelude hiding ((||), (&&), not, or, and)
import Control.Applicative
import Data.List


data GameState = GameState
    {
        playerNames :: [String],
        no          :: Int,
        player      :: Maybe Int,
        mate        :: Maybe Int,
        game        :: GameType,
        hands       :: [Hand],
        score       :: Score,
        takenTr     :: Score,
        rules       :: [Hand -> PlayRule],
        trRule      :: TrickRule,
        condRS      :: Card -> Bool,
        balance     :: Score
    }
-- no:        index of the player who is about to start (intended to change)
-- player:    index of the "Spieler" party player or Nothing if there is no player party.
-- hands:     the players' cards (4-list) (intended to change)
-- score:     the players' scores (4-tuple) (intended to change)
-- takenTr:   the number of taken tricks.
-- rules:     defines which card the player may pick (4-tuple) (only intended to change in Rufspiel)
-- trRule:    the rule that specifies which card takes the trick (NOT intended to change)
-- condRS:    (cryptic!) is necessary for Rufspiel to have a proper "run away" rule.
--            The predicate (fst) is intended to be "const False",
--            except in Rufspiel, where it is "(`notElem` normalTrumps) && suit $== s" and
--            is applied to the first card of the trick.

instance Show GameState where
    show GameState
        {
            playerNames,
            player,
            mate,
            game,
            hands,
            score,
            takenTr,
            balance
        } =
        "Spiel:      " ++ show game ++ "\n" ++
        "Spieler:    " ++ (case player of Nothing -> "Niemand"; Just n -> playerNames!!n) ++ "\n" ++
        -- "Spieler:    " ++ (if isNothing player then "Nobody" else playerNames !! (fromJust player)) ++ "\n" ++
        "Mitspieler: " ++ (case mate of Nothing -> "Niemand"; Just n -> playerNames!!n) ++ "\n" ++
        (concat $ map playerdata [0..3])
      where
        playerdata n =
            (playerNames !! n) ++ ": \n" ++
            " Hand:         " ++ showListNatural (hands !! n) ++ "\n" ++
            " Score:        " ++ show (score   !! n) ++ "\n" ++
            " Taken Tricks: " ++ show (takenTr !! n) ++ "\n" ++
            " Win/Loss:     " ++ show (balance !! n) ++ "\n"

defaultState :: [Hand] -> [Card] -> GameState
defaultState hands ts = GameState
    {
        playerNames = ["Du", "Alex", "Bernhard", "Caroline"],
        no          = 0,
        game        = Ramsch,
        player      = Just 0,
        mate        = Nothing,
        hands,
        rules       = replicate 4 $ const $ cardAllowed ts,
        trRule      = normalTR,
        condRS      = const False,

        score       = replicate 4 0,
        takenTr     = replicate 4 0,
        balance     = replicate 4 0
    }


overbid :: [Hand] -> GameType -> GameState
overbid = stupidOverbid

stupidOverbid :: [Hand] -> GameType -> GameState
stupidOverbid hs Ramsch = (defaultState hs normalTrumps)
    {
        no          = 0,
        player      = Nothing,
        game        = Ramsch
    }

stupidOverbid hands game@(Rufspiel calledSuit) = (defaultState hands normalTrumps)
    {
        no          = 0,
        player      = Just 0,
        mate        = findIndex (elem $ Card calledSuit Ace) hands,
        game        = game,
        rules       = playerRulesRS calledSuit hands,
        condRS      = flip notElem normalTrumps && suit $== calledSuit
    }

stupidOverbid hs game = (defaultState hs $ trumps game)
    {
        no          = 0,
        player      = Just 0,
        mate        = Nothing,
        game        = game,
        hands       = hs,
        trRule      = trickRule game
    }

type Trick = [Card]

play :: Trick -> Hand -> GameState -> IO Card
play = stupidPlay

stupidPlay :: Trick -> Hand -> GameState -> IO Card
stupidPlay _ h _ = return (head h)

trivialPlay :: Trick -> Hand -> GameState -> IO Card
trivialPlay _     []  _    = error "AI cannot play a card" -- this shall NEVER occur, fault by missing savatorian clause
trivialPlay _     [c] _    = return c
trivialPlay trick av GameState { game }
    | game /= Ramsch       = do
        if trick `hasLength` 3
            then let canOverbid = (== 4) <$> ( snd . (\c -> takesTrick (trickRule game) $ reverse (c : trick)) <$> av ) in
                 if | trickScore trick > 8   -> return $ head $ zipPred canOverbid av
                    | otherwise              -> return $ head av
            else do
                return $ head av
    | otherwise            = return $ head av