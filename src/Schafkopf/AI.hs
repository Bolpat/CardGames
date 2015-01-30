{-# LANGUAGE NamedFieldPuns #-}

module Schafkopf.AI where

import Utility
import Utility.Cond

import Cards

import Trick.Rules

import Schafkopf.Score
import Schafkopf.GameTypes

import Prelude hiding ((||), (&&), not, or, and)
import Control.Applicative
import Data.List
--import Data.Maybe

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
            no,
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

nullState :: GameState
nullState = GameState
    {
        playerNames = ["Du", "Alex", "Bernhard", "Caroline"],
        score       = replicate 4 0,
        takenTr     = replicate 4 0,
        balance     = replicate 4 0
    }

defaultState :: [Hand] -> [Card] -> GameState
defaultState hands trumps = nullState
    {
        mate        = Nothing,
        hands,
        rules       = replicate 4 $ const $ cardAllowed trumps,
        trRule      = normalTR,
        condRS      = const False
    }

gameRule :: GameType -> TrickRule
gameRule  Ramsch            = normalTR
gameRule (Rufspiel      s)  = normalTR
gameRule  Bettel            = bettelTR
gameRule (Habicht (Just s)) = farbHabichtTR s
gameRule (Geier   (Just s)) = farbGeierTR   s
gameRule (Wenz    (Just s)) = farbWenzTR    s
gameRule (Habicht Nothing)  = ([maxR King],  tenUpper)
gameRule (Geier   Nothing)  = ([maxR Over],  tenUpper)
gameRule (Wenz    Nothing)  = ([maxR Under], tenUpper)
gameRule (Solo          s)  = farbsoloTR    s
gameRule  BettelBrett       = bettelTR

trumps :: GameType -> [Card]
trumps  Ramsch              = normalTrumps
trumps (Rufspiel      _)    = normalTrumps
trumps  Bettel              = []
trumps (Habicht (Just s))   = (flip Card King  <$> suits) ++ (Card s <$> ranks)
trumps (Geier   (Just s))   = (flip Card Over  <$> suits) ++ (Card s <$> ranks)
trumps (Wenz    (Just s))   = (flip Card Under <$> suits) ++ (Card s <$> ranks)
trumps (Habicht Nothing)    =  flip Card King  <$> suits
trumps (Geier   Nothing)    =  flip Card Over  <$> suits
trumps (Wenz    Nothing)    =  flip Card Under <$> suits
trumps (Solo          s)    =  officers

overbid :: [Hand] -> GameType -> GameState
overbid = stupidOverbid

stupidOverbid hs Ramsch = (defaultState hs normalTrumps)
    {
        no          = 0,
        player      = Nothing,
        game        = Ramsch
    }

stupidOverbid hands game@(Rufspiel calledSuit) = nullState
    {
        no          = 0,
        player      = Just 0,
        mate        = findIndex (elem $ Card calledSuit Ace) hands,
        hands,
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
        trRule      = gameRule game
    }

type Trick = [Card] -- ! reverse order: last element is first card.
    
play :: Trick -> GameState -> Hand -> IO Card
play = stupidPlay

stupidPlay _ _ h = return $ head h
