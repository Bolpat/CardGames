module Watten.GameTypes where

import Cards
import Trick.Rules

import Data.List
import Control.Applicative

kriten :: [Card]
kritenHaube :: Rank -> Suit -> [Card]

kriten          = [Card Hearts King, Card Bells Seven, Card Acorns Seven]
kritenHaube r s = [Card Hearts King, Card Bells Seven, Card Acorns Seven, Card s r]

bidding :: Rank -> Suit -> TrickRule
bidding r s = (
                [
                 maxL $ kritenHaube r s,
                 maxR r,
                 maxS s
                ],
               tenLower
              )

ordRank, ordSuit :: Card -> Card -> Ordering

ordRank c1@(Card _ r1) c2@(Card _ r2) =
    case preOrd c1 c2 of
        Just o  -> o
        Nothing -> compare r1 r2

ordSuit c1@(Card s1 _) c2@(Card s2 _) =
    case preOrd c1 c2 of
        Just o  -> o
        Nothing -> compare s1 s2

preOrd :: Card -> Card -> Maybe Ordering
preOrd c1 c2
    | Just i1 <- mi1, Just i2 <- mi2  = Just $ compare i2 i1
    | Nothing <- mi1, Just _  <- mi2  = Just LT
    | Just _  <- mi1, Nothing <- mi2  = Just GT
    | otherwise                       = Nothing
  where
    mi1 = elemIndex c1 kriten
    mi2 = elemIndex c2 kriten