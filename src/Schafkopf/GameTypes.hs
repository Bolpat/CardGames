{-# LANGUAGE ViewPatterns, MultiWayIf #-}

-- || Implements the Rules for Schafkopf ||--

module Schafkopf.GameTypes
        (
            module Trick.Rules,
            
            PlayRule,
            normalTR, wenzTR, geierTR, habichtTR, bettelTR, alteTreuTR,
            farbsoloTR, farbWenzTR, farbGeierTR, farbHabichtTR,
            cardAllowed, cardAllowedRS, playerRulesRS, mayCallRS,
            GameType(..),
            normalTrumps, officers, alteTreue, soloTrumps,
            trumps, trickRule
        )
    where

import Prelude hiding ((||), (&&), not, or, and)

import Cards
import Trick.Rules
import Utility
import Utility.Cond

import Data.List
import Control.Applicative

-- type TrickRule = ([TrumpRule], DantRule) -- defined in module "Rules"

-- | Some predefined Tricking Rules
normalTR, wenzTR, geierTR, habichtTR, bettelTR, alteTreuTR :: TrickRule

normalTR    = farbsoloTR Hearts
wenzTR      = ([maxR Under],                 tenUpper)
geierTR     = ([maxR Over ],                 tenUpper)
habichtTR   = ([maxR King ],                 tenUpper)
bettelTR    = ([],                           tenLower)
alteTreuTR  = (maxR <$> [King, Over, Under], tenUpper)

-- | Some Functions to create TrickRules from a given Suit
farbsoloTR, farbWenzTR, farbGeierTR, farbHabichtTR :: Suit -> TrickRule

farbsoloTR      s = ([maxR Over, maxR Under, maxS s], tenUpper)
farbWenzTR      s = ([maxR Under,            maxS s], tenUpper)
farbGeierTR     s = ([maxR Over,             maxS s], tenUpper)
farbHabichtTR   s = ([maxR King,             maxS s], tenUpper)

-- | Predefined lists of trumps for the predefined TrickRules.
normalTrumps, officers, alteTreue :: [Card]
soloTrumps :: Suit -> [Card]

officers     = Card <$> suits <*> [Over, Under]
soloTrumps s = officers ++ (Card s <$> ranks)
normalTrumps = soloTrumps Hearts
alteTreue    = (Card <$> suits <*> pure King) ++ officers

-- --------------------------------------------------------------------------------
-- --------------------------------------------------------------------------------

-- | A PlayRule specifies if some card can be played in some context (here context is just the first card of the trick which not necessarily exists).
type PlayRule = Maybe Card -> Card -> Bool

-- | Given the trumps (as list) and the first card of the trick (if there is one), it tests if a player may play a specific card.
-- | This routine works with any game type except those which require full knowledge of the players hand.
--             trumps       first    the card
cardAllowed :: [Card] -> Maybe Card -> Card -> Bool   -- = [Cards] -> PlayRule
cardAllowed     _         Nothing        _    =  True
cardAllowed     ts        (Just f)       c
    | f `elem` ts   =  c `elem` ts
    | c `elem` ts   =  False
    | otherwise     =  suit c == suit f

-- | This is for Rufspiel game type. It is THE exception from the rule above.
-- | Given the called suit, the number of this suited cards at the beginning of the game w/o trumps, the player's hand,
-- | it returns a function with usage like above.
-- | Players who don't hold the called Ace may play with the above rule.
-- | Indeed, giving trumps is obsolete, but there is no improvement when setting them by default.
--              called   num    hand    trumps       first     the card
cardAllowedRS :: Suit -> Int -> Hand -> [Card] -> (Maybe Card -> Card -> Bool) -- = Suit -> Int -> [Card] -> [Card] PlayRule

cardAllowedRS    s      _num    hand    ts           first       card
    | (Card s Ace) `notElem` hand   = cardAllowed ts first card

cardAllowedRS    s       num   _hand    ts          Nothing      card
    | suit card /= s       = True
    | card `elem` ts       = True
    | card == Card s Ace   = True
    | otherwise            = num >= 4

cardAllowedRS    s      _num    hand    ts        (Just first)   card
    | first `elem` ts = if
        | card `elem` ts       -> True
        | card == calledAce    -> False
        | otherwise            -> all (`notElem` ts) hand
    | suit first == s      = card == calledAce
    | card == calledAce    = False
    | all ((`notElem` ts) && suit $/= s) hand = True
    | otherwise            = suit card == suit first
  where calledAce = Card s Ace

-- | Auto-generates rule for each player given the called Suit and the player's hands.
playerRulesRS :: Suit -> [Hand] -> [Hand -> PlayRule]
{-
playerRulesRS s [h1, h2, h3, h4]
    | called `elem` h1   = [rsRule h1, nrmRule,   nrmRule,   nrmRule  ]
    | called `elem` h2   = [nrmRule,   rsRule h2, nrmRule,   nrmRule  ]
    | called `elem` h3   = [nrmRule,   nrmRule,   rsRule h3, nrmRule  ]
    | called `elem` h4   = [nrmRule,   nrmRule,   nrmRule,   rsRule h4]
    | otherwise          = error $ show (Card s Ace) ++ "doesn't appear in any hand!"
    where called        = Card s Ace
          nrmRule       = const $ cardAllowed normalTrumps
          rsRule h hand = cardAllowedRS s (num h) hand normalTrumps
          num           = countBy  $  suit $== s  &&  (`notElem` normalTrumps)
          -- num h    = countBy (\c  ->  suit c == s  &&  c `notElem` normalTrumps) h
-}
playerRulesRS s hs = replicate (length f) nrmRule ++ [rsRule h] ++ replicate (length t) nrmRule where
    (f, h:t)      = splitWhere (called `elem`) hs
    called        = Card s Ace
    nrmRule       = const $ cardAllowed normalTrumps
    rsRule h0 hd = cardAllowedRS s (num h0) hd normalTrumps
    num           = countBy  $  suit $== s  &&  (`notElem` normalTrumps)

-- | Checks if a payer may call the Ace of Acorns, Leaves or Bells.
--           trumps    hand  Acorns Leaves Bells
mayCallRS :: [Card]   -> Hand   ->    [  Bool  ]
mayCallRS    ts   ((\\ ts) -> hand) = mayCall <$> suits where
    -- mayCall :: Suit -> Bool
    -- mayCall s = Card s Ace `notElem` hand  &&  any ((s ==).suit) hand
    mayCall s = notElem (Card s Ace) && any (suit $== s) $ hand

{-
-- | This is for Bettel game type.
--                hand     trumps       first     the card
cardAllowedSB :: [Card] -> [Card] -> (Maybe Card -> Card -> Bool) -- = Suit -> Int -> [Card]
cardAllowedSB     _         _         Nothing       _    = True
cardAllowedSB     h         trumps   (Just first)   c
    | f `elem` trumps   =
        if c `elem` trumps
        then if c > f then True else all (< f) hs
        else disjunct [trumps, hand]
    | any ((suit f ==).suit) (h \\ trumps)   = if
        | c `elem` trumps   -> False
        | suit c == suit f  -> if c > f then True else all ((`elem` trumps) *||* ) h
-}

-- -------------------------------------------------------------------------
-- -------------------------------------------------------------------------

data GameType = Ramsch
              | Rufspiel       Suit
              | Bettel
              | AlteTreu
              | Habicht (Maybe Suit)
              | Geier   (Maybe Suit)
              | Wenz    (Maybe Suit)
              | Solo           Suit
              | BettelBrett
    deriving (Eq)

instance Show GameType where
    show  Ramsch            = "Ramsch"
    
    show (Rufspiel      s ) = "Rufspiel auf " ++ show s
    
    show  Bettel            = "Bettel"
    
    show  AlteTreu          = "Alte Treu"
    show (Habicht Nothing ) = "Habicht"
    show (Habicht (Just s)) = "Farbhabicht auf " ++ show s
    show (Geier   Nothing ) = "Geier"
    show (Geier   (Just s)) = "Farbgeier auf " ++ show s
    show (Wenz    Nothing ) = "Wenz"
    show (Wenz    (Just s)) = "Farbwenz auf " ++ show s
    show (Solo          s ) = "Farbsolo auf " ++ show s
    show  BettelBrett       = "Bettel-Brett"

instance Ord GameType where
    Ramsch              `compare` Ramsch            = EQ
    Ramsch              `compare` _                 = LT
    
    Rufspiel _          `compare` Rufspiel _        = EQ
    Rufspiel _          `compare` _                 = LT
    _                   `compare` Rufspiel _        = GT
    
    Bettel              `compare` Bettel            = EQ
    Bettel              `compare` _                 = LT
    _                   `compare` Bettel            = GT
    
    AlteTreu            `compare` AlteTreu          = EQ
    AlteTreu            `compare` _                 = LT
    _                   `compare` AlteTreu          = GT
    
    Habicht (Just _)    `compare` Habicht (Just _)  = EQ
    Habicht (Just _)    `compare` _                 = LT
    _                   `compare` Habicht (Just _)  = GT
    
    Geier   (Just _)    `compare` Geier   (Just _)  = EQ
    Geier   (Just _)    `compare` _                 = LT
    _                   `compare` Geier   (Just _)  = GT
    
    Wenz    (Just _)    `compare` Wenz    (Just _)  = EQ
    Wenz    (Just _)    `compare` _                 = LT
    _                   `compare` Wenz    (Just _)  = GT
    
    Habicht Nothing     `compare` Habicht Nothing   = EQ
    Habicht Nothing     `compare` _                 = LT
    _                   `compare` Habicht Nothing   = GT
    
    Geier   Nothing     `compare` Geier   Nothing   = EQ
    Geier   Nothing     `compare` _                 = LT
    _                   `compare` Geier   Nothing   = GT
    
    Wenz    Nothing     `compare` Wenz    Nothing   = EQ
    Wenz    Nothing     `compare` _                 = LT
    _                   `compare` Wenz    Nothing   = GT
    
    Solo    _           `compare` Solo    _         = EQ
    Solo    _           `compare` _                 = LT
    _                   `compare` Solo    _         = GT
    
    BettelBrett         `compare` BettelBrett       = EQ
    BettelBrett         `compare` _                 = LT

trickRule :: GameType -> TrickRule
trickRule  Ramsch            = normalTR
trickRule (Rufspiel      _)  = normalTR
trickRule  Bettel            = bettelTR
trickRule  AlteTreu          = alteTreuTR
trickRule (Habicht (Just s)) = farbHabichtTR s
trickRule (Geier   (Just s)) = farbGeierTR   s
trickRule (Wenz    (Just s)) = farbWenzTR    s
trickRule (Habicht Nothing)  = ([maxR King],  tenUpper)
trickRule (Geier   Nothing)  = ([maxR Over],  tenUpper)
trickRule (Wenz    Nothing)  = ([maxR Under], tenUpper)
trickRule (Solo          s)  = farbsoloTR    s
trickRule  BettelBrett       = bettelTR

trumps :: GameType -> [Card]
trumps  Ramsch              = normalTrumps
trumps (Rufspiel      _)    = normalTrumps
trumps  Bettel              = []
trumps  AlteTreu            = alteTreue
trumps (Habicht (Just s))   = (flip Card King  <$> suits) ++ (Card s <$> ranks)
trumps (Geier   (Just s))   = (flip Card Over  <$> suits) ++ (Card s <$> ranks)
trumps (Wenz    (Just s))   = (flip Card Under <$> suits) ++ (Card s <$> ranks)
trumps (Habicht Nothing)    =  flip Card King  <$> suits
trumps (Geier   Nothing)    =  flip Card Over  <$> suits
trumps (Wenz    Nothing)    =  flip Card Under <$> suits
trumps (Solo          s)    = soloTrumps s
trumps BettelBrett          = []