{-# LANGUAGE CPP #-}

module Cards.Parse (readHand,
                    readCard,
                    readCardDirect,
                    readCardWith,
                    readCardWithBy,
                    readSuit,
                    readRank)
    where

import Cards
import Utility

import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

-- | How these functions work:
--   They take some suitChars and/or rankChars parameters which are used for 2-letter notation.
--   2-letter notation is very common in card games.

-- | All these functions take a list of suit and/or rank chars. They are intended to be used like this:
--   import qualified Cards.Parse as Parse
--   suitChars = theCharsThatSuitYourGameBest
--   rankChars = theCharsThatSuitYourGameBest
--   parseWhatever = Parse.parseWhatever suitChars rankChars
--   readWhatever  = Parse.readWhatever  suitChars rankChars
-- will generate your specific functions without collisions.
-- | From now on, suitChars and/or rankChars parameters will be ignored in the documentation.

-- DO NOT CHANGE UNDER HERE --------
#define SUIT 0
#define RANK 1
-- YOU MAY CHANGE AFTER HERE -------

#define TWOLETTERFIRST SUIT

-- DO NOT CHANGE UNDER HERE --------
#if TWOLETTERFIRST == SUIT
#define CHARS suitChars rankChars
#define PATTERN [s,r]
#elif TWOLETTERFIRST == RANK
#define CHARS rankChars suitChars
#define PATTERN [r,s]
#else
#error "TWOLETTERFIRST must be specified as 0 (SUIT) or 1 (RANK)!"
#endif

readHand :: [Char] -> [Char] -> String -> IO Hand
readHand CHARS message = do
    putStrLn message
    leseRec where
    leseRec = do
        str <- getLine
        case parseHand CHARS str of
            Just k  -> return k
            Nothing -> putStrLn "Zu einer Abkürzung gibt es keine Karte. Bitte erneut eingeben." >> leseRec

-- | Takes a string whose 'words' (words in Haskell sense) represent 2-letter notated Cards.
parseHand :: [Char] -> [Char] -> String -> Maybe Hand
parseHand CHARS = sequence . parseHand' . words where parseHand' = map $ parseCard CHARS

parseCard :: [Char] -> [Char] -> String -> Maybe Card
parseCard CHARS PATTERN
            | Just st <- elemIndex s suitChars,
              Just rk <- elemIndex r rankChars
                = Just $ Card (toEnum st) (toEnum rk)
parseCard _ _ _ = Nothing

-- | Reads a Card from user input in 2-letter notation (xY) where x is the Suit and Y is the rank.
-- | The user will be asked to repeat until the input is valid.
readCard :: [Char] -> [Char] -> String -> IO Card
readCard CHARS msg = fst <$> readCardDirect CHARS id id msg
{-
readCard CHARS message = do
    putStrLn message
    leseRec where
    leseRec = do
        str <- getLine
        case parseCard CHARS str of
            Just k  -> putStrLn ("Karte: " ++ show k) >> return k
            Nothing -> putStrLn "Zu dieser Abkürzung gibt es keine Karte. Bitte erneut eingeben." >> leseRec
-}

-- | Reads some String from the user. The first function generates a 2-letter notation from this String that is interpreted,
--   the second generates some other result from the String. Last is the message.
readCardDirect ::           [Char] -> [Char] -> (String -> String) -> (String -> b) -> String -> IO (Card, b)
readCardDirect CHARS = readCardWithBy CHARS Just

-- | Reads some String from the user that is read by standard read function. The rest is like readCardDirect.
readCardWith   :: Read a => [Char] -> [Char] -> (a      -> String) -> (a      -> b) -> String -> IO (Card, b)
readCardWith CHARS   = readCardWithBy CHARS maybeRead

-- | Reads some String from the user. The first function tries to get some value from it.
--   The second uses this value to generate 2-letter notaton, the third uses it to get a result.
--   If one of getting the value or interpreting the 2-letter notation fails, the function repeats aksing the user for input.
readCardWithBy :: [Char] -> [Char] -> (String -> Maybe a) -> (a -> String) -> (a -> b) -> String -> IO (Card, b)
readCardWithBy    CHARS               ownMaybeRead           to2Letter        getRes      message = do
    putStrLn message
    leseRec
  where
    leseRec = do
        str <- getLine
        case interpret str of
            Just r  -> return r
            Nothing -> failMsg >> leseRec
    interpret str' = do
        a <- ownMaybeRead str'
        c <- parseCard CHARS $ to2Letter a
        return (c, getRes a)
    failMsg = putStrLn "Diese Eingabe ist ungültig, sie stellt evtl. keine Karte dar. Bitte erneut eingeben." >> leseRec

readSuit :: [Char] -> String -> IO Suit
readSuit suitChars message = do
    putStrLn message
    leseRec where
    leseRec = do
        str <- getLine
        case parseSuit str of
            Just s  -> putStrLn ("Farbe: " ++ show s) >> return s
            Nothing -> putStrLn "Zu dieser Abkürzung gibt es keine Farbe. Bitte erneut eingeben." >> leseRec
      where
        parseSuit [s] | Just st <- elemIndex s suitChars   = Just . toEnum $ st
        parseSuit  _                                       = Nothing

readRank :: [Char] -> String -> IO Rank
readRank rankChars message = do
    putStrLn message
    leseRec where
    leseRec = do
        str <- getLine
        case parseRank str of
            Just r  -> putStrLn ("Rang: " ++ show r) >> return r
            Nothing -> putStrLn "Zu dieser Abkürzung gibt es keine Farbe. Bitte erneut eingeben." >> leseRec
      where
        parseRank [r] | Just st <- elemIndex r rankChars   = Just . toEnum $ st
        parseRank  _                                       = Nothing