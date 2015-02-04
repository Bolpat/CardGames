module Cards.Parse (parseHand, parseCard, readCard, readSuit, readRank) where

import Cards

import Data.List
import Control.Applicative
import Control.Monad

parseHand :: [Char] -> [Char] -> String -> Maybe Hand
parseHand suitChars rankChars = sequence . parseHand' . words where
    parseHand' = map $ parseCard suitChars rankChars

parseCard :: [Char] -> [Char] -> String -> Maybe Card
parseCard suitChars rankChars [s,r]
            | Just st <- elemIndex s suitChars,
              Just rk <- elemIndex r rankChars
                = Just $ Card (toEnum st) (toEnum rk)
parseCard _ _ _ = Nothing

-- | Reads a Card from user input in 2 letter notation (xY) where x is the Suit and Y is the rank.
-- | The user will be asked to repeat until the input is valid.
readCard :: [Char] -> [Char] -> String -> IO Card
readCard suitChars rankChars message = do
    putStrLn message
    leseRec where
    leseRec = do
        str <- getLine
        case parseCard suitChars rankChars str of
            Just k  -> putStrLn ("Karte: " ++ show k) >> return k
            Nothing -> putStrLn "Zu dieser Abkürzung gibt es keine Karte. Bitte erneut eingeben." >> leseRec


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