module Cards.Parse (readCard, readSuit, parseCard) where

import Cards

import Data.List

-- | Tries to parse a Card from a 2 letter notation; Nothing is returned when input is invalid.
parseCard :: [Char] -> [Char] -> [Char] -> Maybe Card
parseCard suitChars rankChars [s,r]
                | Just st <- elemIndex s suitChars,
                  Just rk <- elemIndex r rankChars
                   = Just $ Card (toEnum st) (toEnum rk)
parseCard  _ _ _   = Nothing

-- | Reads a Card from user input in 2 letter notation (xY) where x is the Suit and Y is the rank.
-- | The user will be asked to repeat until the input is valid.
readCard :: [Char] -> [Char] -> String -> IO Card
readCard suitChars rankChars message = do
    putStrLn message
    leseRec where
    leseRec = do
        str <- getLine
        case parseCard suitChars rankChars str of
            Just k  -> (putStrLn $ "Karte: " ++ show k) >> return k
            Nothing ->  putStrLn "Zu dieser Abkürzung gibt es keine Karte. Bitte erneut eingeben." >> leseRec

readSuit :: String -> IO Suit
readSuit message = do
    putStrLn message
    leseRec where
    leseRec = do
        str <- getLine
        case parseSuit str of
            Just s  -> (putStrLn $ "Farbe: " ++ show s) >> return s
            Nothing ->  putStrLn "Zu dieser Abkürzung gibt es keine Farbe. Bitte erneut eingeben." >> leseRec
        where
            parseSuit :: [Char] -> Maybe Suit
            parseSuit [s] | Just suit <- elemIndex s "sghe"   = Just . toEnum $ suit
            parseSuit  _                                      = Nothing
