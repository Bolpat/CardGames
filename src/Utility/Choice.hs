{-# LANGUAGE TemplateHaskell #-}

module Utility.Choice where


import Utility.Templates


import Data.Maybe
import Data.Char
import Control.Applicative


choiceYesNo :: String -> c -> c -> IO c
choiceYesNo prompt yes no = do
    putStr prompt
    putStrLn " (j/n)?"
    c <- getLine
    case c of
        "n"   -> return no
        "N"   -> return no
        _     -> return yes

-- | Predefined choice where the keys are ascending numbers.
choiceNum :: String -> [(String, c)] -> IO c
choiceNum p l = choiceBy show
                         (fmap fst . listToMaybe . reads)
                         p $
                         zipWith $(tConc [1,2]) [1::Int ..] l

-- | Uses the first character (toLower!) of the shown value as key. The function will NOT assert that these keys are distinct.
choiceAlpha :: Show c => String -> [(String, c)] -> IO c
choiceAlpha p l = choiceBy return
                           listToMaybe
                           p $
                           zipWith $(tConc [1,2]) (toLower . head . show . snd <$> l) l

-- | Uses standard show and adapted read for choiceBy (not recommended).
choice :: (Eq key, Show key, Read key) =>
    String -> [(key, String, value)] -> IO value
choice = choiceBy show $ fmap fst . listToMaybe . reads

-- | Lets the user make a choice.
--   The first argument is a show function, the second shall try to interpret an input as a key,
--   next follows the prompt to be displayed at first,
--   The least is a list of (key, description, value) tuples.
--   The keys don't necessarily have to be disjunct, but prior keys will be overwritten.
choiceBy :: Eq key => (key -> String) -> (String -> Maybe key) ->
--  Prompt    Selection list
    String -> [(key, String, value)] -> IO value
choiceBy _    _    _      [] = error "Give the user something to choose on."
choiceBy shw rd prompt l  = do
    putStrLn prompt
    printPoss l
    (_, descr, value) <- choiceIter
    putStrLn $ "Du hast " ++ descr ++ " gewählt."
    putStrLn ""
    return value
  where
    -- printPoss :: Show key => [(key, String, value)] -> IO ()
    printPoss = mapM_ $ \(key, descr, _) -> putStrLn $ shw key ++ ": " ++ descr
    
    -- coiceAcc :: Eq key => key -> [(key, descr, value)] -> Maybe (key, descr, value)
    coiceAcc _   []              = Nothing
    coiceAcc inp (h@(a, _, _):t) = case coiceAcc inp t of
        Nothing | inp == a  -> Just h
        r                   -> r
    
    -- choiceIter :: IO (key, String, value) -- Haskell doesn't like the type here.
    choiceIter = do
        chS <- getLine
        case rd chS of
            Nothing -> putStrLn "Eingabe konnte nicht interpretiert werden. Bitte erneut eingeben." >> choiceIter
            Just k  -> case coiceAcc k l of
                Nothing -> putStrLn "Wahl ungültig. Bitte erneut eingeben." >> choiceIter
                Just r  -> return r