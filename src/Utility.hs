{-# LANGUAGE TemplateHaskell #-}

module Utility where

import Utility.Templates

import Data.List
import Data.Maybe
import Data.Char
import Control.Applicative

{-
-- | Tests if a list has a certain length.
-- | Like length l == n, but works with infinite lists too.
hasLength :: Int -> [a] -> Bool
hasLength 0 []    = True
hasLength 0 _     = False
hasLength _ []    = False
hasLength n (_:t) = hasLength (n-1) t

-- | Tests if a list has the given length or is shorter.
-- | Like length l <= n, but works with infinite lists too.
maxLength :: Int -> [a] -> Bool
maxLength _ []    = True
maxLength 0 _     = False
maxLength n (_:t) = maxLength (n-1) t

-- | Tests if a list has at least the given length.
-- | Like length l >= n, but works with infinite lists too.
minLength :: Int -> [a] -> Bool
minLength 0 _  = True
minLength _ [] = False
minLength n (_:t) = minLength (n-1) t
-}

-- | Returns whether the given list elements are distinct.
distinct :: Eq a => [a] -> Bool
distinct = distinctBy (==)

-- | Returns whether the given list elements are distinct, where the programmer may use their own equality check.
distinctBy :: (a -> a -> Bool) -> [a] -> Bool
distinctBy _   []    = True
distinctBy cmp (h:t) = all (not . cmp h) t && distinctBy cmp t

-- | Returns whether the given lists are pairwise disjunct. Each list may be not contain distinct elements.
disjunct :: Eq a => [[a]] -> Bool
disjunct = disjunctBy (==)

-- | Returns whether the given lists are pairwise disjunct, where the programmer may use their own equality check.
disjunctBy :: (a -> a -> Bool) -> [[a]] -> Bool
disjunctBy _   []     = True
disjunctBy cmp (l:ls) = disjAux l ls && disjunctBy cmp ls where
    disjAux []    _    = True
    disjAux (h:t) lsts = all (all $ not . cmp h) lsts && disjAux t lsts

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

-- | Uses standard show and 'read' for choiceBy (not recommended).
choice :: (Eq key, Show key, Read key) =>
    String -> [(key, String, value)] -> IO value
choice = choiceBy show (fmap fst . listToMaybe . reads)

-- | Lets the user make a choice.
--   The first argument is a show function, the second shall try to interpret an input as a key,
--   next follows the prompt to be displayed at first,
--   The least is a list of (key, description, value) tuples.
--   The keys don't necessarily have to be disjunct, but prior keys will be overwritten.
choiceBy :: Eq key => (key -> String) -> (String -> Maybe key) ->
--  Prompt    Selection list
    String -> [(key, String, value)] -> IO value
choiceBy _    _    _      [] = error "Give the user something to choose on."
choiceBy show read prompt l  = do
    putStrLn prompt
    printPoss l
    (_, descr, value) <- choiceIter
    putStrLn $ "Du hast " ++ descr ++ " gewählt."
    return value
    where
        -- printPoss :: Show key => [(key, String, value)] -> IO ()
        printPoss = mapM_ $ \(key, descr, _) -> putStrLn $ show key ++ ": " ++ descr
        
        -- coiceAcc :: Eq key => key -> [(key, descr, value)] -> Maybe (key, descr, value)
        coiceAcc _   []              = Nothing
        coiceAcc inp (h@(a, _, _):t) = case coiceAcc inp t of
            Nothing | inp == a  -> Just h
            r                   -> r
        
        -- choiceIter :: IO (key, String, value) -- Haskell doesn't like the type here.
        choiceIter = do
            chS <- getLine
            case read chS of
                Nothing -> putStrLn "Eingabe konnte nicht interpretiert werden. Bitte erneut eingeben." >> choiceIter
                Just k  -> case coiceAcc k l of
                    Nothing -> putStrLn "Wahl ungültig. Bitte erneut eingeben." >> choiceIter
                    Just r  -> return r

none :: (a -> Bool) -> [a] -> Bool
none c = all $ not . c

-- | e. g. usage: (action) `doUntilM` (condition, conditionFalseAction)
--                (action) `doWhileM` (condition, conditionTrueAction)
--  do (getLine)
--       `doUntilM` ((5 <).length,
--                   putStrLn "input must be at least 5 characters long.")
whileM, untilM :: Monad m => m a -> (a -> Bool, m b) -> m a
action `untilM` cr@(cond, recAct) = do
    a <- action
    if cond a then return a
              else do _ <- recAct; action `untilM` cr
action `whileM` cr@(cond, recAct) = do
    a <- action
    if cond a then do _ <- recAct; action `whileM` cr
              else return a

-- | e. g. usage: (add x mod b) y  ==  (x + y) `mod` b
--                (dim x mod b) y  ==  (x - y) `mod` b
--                (sub x mod b) y  ==  (y - x) `mod` b
add, dim, sub :: Num a => a -> (a -> b -> c) -> b -> a -> c
add x m b y = m (x + y) b -- add
dim x m b y = m (x - y) b -- diminish
sub x m b y = m (y - x) b -- subtract

-- | Counts the elements in a list, which satisfy the predicate.
countBy :: (a -> Bool) -> [a] -> Int
countBy p = foldl' (\n x -> if p x then let n' = n+1 in n' `seq` n' else n) 0

-- | Tests the given condition and returns either a singleton list with the given element, if the condition holds, else the empty list
option :: Bool -> a -> [a]
option p a = if p then [a] else []

-- | Converts a Show-List to a human readable output.
showListNatural :: Show a => [a] -> String
showListNatural []      = ""
showListNatural [a]     = show a
showListNatural [a, b]  = show a ++ " und " ++ show b
showListNatural (a:t)   = show a ++ ", " ++ showListNatural t

-- | uses a unique filter for each element of the list.
zipFilter :: [a -> Bool] -> [a] -> [a]
zipFilter _      []     = []
zipFilter []     l      = l
zipFilter (c:cs) (x:xs) = if c x then x : zipFilter cs xs else zipFilter cs xs

-- | Returns the pairwise function application of neighbored list elements.
zipBetweenWith :: (a -> a -> b) -> [a] -> [b]
zipBetweenWith f l = zipWith f l $ tail l

maybeLast :: [a] -> Maybe a
maybeLast []    = Nothing
maybeLast [a]   = Just a
maybeLast (_:t) = maybeLast t

-- | Similar to foldM with preserved accumulated and usable interim result.
foldUM :: Monad m => (a -> [b] -> m b) -> [a] -> m [b]
foldUM = foldUMAux [] where
    -- foldUMAux :: [b] -> (a -> [b] -> m b) -> [a] -> m [b]
    foldUMAux bs _ []    = return bs
    foldUMAux bs f (h:t) = do
        b <- f h bs
        foldUMAux (b : bs) f t
