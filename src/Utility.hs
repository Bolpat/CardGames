{-# LANGUAGE TemplateHaskell, BangPatterns #-}

module Utility where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Applicative

ow :: Bool
ow = otherwise

-- | Tries to interpret a String as an arbitrary Read type.
maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- | Tests if a list has a certain length.
-- | Like length l == n, but works with infinite lists too.
hasLength :: [a] -> Int -> Bool
[]    `hasLength`   0  = True
_     `hasLength`   0  = False
[]    `hasLength`   _  = False
(_:t) `hasLength` (!n) = t `hasLength` (n-1)

-- | Tests if a list has the given length or is shorter.
-- | Like length l <= n, but works with infinite lists too.
maxLength :: [a] -> Int -> Bool
[]    `maxLength`   _  = True
_     `maxLength`   0  = False
(_:t) `maxLength` (!n) = t `maxLength` (n-1)

-- | Tests if a list has at least the given length.
-- | Like length l >= n, but works with infinite lists too.
minLength :: [a] -> Int -> Bool
_     `minLength`   0  = True
[]    `minLength`   _  = False
(_:t) `minLength` (!n) = t `minLength` (n-1)

isLeft, isRight :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False
isRight         = not . isLeft

-- | Alias for flip elem. Avoids constructions like (`elem` list). inList l x  <-->  x `elem` l
inList :: Eq a => [a] -> a -> Bool
inList = flip elem

-- | Alias for flip notElem. Avoids constructions like (`notInList` list). notInList l x  <-->  x `notInList` l
notInList :: Eq a => [a] -> a -> Bool
notInList = flip notElem

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

-- | Splits a given list like this:
--   the first element of the second list is the first in the given list to comply the predicate.
splitWhere :: (a -> Bool) -> [a] -> ([a], [a])
splitWhere _ []             = ([], [])
splitWhere p l@(h:t) | p h  = ([], l)
                     | ow   = let (s, r) = splitWhere p t in (h:s, r)

between :: Ord a => (a, a) -> a -> Bool
between (l, u) v = l <= v  &&  v <= u

inside :: Ord a => (a, a) -> a -> Bool
inside  (l, u) v = l <  v  &&  v <  u

-- | returns a list of indices, where at any index the element of the given list is a maximal element.
indicesOfMaxima :: Ord a => [a] -> [Int]
indicesOfMaxima = indicesOfMaximaBy compare

-- | returns a list of indices, where at any index the element of the given list is a maximal element, according to the ordering funciton.
indicesOfMaximaBy :: (a -> a -> Ordering) -> [a] -> [Int]
indicesOfMaximaBy _ []        = []
indicesOfMaximaBy cmp (h':t') = indicesOfMaxima' 1 [0] h' t' where
    indicesOfMaxima' _  acc _ []    = reverse acc
    indicesOfMaxima' !n acc m (h:t)
        | c == LT   = indicesOfMaxima' (n + 1) acc     m t
        | c == GT   = indicesOfMaxima' (n + 1) [n]     h t
        | ow        = indicesOfMaxima' (n + 1) (n:acc) m t
      where c  = h `cmp` m
-- | returns the maximum
maxima :: Ord b => (a -> b) -> [a] -> [a]
maxima = maximaBy compare

maximaBy :: (b -> b -> Ordering) -> (a -> b) -> [a] -> [a]
maximaBy c f l = (l !!) <$> (indicesOfMaximaBy c $ f <$> l)

none :: (a -> Bool) -> [a] -> Bool
none c = all $ not . c

-- | e. g. usage: (add x mod b) y  ==  (x + y) `mod` b
--                (dim x mod b) y  ==  (x - y) `mod` b
--                (sub x mod b) y  ==  (y - x) `mod` b
add, dim, sub :: Num a => a -> (a -> b -> c) -> b -> a -> c
eqv :: (Num a, Eq c) => a -> (a -> b -> c) -> b -> a -> Bool
add x m b y = m (x + y) b -- add
dim x m b y = m (x - y) b -- diminish
sub x m b y = m (y - x) b -- subtract
eqv x m b y = m x b == m y b -- eqivalent mod b

plus, minus :: Num a => a -> a -> (a -> b -> c) -> b -> c
plus  x y m b = m (x + y) b
minus x y m b = m (x - y) b

-- | Counts the elements in a list, which satisfy the predicate.
countBy :: (a -> Bool) -> [a] -> Int
countBy p = foldl' (\ !n x -> if p x then n + 1 else n) 0

-- | Sums after mapping a function on the domain list.
--   This is intended to be used with a view function like "(fromEnum . rank)" on a list of Cards.
sumBy :: Num b => (a -> b) -> [a] -> b
sumBy f = sum . map f

-- | Calulates the product after mapping a function on the domain list. This is intended to be used with a view function.
productBy :: Num b => (a -> b) -> [a] -> b
productBy f = product . map f

-- | Tests the given condition and returns either a singleton list with the given element, if the condition holds, else the empty list
option :: Bool -> a -> [a]
option p a = if p then [a] else []

-- | Gets sublist by given indices.
infix 8 !!!
(!!!) :: [a] -> [Int] -> [a]
_ !!! []     = []
l !!! (i:is) = l !! i : l !!! is

-- | Converts a showable list to a human readable output.
showListNatural :: Show a => [a] -> String
showListNatural l = concatNatural $ map show l

-- | Concatinates Strings with commas, spaces and "and", like a written enumeration.
concatNatural :: [String] -> String
concatNatural []      = ""
concatNatural [a]     = a
concatNatural [a, b]  = a ++ " und " ++ b
concatNatural (a:t)   = a ++ ", " ++ concatNatural t

-- | Uses a Bool value to indicate if the corresponding element in the given list is kept.
zipPred :: [Bool] -> [a] -> [a]
zipPred []        _      = []
zipPred _         []     = []
zipPred (True:cs) (x:xs) = x : zipPred cs xs
zipPred (_   :cs) (_:xs) =     zipPred cs xs

-- | Uses a unique filter for each element of the list.
zipFilter :: [a -> Bool] -> [a] -> [a]
zipFilter _      []     = []
zipFilter []     l      = l
zipFilter (c:cs) (x:xs)
    | c x               = x : zipFilter cs xs
    | otherwise         =     zipFilter cs xs

-- | Returns the pairwise function application of neighbored list elements.
zipBetweenWith :: (a -> a -> b) -> [a] -> [b]
zipBetweenWith f [] = []
zipBetweenWith f l  = zipWith f l $ tail l

maybeLast :: [a] -> Maybe a
maybeLast []    = Nothing
maybeLast l     = Just $ last l

-- | e. g. usage: (action) `doUntilM` (condition, conditionFalseAction)
--                (action) `doWhileM` (condition, conditionTrueAction)
--  do
--      ...
--      (getLine)
--          `doUntilM` ((5 <).length, putStrLn "input must be at least 5 characters long.")
--          `doWhileM` (head $==$ last, putStrLn "first and last character have to differ.")
--      ...
whileM, untilM :: Monad m => m a -> (a -> Bool, m b) -> m a
action `untilM` cr@(cond, recAct) = do
    a <- action
    if cond a then return a
              else do _ <- recAct; action `untilM` cr
action `whileM` cr@(cond, recAct) = do
    a <- action
    if cond a then do _ <- recAct; action `whileM` cr
              else return a

-- | Similar to foldM with preserved accumulated and usable interim result.
foldUM :: Monad m => state -> (b -> state -> state) -> (a -> state -> m b) -> [a] -> m state
foldUM state _      _      []    = return state
foldUM state update action (a:t) = do
    b <- action a state
    foldUM (update b state) update action t

foldCM :: Monad m => state -> (state -> state) -> (state -> m state) -> (state -> Bool) -> m state
foldCM start update action cond = do
    state <- action start
    if cond state
        then foldCM (update state) update action cond
        else return state

foldUCM :: Monad m => state -> (b -> state -> state) -> (a -> state -> m b) -> (b -> Bool) -> [a] -> m state
foldUCM state _      _      _    []    = return state
foldUCM state update action cond (a:t) = do
    b <- action a state
    if cond b
        then foldUCM (update b state) update action cond t
        else return state