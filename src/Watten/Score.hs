module Watten.Score where

type Score = [Int]

addSc :: Int -> Int -> Score -> Score
addSc i val sc = take i sc  ++  [sc !! i + val]  ++  drop (i + 1) sc

incSc :: Int -> Score -> Score
incSc i = addSc i 1

addScs :: [Int] -> Int -> Score -> Score
addScs []     _   = id
addScs (i:is) val = addScs is val . addSc i val

incScs :: [Int] -> Score -> Score
incScs is = addScs is 1