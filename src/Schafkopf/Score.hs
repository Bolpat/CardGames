module Schafkopf.Score where
import Cards

--import Control.Monad.State

type Score = [Int]

-- | Adds the sum of the Cards' score to the i-th element in the list.
addScCs :: Int -> Score -> [Card] -> Score
addScCs i sc cs = addSc i sc $ trickScore cs

-- | Adds the to the i-th element in the score list the amount of val.
addSc :: Int -> Score -> Int -> Score
addSc i sc val = take i sc  ++  [sc!!i + val]  ++  drop (i+1) sc

trickScore :: [Card] -> Int
trickScore = sum . map cardScore

cardScore :: Card -> Int
cardScore = rankScore . rank

rankScore :: Rank -> Int
rankScore Under =  2
rankScore Over  =  3
rankScore King  =  4
rankScore Ten   = 10
rankScore Ace   = 11
rankScore _     =  0
