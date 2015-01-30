module Watten.GameTypes where

import Cards
import Trick.Rules

import Control.Applicative

bidding :: Rank -> Suit -> TrickRule
bidding r s =
  (
    (maxC <$> [Card Hearts King,
               Card Bells  Seven,
               Card Acorns Seven,
               Card s      r]) ++
    [maxR r, maxS s],
    tenLower
  )

-- | Special DantRule where 10 is ordered between 9 and U.
--   for compatibility reasons... works also when 10 ordered between 9 and U by default.
tenLower :: DantRule
tenLower = dant cmp where
    Ten `cmp` b   | Under <= b && b <= King   = LT
    a   `cmp` Ten | Under <= a && a <= King   = GT
    a   `cmp` b                               = compare a b

