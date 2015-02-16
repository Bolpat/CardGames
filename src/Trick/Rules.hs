-- || Provides general Rules for any Trick taking Card Game | --

module Trick.Rules where

import Data.List (sortBy)
import Cards

-- | Comparing function by trump criteria. If none of the compared cards is a trump EQ will be returned.
type TrumpRule = Card -> Card -> Ordering
-- | N. B.: particularly TrumpRules maybe won't result in EQ, even if the Cards are equal by default Suit-Rank-comparison.
--          e. g. in the game Doppelkopf where two Decks are used and every Card exists twice, for any conflict of two Suit-Rank-equal Cards, the first beats the second,
--          with the optional exception for the Ten of Hearts where the second beats the first.

-- | Comparing function in the case that none of the cards is a trump.
-- | The first argument will be interpreted as the first card in the trick. The two others will be compared.
type DantRule  = Card -> Card -> Card -> Ordering
-- | N. B.: in the card game 'Watten', 'dant' is common word to indicate a trick made without any trumps.
-- |        in the context of other games this word is not used, but because there is no other word, this one will be used.

-- | Combined type for whole rules. The list will be processed until there is a decision. If none of the TrumpRules fire then the DantRule will make the decision.
type TrickRule = ([TrumpRule], DantRule)

-- | Sets one specific Card as the highest.
maxC :: Card -> TrumpRule
maxC = maxCby GT

-- | Sets one specific Card as the highest.
--   The given ordering is used in case of both Cards are the specific Card and shall not be EQ.
maxCby :: Ordering -> Card -> TrumpRule
maxCby o c c1 c2
    | c == c1, c == c2 = o
    | c1 == c, c2 /= c = GT
    | c1 /= c, c2 == c = LT
    | otherwise        = EQ

-- | Sets a list of Cards as the highest (in descending order).
maxL :: [Card] -> TrumpRule
maxL = maxLby GT

-- | Sets a list of Card as the highest (in descending order). It uses maxCBy along to determine the result.
maxLby :: Ordering -> [Card] -> TrumpRule
maxLby _ [] _ _ = EQ
maxLby o (c:cs) c1 c2
    | res == EQ   = maxLby o cs c1 c2
    | otherwise   = res
  where
    res = maxCby o c c1 c2

-- | Sets the given Rank as the highest Cards. Within it will be compared by the canonical order given by Suit.
maxR :: Rank -> TrumpRule
maxR = maxRby compare

-- | Sets the given Rank as the highest Cards. Within it will be compared by the given ordering function for Suits.
maxRby :: (Suit -> Suit -> Ordering) -> Rank -> TrumpRule
maxRby cmp r (Card s1 r1) (Card s2 r2)
    | r == r1, r == r2   = cmp s1 s2
    | r == r1, r /= r2   = GT
    | r /= r1, r == r2   = LT
    | otherwise          = EQ

-- | Sets the given Suit as the highest Cards. Within it will be compared by the canonical order given by Rank.
maxS :: Suit -> TrumpRule
maxS = maxSby compare

-- | Sets the given Suit as the highest Cards. Within it will be compared by the given ordering function for Ranks.
maxSby :: (Rank -> Rank -> Ordering) -> Suit -> TrumpRule
maxSby cmp s (Card s1 r1) (Card s2 r2)
    | s == s1, s == s2   = cmp r1 r2
    | s == s1, s /= s2   = GT
    | s /= s1, s == s2   = LT
    | otherwise          = EQ

-- | Returns a DantRule derived from a function to order Ranks.
dant :: (Rank -> Rank -> Ordering) -> DantRule
dant rangCmp                          (Card s _) (Card s1 r1) (Card s2 r2)
    | s1 == s2  = rangCmp r1 r2
    | s1 == s   = GT
    | s2 == s   = LT
    | otherwise = EQ

-- | DantRule used for nearly any game type.
tenUpper :: DantRule
tenUpper = dant $ if Ten > King then compare else cmp where
    Ten `cmp` b   | Under <= b && b <= King   = GT
    a   `cmp` Ten | Under <= a && a <= King   = LT
    a   `cmp` b                               = compare a b

-- | Special DantRule where 10 is ordered between 9 and U.
tenLower :: DantRule
tenLower = dant $ if Ten < Under then compare else cmp where
    Ten `cmp` b   | Under <= b && b <= King   = LT
    a   `cmp` Ten | Under <= a && a <= King   = GT
    a   `cmp` b                               = compare a b

-- | Given a trick rule, it returns which card is the trick taking one, combined with its index.
takesTrick :: TrickRule -> [Card] -> (Int, Card)
takesTrick _    []     = error "No card given."
takesTrick _    (f:[]) = (0, f)
takesTrick rule (f:cs) = takesTrick' 1 (0, f) cs where
    takesTrick' :: Int -> (Int, Card) -> [Card] -> (Int, Card)
    takesTrick' _ mx []        = mx
    takesTrick' n mx@(_, mxC) (c:cs)
        | c `overbids` mxC   = takesTrick' (n+1) (n, c) cs
        | otherwise          = takesTrick' (n+1)  mx    cs
    overbids = step rule f

-- | Given a TrickRule and the first card in the trick, it will return a function that returns True if the first card is the trick-taking one else False.
step :: TrickRule -> Card  ->  Card -> Card -> Bool
step   ([],   dnt)   f         c d = case dnt f c d of
    GT -> True
    LT -> False
    EQ -> error "this case may not occur if the Rules are made properly (or c and d are identical - even if then it may not occur also)"
step  (r:rs, dnt)    f       c d  | res /= EQ   = (res == GT)
                                  | otherwise   = step (rs, dnt) f c d
    where res = r c d

-- | Sorts Cards (e. g. on the display) by the given TrickRule.
-- | Not good for trick testing because: here it is a linear order on all cards, but trick testing is NOT a linear order.
{-
sortTR :: TrickRule -> [Card] -> [Card]
sortTR = sortBy . cmpTR where
    cmpTR :: TrickRule -> (Card -> Card -> Ordering)
    cmpTR ([],   _) (Card s1 r1) (Card s2 r2) | res == EQ   = compare r1 r2
                                              | otherwise   = res   where res = compare s1 s2
    cmpTR (r:_ , _)  c1           c2          | res /= EQ   = res   where res = r c1 c2
    cmpTR (_:rs, d)  c1           c2                        = cmpTR (rs, d) c1 c2
-}
sortTR :: TrickRule -> [Card] -> [Card]
{-
sortTR (rs, _) = (sortBy . cmpTR) rs where
    cmpTR :: [TrumpRule] -> (Card -> Card -> Ordering)
    cmpTR []      (Card s1 r1) (Card s2 r2) | res == EQ   = compare r1 r2
                                            | otherwise   = res   where res = compare s1 s2
    cmpTR (r: _)  c1           c2           | res /= EQ   = res   where res = r c1 c2
    cmpTR (_:rt)  c1           c2                         = cmpTR rt c1 c2
-}
sortTR tr = sortTRBy tr suit rank

sortTRBy :: (Ord a, Ord b) => TrickRule -> (Card -> a) -> (Card -> b) -> [Card] -> [Card]
sortTRBy (tr, _) f g = (sortBy . cmpTR) tr where
    cmpTR :: [TrumpRule] -> (Card -> Card -> Ordering)
    cmpTR []      c1 c2 | res == EQ   = compareBy g c1 c2
                        | otherwise   = res   where res = compareBy f c1 c2
    cmpTR (r: _)  c1 c2 | res /= EQ   = res   where res = r c1 c2
    cmpTR (_:rt)  c1 c2               = cmpTR rt c1 c2
    compareBy f a b = f a `compare` f b