-- || Provides general Rules for any Trick taking Card Game | --

module Trick.Rules where

import Data.List (sortBy)
import Cards

-- | Comparing function by trump criteria. If none of the compared cards is a trump EQ shall be returned.
type TrumpRule = Card -> Card -> Ordering

-- | Comparing function in the case that none of the cards is a trump.
-- | The first argument shall be interpreted as the first card in the trick. The two others shall be compared.
type DantRule  = Card -> Card -> Card -> Ordering
-- | N. B.: 'dant' is common word to indicate a trick made without any trumps - in the card game 'Watten'.
-- |        in the context of Schafkopf this word is not used, but because there is no other, this one will be used.

-- | Combined type for whole rules. The list shall be processed until there is a decision. If none of the TrumpRules fire then the DantRule shall make the decision.
type TrickRule = ([TrumpRule], DantRule)

-- | Sets one specific Card as the highest.
maxC :: Card -> TrumpRule
maxC c c1 c2 | c1 == c, c2 /= c = GT
             | c1 /= c, c2 == c = LT
             | otherwise        = EQ

-- | Sets a list of Cards as the highest (in descending order).
maxL :: [Card] -> TrumpRule
maxL []     _  _                      = EQ
maxL (c:cs) c1 c2 | c1 == c, c2 /= c  = GT
                  | c1 /= c, c2 == c  = LT
                  | otherwise         = maxL cs c1 c2

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

-- | Given a trick rule, it returns which card is the trick taking one, combined with its index.
takesTrick :: TrickRule -> [Card] -> (Card, Int)
takesTrick _ []         = error "No card given."
takesTrick _ (f:[])     = (f, 0)
takesTrick rule (f:cs)  = takesTrick' 1 (f, 0) cs where
    overbids = step rule f
    takesTrick' _ mx []        = mx
    takesTrick' n mx@(mxC, _) (c:cs)
        | c `overbids` mxC   = takesTrick' (n+1) (c, n) cs
        | otherwise          = takesTrick' (n+1)  mx    cs

    -- | Given a TrickRule and the first card in the trick, it will return True if the first card is the trick-taking one else False.
    step :: TrickRule -> Card  ->  Card -> Card -> Bool
    step   ([],   dnt)   fst       c d = case dnt fst c d of
        GT -> True
        LT -> False
        EQ -> error "this case may not occur if the Rules are made properly."
    step  (r:rs, dnt)    fst       c d  | res /= EQ   = (res == GT)
                                        | otherwise   = step (rs, dnt) fst c d
        where res = r c d

-- | Sorts Cards (e. g. on the display) by the given TrickRule.
-- | Not good for trick testing because: here it is a linear order on all cards, but trick testing is NOT a linear order.
sortTR :: TrickRule -> [Card] -> [Card]
sortTR = sortBy . cmpTR where
    cmpTR :: TrickRule -> Card -> Card -> Ordering
    cmpTR ([],   _) (Card s1 r1) (Card s2 r2) | res == EQ   = compare r1 r2
                                              | otherwise   = res   where res = compare s1 s2
    cmpTR (r:_ , _)  c1           c2          | res /= EQ   = res   where res = r c1 c2
    cmpTR (_:rs, d)  c1           c2                        = cmpTR (rs, d) c1 c2
