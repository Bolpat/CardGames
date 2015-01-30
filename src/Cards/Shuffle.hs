{-# LANGUAGE ViewPatterns #-}

module Cards.Shuffle (getCards) where

import Cards

import System.Random
import Control.Monad
import Control.Applicative

-- | Returns a 4-list of disjunct and distinct 8-lists.
-- | All Cards will be dealt randomly.
-- | dc : Number of decks.
-- | plc: Number of players.
-- | cpp: Cards per player.
getCards :: Int -> Int -> Int -> IO [Hand]
getCards dc plc cpp = do
    let allC = concat $ replicate dc allCards
    let cdc = dc * length allCards -- total number of cards
    when (plc * cpp > cdc) $ error "Not enough cards to play."
    indcs <- getRandIndces cdc
    let mixedCards = iterApp getRemoveIndex indcs allC
    return $ hands plc mixedCards
      where
        hands n cs = if n < 1 then [] else
                        (take cpp cs) : (hands (n-1) (drop cpp cs))


-- | Applies an indexing function to an index list and a arbitrary separation list.
-- | The separated elements will be returned.
iterApp :: (a -> [b] -> (b, [b])) -> [a] -> [b] -> [b]
iterApp _  []           _        = []
iterApp _  _            []       = []
iterApp f (h:t) (f h -> (b, lb)) = b : iterApp f t lb
--iterApp f (h:t) lb = let (b, lb') = f h lb in b : iterApp f t lb'

-- | Generates a list with n random indices.
-- | For the i-th index the possible range is 0 .. n-i where i = 1 .. n
getRandIndces :: Int -> IO [Int]
getRandIndces m | m < 0         = return []
getRandIndces 0                 = return [0]
getRandIndces (subtract 1 -> n) = (:) <$> randomRIO (0, n) <*> getRandIndces n

-- | Returns the indexed element and the given list with the indexed element removed.
getRemoveIndex :: Int -> [a] -> (a, [a])
getRemoveIndex n (splitAt n -> (xh, xt)) = (head xt, xh ++ (tail xt))
