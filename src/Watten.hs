module Watten where

import Cards.Shuffle
import Utility

import Control.Applicative

main_Watten :: IO ()
main_Watten = do
    hands <- getCards 1 4 5
    putStrLn $ show $ show <$> hands