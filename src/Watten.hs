module Watten where

import Cards.Shuffle
import Utility

main_Watten = do
    hands <- getCards 1 4 5
    print hands