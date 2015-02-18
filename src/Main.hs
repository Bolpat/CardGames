import Prelude hiding (or, and)
import Schafkopf
import Watten

import Utility.Cond
import Utility.Choice
import Cards

import Text.Printf
import Control.Monad
import Control.Applicative

main :: IO ()
main = join $ choiceNum "Was möchtest du spielen?"
    [
        ("Schafkopf", mainSchafkopf),
        ("Watten",    mainWatten   )
    ]

