import Prelude hiding (or, and)
import Utility.Cond
import Utility.Choice
import Schafkopf
import Watten

import Cards
import Control.Monad
import Control.Applicative

main :: IO ()
main = do
    print ranks
    join $ choiceNum "Was möchtest du spielen?"
        [
            ("Schafkopf", mainSchafkopf),
            ("Watten",    mainWatten   )
        ]

