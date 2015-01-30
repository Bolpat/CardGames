import Utility (choiceNum)
import Schafkopf
import Watten

import Control.Monad

main :: IO ()
main = Control.Monad.join $ choiceNum "Was möchtest du spielen?"
    [
        ("Schafkopf", main_Schafkopf),
        ("Watten",    main_Watten   )
    ]
