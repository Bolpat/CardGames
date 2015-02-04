module Watten.Help where

import Prelude hiding (lookup)
import Data.Map

help :: String -> IO ()
help lemma = case lookup lemma lemmata of
    Just l  -> putStrLn l
    Nothing -> putStrLn "Diesem Lemma sind keine Informationen zugeordnet."

lemmata :: Map String String
lemmata = fromList
    [
        ("rules",   helpRules),
        ("playing", helpPlaying)
    ]

helpRules   = "WATTEN\n\
              \Ansagen:\n\
              \Bei zwei, vier fünf oder sechs Spielern sagt Vorhand zuerst den Schlag an, der Geber dann den Trumpf.\n\
              \Bei drei Spielern sagt Vorhand den Schlag und die Farbe an.\
              \Parteien:\n\
              \Zwei Spieler: keine, jeder spielt für sich.\n\
              \Drei Spieler: Vorhand spielt gegen die anderen beiden Spieler.\n\
              \Vier oder sechs Spieler: alle Spieler mit gerader bzw. ungerader Spielernummer bilden eine Partei.\n\
              \Fünf Spieler: Vorhand und Geber spielen gegen die anderen Spieler."

helpPlaying = "WIE MAN SPIELT\n \
              \Einfach den Anweisungen folgen."