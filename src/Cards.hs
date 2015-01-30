{-# LANGUAGE CPP #-}

-- DO NOT CHANGE UNDER HERE ----------------------------------------------------
#define ENG 0
#define FRA 1
#define DEU 2
#define ITA 3

#define RANKS_2_10  0
#define RANKS_2__8  1
#define RANKS_6_10  2
#define RANKS_7_10  3
#define RANKS_9_10  4

#define LOW_10      0
#define HIGH_10     1
-- YOU MAY CHANGE UNDER HERE ---------------------------------------------------

-- || Everything concerning playing cards || --


-- | Available Languages:   ENG (US-English) (default),
--                          DEU (German),
--                          ITA (Italian) is not fully supported,
--
-- | Available Suits:       FRA (French) (default),
--                          DEU (German),
--                          ITA (Italian)
--
-- | Available Ranks:       Please keep in mind that the court cards are derived from the
--                          selected suits. The Ace is not a court card.
--                          RANKS_2_10 (A, 2 .. 10, court cards) (default),
--                          RANKS_6_10 (6 .. 10, court cards, A),
--                          RANKS_7_10 (7 .. 10, court cards, A),
--                          RANKS_9_10 (9, 10, court cards, A),
--                          RANKS_2__8 (A, 2 .. 8, court cards)
--
-- | Available Orderings:   LOW_10  (.. 9, 10, U, O, K, A) (default),
--                          HIGH_10 (.. 9, U, O, K, 10, A)
--  HIGH_10 only works properly when RANKS is RANKS_6_10 or RANKS_7_10 or RANKS_9_10
--  and when SUITS is DEU.
--
-- | Changing the language will only affect Show instance.
--   If you use the returned String, it is not expected that code will break when changing.
-- | Changing SUITS or RANKS will (!!) affect code and very likely break it.
-- | Changing Orderings will (!) affect the (derived) Ord instance, but it is not expected
--   that code will break when changing (you will just have unexpected behavior).

#define LANG        DEU
#define SUITS       DEU
#define RANKS       RANKS_7_10
#define RANKORDER   HIGH_10

-- DO NOT CHANGE UNDER HERE ----------------------------------------------------
                module Cards where

                import Control.Applicative


#if SUITS == DEU
                data Suit = Bells | Hearts | Leaves | Acorns
#if LANG == DEU
                    deriving (Eq, Ord, Enum, Bounded)
                instance Show Suit where
                    show Bells  = "Schellen"
                    show Hearts = "Herz"
                    show Leaves = "Gras"
                    show Acorns = "Eichel"

#elif LANG == ITA
                    deriving (Eq, Ord, Enum, Bounded)
                instance Show Suit where
                    show Bells  = "Campanelli"
                    show Hearts = "Cuori"
                    show Leaves = "Foglie"
                    show Acorns = "Ghiande"
#else
                    deriving (Eq, Ord, Enum, Bounded, Show)
                suitChar = toLower . head . show
#endif
#elif SUITS == ITA
                data Suit = Coins | Cups | Swords | Clubs
#if LANG == DEU
                    deriving (Eq, Ord, Enum, Bounded)
                instance Show Suit where
                    show Coins  = "Münzen"
                    show Cups   = "Kelch"
                    show Swords = "Schwert"
                    show Clubs  = "Prügel"

#elif LANG == ITA
                    deriving (Eq, Ord, Enum, Bounded)
                instance Show Suit where
                    show Coins  = "Denari"
                    show Cups   = "Coppe"
                    show Swords = "Spade"
                    show Clubs  = "Bastoni"

#else
                    deriving (Eq, Ord, Enum, Bounded, Show)
#endif
#else
                data Suit = Diamonds | Hearts | Spades | Clubs
#if LANG == DEU
                    deriving (Eq, Ord, Enum, Bounded)
                instance Show Suit where
                    show Diamonds = "Karo"
                    show Hearts   = "Herz"
                    show Spades   = "Pik"
                    show Clubs    = "Kreuz"
#elif LANG == ITA
                    deriving (Eq, Ord, Enum, Bounded)
                instance Show Suit where
                    show Diamonds = "Quadri"
                    show Hearts   = "Cuori"
                    show Spades   = "Picche"
                    show Clubs    = "Fiori"

#else
                    deriving (Eq, Ord, Enum, Bounded, Show)
#endif
#endif




                data Rank =
#if (RANKS_6_10 <= RANKS) && (RANKS <= RANKS_9_10)
#if RANKS == RANKS_6_10
                            Six   |
#endif

#if RANKS <= RANKS_7_10
                            Seven |
                            Eight |
#endif
                            Nine
#if RANKORDER == HIGH_10
                          | Under
                          | Over
                          | King
                          | Ten
#else
                          | Ten
                          | Under
                          | Over
                          | King
#endif
                          | Ace
#if LANG == DEU
                    deriving (Eq, Ord, Enum, Bounded)
                instance Show Rank where
#if RANKS == RANKS_6_10
                    show Six   = "Sechs"
#endif

#if RANKS <= RANKS_7_10
                    show Seven = "Sieben"
                    show Eight = "Acht"
#endif
                    show Nine  = "Neun"
                    show Ten   = "Zehn"
                    show Under = "Unter"
                    show Over  = "Ober"
                    show King  = "König"
                    show Ace   = "Ass"
#elif LANG == ITA
                    deriving (Eq, Ord, Enum, Bounded)
                instance Show Rank where
#if RANKS == RANKS_6_10
                    show Six   = "Sei"
#endif

#if RANKS <= RANKS_7_10
                    show Seven = "Sette"
                    show Eight = "Otto"
#endif
                    show Nine  = "Nove"
                    show Ten   = "Dieci"
                    show Under = "Inferiore"
                    show Over  = "Superiore"
                    show King  = "Re"
                    show Ace   = "Asso"
#else
                    deriving (Eq, Ord, Enum, Bounded, Show)
#endif
#else
                            Ace
                          | Two
                          | Three
                          | Four
                          | Five
                          | Six
                          | Seven
#if RANKS != RANKS_2__8
                          | Eight
                          | Nine
                          | Ten
                          | Jack
                          | Queen
#else
                          | Knave
                          | Knight
#endif
                          | King
#if LANG == DEU
                    deriving (Eq, Ord, Enum, Bounded)
                instance Show Rank where
                    show Ace   = "Ass"
                    show Two   = "Zwei"
                    show Three = "Drei"
                    show Four  = "Vier"
                    show Five  = "Fünf"
                    show Six   = "Sechs"
                    show Seven = "Sieben"
#if RANKS == RANKS_2__8
                    show Eight = "Acht"
                    show Nine  = "Neun"
                    show Ten   = "Zehn"
                    show Jack  = "Bube"
                    show Queen = "Dame"
#else
                    show Knave = "Soldat"
                    show Knight= "Ritter"
#endif
                    show King  = "König"
#elif LANG == ITA
                    deriving (Eq, Ord, Enum, Bounded)
                instance Show Rank where
                    show Ace   = "Asso"
                    show Two   = "Due"
                    show Three = "Tre"
                    show Four  = "Quattro"
                    show Five  = "Cinque"
                    show Six   = "Sei"
                    show Seven = "Sette"
#if RANKS == RANKS_2__8
                    show Eight = "Otto"
                    show Nine  = "Nove"
                    show Ten   = "Dieci"
                    show Jack  = "Fante"
                    show Queen = "Regina"
#else
                    show Knave = "Fante"
                    show Knight= "Cavallo"
#endif
                    show King  = "Re"
#else
                    deriving (Eq, Ord, Enum, Bounded, Show)
#endif
#endif


                -- | Cards with Suit and Rank
                type Hand = [Card]
                data Card = Card { suit :: Suit,
                                   rank :: Rank }
                    deriving (Eq)
#if LANG == DEU
                instance Show Card where
                    show (Card s r) = show s ++ "-" ++ show r
#elif LANG == ITA
                instance Show Card where
                    show (Card s r) = show r ++ " di " ++ show s
#else
                instance Show Card where
                    show (Card s r) = show r ++ " of " ++ show s
#endif


                -- | The ordered list of Suits
                suits :: [Suit]
                suits = [minBound .. maxBound]

                -- | The ordered list of Ranks
                ranks :: [Rank]
                ranks = [minBound .. maxBound]

                -- | The whole deck
                allCards :: [Card]
                allCards = Card <$> suits <*> ranks
                --allCards = [ Card s r | s <- suits, r <- ranks ]