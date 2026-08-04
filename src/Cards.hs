module Cards where

--
--- < Cards > ---
--

data CardValue = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Enum)

data CardSuite = Clubs | Spades | Diamonds | Hearts
  deriving (Eq, Enum)

data Card = Card
  { rank :: CardValue,
    suit :: CardSuite
  }
  deriving (Eq)

-- OR --
-- data Card = Card CardValue CardSuite deriving(Eq)

instance Show CardValue where
  show :: CardValue -> String
  show cardRank = ["2", "3", "4", "5", "6", "7", "8", "9", "10", "J", "Q", "K", "A"] !! fromEnum cardRank

instance Show CardSuite where
  show :: CardSuite -> String
  show Spades = "♠"
  show Clubs = "♣"
  show Diamonds = "♦"
  show Hearts = "♥"

instance Show Card where
  show :: Card -> String
  show (Card a b) = show a ++ show b

type Deck = [Card]

fullDeck :: Deck -- [Card]
fullDeck = [Card {rank = x, suit = y} | y <- [Clubs .. Hearts], x <- [Two .. Ace]]

smallDeck :: Deck
smallDeck = [Card Ace Spades, Card Two Clubs, Card Jack Hearts]

dealCards :: Int -> Deck -> (Deck, Deck)
dealCards n deck =
  let lastCards = drop n deck
      playCards = take n deck
   in (lastCards, playCards)

countCards :: Deck -> (Int, Deck)
countCards deck = (length deck, deck)

--
--- < Games > ---
--

data GameSettings = GameSettings
  { minBet :: Int,
    maxBet :: Int
  }
  deriving (Show)

data GameState = GameState
  { player :: Player,
    dealer :: Dealer,
    deck :: Deck
  }
  deriving (Show)

--
--- < Players > ---
--

data Player = Player
  { pHand :: Hand,
    pBet :: Int,
    pMoney :: Int
  }

instance Show Player where
  show :: Player -> String
  show (Player hand bet money) =
    unlines ["   Игрок", "Ставка: " <> (show bet), "Деньги: " <> (show money), "Рука: " <> (handToStr hand)]



newtype Dealer = Dealer
  { dHand :: Hand
  }

instance Show Dealer where 
  show :: Dealer -> String
  show (Dealer hand) = "Рука Дилера: " <> handToStr hand

handToStr :: Hand -> String
handToStr hand = mconcat [ cardsStr, " : ", currValueHand, " очков" ]
  where
    cardsStr = (unwords . map show) hand
    currValueHand = show $ handValue hand
--
--- < Game > ---
--

type UpdatePlayer = Player

makeBet :: Int -> Player -> UpdatePlayer
makeBet bet curr@Player {pMoney} = curr {pBet = bet, pMoney = (pMoney - bet)}

payout :: Int -> Player -> UpdatePlayer
payout amount curr@Player {pMoney} = curr {pBet = 0, pMoney = pMoney + amount, pHand = []}

type Hand = [Card]

handValue :: Hand -> Int
handValue hand =
  let baseValue = (sum . map cardValue) hand
      ace's = filter (\Card {rank} -> rank == Ace) hand
   in baseValue `valueWithAce` ace's
  where
    valueWithAce = foldl' choiceValueAce
    choiceValueAce acc _
      | (acc + maxAce) > blackjack = acc + minAce
      | otherwise = acc + maxAce
    (minAce, maxAce, blackjack) = (1, 11, 21)

cardValue :: Card -> Int
cardValue Card {rank} =
  case rank of
    Ace -> 0
    Jack -> 10
    Queen -> 10
    King -> 10
    _ -> fromEnum rank - fromEnum Two + 2
