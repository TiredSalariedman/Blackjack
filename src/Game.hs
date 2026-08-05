module Game where

import Cards
  ( Dealer (Dealer, dHand),
    GameSettings (..),
    GameState (..),
    Player (..),
    dealCards,
    fullDeck,
    handValue,
    makeBet,
    payout,
  )
import Control.Monad (when)
import Control.Monad.Reader
  ( MonadIO (..),
    MonadReader (ask),
    ReaderT,
  )
import Control.Monad.State.Strict
  ( MonadState (get),
    StateT,
    gets,
    modify',
  )
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import IOFloor (getPlayerBet, shuffleDeck)
import System.Exit (exitSuccess)

type Game = ReaderT GameSettings (StateT GameState IO)

defailtSettings :: GameSettings
defailtSettings = GameSettings {minBet = 10, maxBet = 100}

initialState :: GameState
initialState =
  GameState
    { player = Player {pHand = [], pBet = 0, pMoney = 100},
      dealer = Dealer {dHand = []},
      deck = fullDeck
    }

gameCycle :: Game ()
gameCycle = do
  emptyLine
  printInMT "========== ♠ ♣ ♦ ♥ =========="
  emptyLine
  playerBetAction
  emptyLine

  GameState {deck} <- get
  when (length deck < 15) $ do
    modify'
      ( \gameState ->
          gameState {deck = fullDeck}
      )
    gameCycle

  shuffledDeck <- liftIO $ shuffleDeck deck
  modify'
    ( \gameState ->
        gameState {deck = shuffledDeck}
    )
  dealCardsToPlayer 2
  dealCardsToDealer 1

  emptyLine
  playerAction
  emptyLine

  currDealer <- gets dealer
  modify'
    ( \gameState ->
        gameState {dealer = currDealer {dHand = []}}
    )
  gameCycle

playerBetAction :: Game ()
playerBetAction = do
  GameSettings {minBet, maxBet} <- ask
  bet <- liftIO $ getPlayerBet minBet maxBet
  currPlayer@Player {pMoney} <- gets player
  when (pMoney < minBet) $ do
    printInMT "У вас не достаточно денег для Ставки!"
    liftIO $ exitSuccess

  let updatePlayer = makeBet bet currPlayer

  modify' (\gameState -> gameState {player = updatePlayer})

dealCardsToPlayer :: Int -> Game ()
dealCardsToPlayer nCard = do
  GameState {player, deck} <- get

  let (newDeck, playerDelt) = dealCards nCard deck
  let updatePlayer = player {pHand = player.pHand <> playerDelt}

  modify' (\gameState -> gameState {player = updatePlayer, deck = newDeck})

dealCardsToDealer :: Int -> Game ()
dealCardsToDealer nCard = do
  GameState {dealer, deck} <- get

  let (newDeck, dealerDelt) = dealCards nCard deck
  let updateDealer = dealer {dHand = dealer.dHand <> dealerDelt}

  modify' (\gameState -> gameState {dealer = updateDealer, deck = newDeck})

dealerAction :: Int -> Game ()
dealerAction playerValue = do
  Dealer {dHand} <- gets dealer
  let dealerValue = handValue dHand
  if dealerValue < 17
    then do
      dealCardsToDealer 1

      showDealerHand

      dealerAction playerValue
    else do
      dealerEndGame dealerValue playerValue

dealerEndGame :: Int -> Int -> Game ()
dealerEndGame dealerValue playerValue = do
  showDealerHand

  printInMT $ "У Дилера было " <> T.show dealerValue <> " очка/ов"
  curr@Player {pBet} <- gets player

  let (message, reward) =
        if (dealerValue > 21) || (dealerValue < playerValue)
          then ("Игрок Победил!", pBet * 3)
          else ("Игрок Проиграл :(", 0)

  finishRound message reward curr

playerAction :: Game ()
playerAction = do
  showDealerHand
  showState player
  action <- liftIO $ do
    TIO.putStrLn "Вы хотите (V)зять карту или (P)ропустить раунд?"
    getLine
  case action of
    "V" -> do
      dealCardsToPlayer 1
      hitPl@Player {pBet, pHand} <- gets player
      let newValue = handValue pHand
      showState player

      if newValue > 21
        then finishRound "Перебор!" 0 hitPl
        else
          if newValue == 21
            then finishRound "21! Удвоение выигрыша!" (pBet * 6) hitPl
            else playerAction
    "P" -> do
      showState player
      printInMT "Вы пропустили раунд."
      Player {pHand} <- gets player
      let newValue = handValue pHand
      dealerAction newValue
    _ -> do
      printInMT "ОШИБКА: Ввод должен содержать букву V или P. Пожалуйста, введи команду снова."
      playerAction

finishRound :: T.Text -> Int -> Player -> Game ()
finishRound
  message
  amount
  currPlayer = do
    printInMT message
    let updatePlayer = payout amount currPlayer
    modify' (\gameState -> gameState {player = updatePlayer})

    showState player

showDealerHand :: Game ()
showDealerHand = showState dealer

showState :: (MonadState s m, MonadIO m, Show a) => (s -> a) -> m ()
showState field = do
  currField <- gets field
  emptyLine
  liftIO $ printText currField
  emptyLine
  where
    printText = TIO.putStrLn . T.show

emptyLine :: (MonadState s m, MonadIO m) => m ()
emptyLine = printInMT ""

printInMT :: (MonadIO m) => T.Text -> m ()
printInMT text = liftIO $ TIO.putStrLn text
