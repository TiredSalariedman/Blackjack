module IOFloor where

import System.Random ( newStdGen, Random(randoms) )
import Data.List (sortOn)
import Text.Read ( readMaybe )
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Cards ( Deck )

shuffleDeck :: Deck -> IO Deck
shuffleDeck deck = do
  gen <- newStdGen
  return $ (map snd . shuffle) $ zip (randoms gen :: [Int]) deck
  where
    shuffle = sortOn fst


-- OR --
-- shuffleDeck deck = newStdGen >>= \gen -> pure $ (map snd . shuffle) $ zip (randoms gen :: [Int]) deck
--   where
--     shuffle = sortOn fst

getPlayerBet :: Int -> Int -> IO Int
getPlayerBet minBet maxBet = do
    bet <- do
        TIO.putStrLn $ mconcat ["Введите вашу ставку (мин: ", T.show minBet, ", макс: ", T.show maxBet, "):"]
        readMaybe <$> getLine
    case bet of
        Just b | b >= minBet && b <= maxBet ->
            pure b
        _ -> do
            TIO.putStrLn "ОШИБКА: Ставка должна быть целым числом. Пожалуйста, попробуйте еще раз."
            getPlayerBet minBet maxBet
