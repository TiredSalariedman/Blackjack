module Main (main) where

import Control.Monad.Reader ( ReaderT(runReaderT) )
import Control.Monad.State.Strict ( execStateT )
import Game ( gameCycle, defailtSettings, initialState )
import Cards ( GameState )

main :: IO GameState
main = execStateT (runReaderT gameCycle defailtSettings) initialState
