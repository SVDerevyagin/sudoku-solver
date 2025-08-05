module Main (main) where

import Brick (defaultMain)
import System.Random (newStdGen)

import TUI
import Sudoku

main :: IO ()
main = do
  gen <- newStdGen
  let (board, g) = generateEasyBoard gen
      game = State board g (0,0) False [] Nothing
  _ <- defaultMain sudokuApp game
  return ()
