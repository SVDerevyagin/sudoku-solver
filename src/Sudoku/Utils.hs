{-# LANGUAGE DataKinds #-}

module Sudoku.Utils
  ( printBoard
  , sudokuRules
  ) where

import Data.Finite (Finite)
import Data.List (intercalate, sort)
import Data.Maybe (catMaybes)
import System.Console.ANSI (Color(..), SGR(..), ConsoleLayer(..), ColorIntensity (..), setSGRCode)

import Sudoku.Types (Board, Row, Col, Block, Cell(..), toDigit, rowCells, blockCells, colCells)

-- | Print a 'Board' on screen
printBoard :: Board -> IO ()
printBoard = putStrLn . renderBoard

-- | Creates list of chunks
chunksOf3 :: [a] -> [[a]]
chunksOf3 [] = []
chunksOf3 ls = take 3 ls : chunksOf3 (drop 3 ls)

-- | Returns a human-readable string representing a 'Board' for testing purposes
renderBoard :: Board -> String
renderBoard b = unlines result
  where
    topLine = "╔═══╦═══╦═══╗"
    midLine = "╠═══╬═══╬═══╣"
    btmLine = "╚═══╩═══╩═══╝"
    rows = map (rowCells b) [0..8]
    renderedRows = map renderRow rows
    rowChunks = chunksOf3 renderedRows
    result = [topLine]
          ++ intercalate [midLine] rowChunks
          ++ [btmLine]

-- | Returns a human-readable string representing a row
renderRow :: [Cell] -> String
renderRow cells = concat result
  where
    separator = "║"
    renderedCells = map renderCell cells
    cellChunks = chunksOf3 renderedCells
    result = [separator]
          ++ intercalate [separator] cellChunks
          ++ [separator]

-- | Returns a human-readable string representing a 'Cell'
renderCell :: Cell -> String
renderCell EmptyCell   = " "
renderCell (Given   x) = colorizeString givenCellFg   givenCellBg   $ show $ toDigit x
renderCell (Written x) = colorizeString writtenCellFg writtenCellBg $ show $ toDigit x

-- | Hard-coded colour constants
givenCellFg :: Color
givenCellFg = Cyan
givenCellBg :: Maybe Color
givenCellBg = Nothing
writtenCellFg :: Color
writtenCellFg = White
writtenCellBg :: Maybe Color
writtenCellBg = Nothing

-- | Adds colours to a string
colorizeString :: Color -> Maybe Color -> String -> String
colorizeString fg bg txt
  =  setSGRCode style
  ++ txt
  ++ setSGRCode [Reset]
  where
    style = catMaybes
          [ SetColor Foreground Dull  <$> Just fg
          , SetColor Background Vivid <$> bg
          ]


-- | helper for isListUnique
isListUnique' :: Ord a => [a] -> Bool
isListUnique' [] = True
isListUnique' [_] = True
isListUnique' (x:y:xs) = x /= y && isListUnique (y:xs)

-- | Checks if a list has only unique values
isListUnique :: Ord a => [a] -> Bool
isListUnique = isListUnique' . sort

checkAll :: (Board -> Finite 9 -> Bool) -> Board -> Bool
checkAll f b = all (f b) [0..8]

-- | Delete 'EmptyCell's
catEmpties :: [Cell] -> [Cell]
catEmpties []             = []
catEmpties (EmptyCell:cs) =   catEmpties cs
catEmpties (c        :cs) = c:catEmpties cs

-- | Checks if a row is correct
rowIsCorrect :: Board -> Row -> Bool
rowIsCorrect b r = isListUnique $ catEmpties $ rowCells b r

-- | Checks if all rows are correct
rowsAreCorrect :: Board -> Bool
rowsAreCorrect = checkAll rowIsCorrect

-- | Checks if a column is correct
colIsCorrect :: Board -> Col -> Bool
colIsCorrect b r = isListUnique $ catEmpties $ colCells b r

-- | Checks if all columns are correct
colsAreCorrect :: Board -> Bool
colsAreCorrect = checkAll colIsCorrect

-- | Checks if a block is correct
blockIsCorrect :: Board -> Block -> Bool
blockIsCorrect b r = isListUnique $ catEmpties $ blockCells b r

-- | Checks if all blocks are correct
blocksAreCorrect :: Board -> Bool
blocksAreCorrect = checkAll blockIsCorrect

-- | Checks if sudoku rules are satisfied
sudokuRules :: Board -> Bool
sudokuRules b = rowsAreCorrect b
             && colsAreCorrect b
             && blocksAreCorrect b
