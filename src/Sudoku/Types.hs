{-# LANGUAGE DataKinds #-}


{-|
Module: Sudoku.Types
Description: Types describing a sudoku puzzle and a few utility functions

Types describing a sudoku puzzle and a few utility functions
-}
module Sudoku.Types
  ( -- * Cell
    CellValue
  , toDigit, fromDigit
  , Cell(..)
  , isCellEmpty, isCellNotEmpty, getCellValue
    -- * Board
  , Row, Col, Block
  , Board(..)
  , mkBoard, sameBoard
  , rowCells, colCells, blockCells
  , getCell, setCell, eraseCell
  ) where

import Data.Array (Array, (!), (//), listArray)
import Data.Finite (Finite, finite, getFinite)


--------------------------------------------------------------------------------
-- * Cell

-- | Cells can have numbers from 1 to 9
type CellValue = Finite 9

-- | Converts CellValue to human digits (1–9)
toDigit :: CellValue -> Int
toDigit = (+1) . fromIntegral . getFinite

-- | Converts human digits (1–9) to CellValue
fromDigit :: Int -> Maybe CellValue
fromDigit n
  | 1 <= n && n <= 9 = Just $ finite $ fromIntegral n-1
  | otherwise        = Nothing

-- | A value in a cell may be given from the beginning or written down
--   by the player
data Cell = EmptyCell            -- ^ The cell is empty
          | Given CellValue      -- ^ The value is given from the beginning
          | Written CellValue    -- ^ The value is written by the player
          deriving (Show, Eq)

instance Ord Cell where
  compare x y = compare (getCellValue x) (getCellValue y)

-- | Checks if a cell is empty
isCellEmpty :: Cell -> Bool
isCellEmpty EmptyCell = True
isCellEmpty _         = False

-- | Checks if a cell is not empty
isCellNotEmpty :: Cell -> Bool
isCellNotEmpty EmptyCell = False
isCellNotEmpty _         = True

-- | Returns the cell’s value
getCellValue :: Cell -> Maybe CellValue
getCellValue EmptyCell = Nothing
getCellValue (Given v) = Just v
getCellValue (Written v) = Just v


--------------------------------------------------------------------------------
-- * Board

-- | Row indices
type Row = Finite 9

-- | Column indices
type Col = Finite 9

-- | Block number
type Block = Finite 9

-- | Sudoku board
newtype Board = Board (Array (Row, Col) Cell)
              deriving (Show, Eq)

-- | Constructor for a board, for testing purposes
mkBoard :: [[Cell]] -> Either String Board
mkBoard rows
  | length rows /= 9           = Left "SudokuBoard.mkBoard: the amount of rows must be 9"
  | any ((/= 9) . length) rows = Left "SudokuBoard.mkBoard: the amount of cells in every row must be 9"
  | otherwise                  = Right $ Board
                                       $ listArray (minBound, maxBound)
                                       $ concat rows

-- | Gets all cells from a row
rowCells :: Board -> Row -> [Cell]
rowCells (Board b) row = [b ! (row, col) | col <- [minBound .. maxBound]]

-- | Gets all cells from a col
colCells :: Board -> Col -> [Cell]
colCells (Board b) col = [b ! (row, col) | row <- [minBound .. maxBound]]

-- | Gets all cells from a 3×3 block
blockCells :: Board -> Block -> [Cell]
blockCells (Board b) block =
  [ b ! (r,c)
  | r <- [baseRow .. baseRow+2]
  , c <- [baseCol .. baseCol+2]
  ]
  where
    baseRow = 3 * (block `div` 3)
    baseCol = 3 * (block `mod` 3)

-- | Gets the value of a cell
getCell :: Board -> Row -> Col -> Maybe CellValue
getCell (Board b) row col = getCellValue $ b ! (row, col)

-- | Updates a cell when the player puts a number in it
setCell :: Board -> (Row, Col) -> CellValue -> Board
setCell (Board b) ind val = case b ! ind of
  Given _ -> Board   b   -- the player can’t rewrite a given cell
  _       -> Board $ b // [(ind, Written val)]

-- | Erases a cell
eraseCell :: Board -> (Row, Col) -> Board
eraseCell (Board b) ind = case b ! ind of
  Written _ -> Board $ b // [(ind, EmptyCell)]
  _         -> Board   b

-- | Checks if two boards are the same
sameBoard :: Board -> Board -> Bool
b1 `sameBoard` b2
  = and [ getCell b1 r c == getCell b2 r c
        | r <- [minBound..maxBound]
        , c <- [minBound..maxBound]
        ]
