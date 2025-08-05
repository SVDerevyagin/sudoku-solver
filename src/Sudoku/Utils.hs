{-# LANGUAGE DataKinds #-}

{-|
Module: Sudoku.Utils
Description: Utility functions for testing and solving

Utility functions for testing and solving
-}
module Sudoku.Utils
  ( printBoard
  , sudokuRules
  , possibleValues
  , emptyCellsAmount
  , randomElement, shuffle
  ) where

import Data.Finite (Finite)
import Data.List (intercalate, sort, (\\))
import Data.Maybe (catMaybes, mapMaybe)
import System.Console.ANSI (Color(..), SGR(..), ConsoleLayer(..), ColorIntensity (..), setSGRCode)

import Sudoku.Types ( Board, Row, Col, Block, Cell(..), CellValue
                    , toDigit, rowCells, blockCells, colCells, getCellValue, isCellEmpty, getCellValue, findIndices)
import System.Random (RandomGen(..), Random (randomR))

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
rowIsCorrect b r = isListUnique $ map getCellValue $ catEmpties $ rowCells b r

-- | Checks if all rows are correct
rowsAreCorrect :: Board -> Bool
rowsAreCorrect = checkAll rowIsCorrect

-- | Checks if a column is correct
colIsCorrect :: Board -> Col -> Bool
colIsCorrect b r = isListUnique $ map getCellValue $ catEmpties $ colCells b r

-- | Checks if all columns are correct
colsAreCorrect :: Board -> Bool
colsAreCorrect = checkAll colIsCorrect

-- | Checks if a block is correct
blockIsCorrect :: Board -> Block -> Bool
blockIsCorrect b r = isListUnique $ map getCellValue $ catEmpties $ blockCells b r

-- | Checks if all blocks are correct
blocksAreCorrect :: Board -> Bool
blocksAreCorrect = checkAll blockIsCorrect

-- | Checks if sudoku rules are satisfied
sudokuRules :: Board -> Bool
sudokuRules b = rowsAreCorrect b
             && colsAreCorrect b
             && blocksAreCorrect b


-- | Returns all possible values that could be placed at ('Row', 'Cell')
possibleValues :: Board -> (Row, Col) -> [CellValue]
possibleValues b (r,c) = [0..8] \\ used
  where
    row = rowCells b r
    col = colCells b c
    block = blockCells b (3*(r`div`3) + c`div`3)
    used = mapMaybe getCellValue $ row ++ col ++ block

-- | Returns the amount of empty cells
emptyCellsAmount :: Board -> Int
emptyCellsAmount = length . (`findIndices` isCellEmpty)


-- | Returns a random element from a list
randomElement :: RandomGen gen => [a] -> gen -> (Maybe a, gen)
randomElement [] g = (Nothing, g)
randomElement xs g = (Just $ xs !! i, g')
  where
    n = length xs
    (i, g') = randomR (0, n-1) g

-- | Shuffles a list
shuffle :: RandomGen gen => [a] -> gen -> ([a], gen)
shuffle []  gen = ([], gen)
shuffle [x] gen = ([x], gen)
shuffle xs  gen = (v:zs, g2)
  where
    (i, g1) = randomR (0, length xs-1) gen
    v  = xs !! i
    ys = take i xs ++ drop (i+1) xs
    (zs, g2) = shuffle ys g1

