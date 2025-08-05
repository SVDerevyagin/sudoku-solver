
{-|
Module: Sudoku.Generator
Description: Exports functions for generating a Sudoku puzzle

Exports functions for generating a Sudoku puzzle
-}

module Sudoku.Generator
  ( generateEasyBoard
  , generateMediumBoard
  , generateHardBoard
  , randomBoard, removeNCells
  ) where

import Sudoku.Types (Board, Cell (..), fillCell, isCellEmpty, isCellNotEmpty, deleteCell, findIndex, findIndices, mkBoard)
import Sudoku.Utils (shuffle, possibleValues, emptyCellsAmount)
import Sudoku.Solver (solvable)
import System.Random (RandomGen(..), Random(..))


-- | Empty board, generating a new puzzle starts from filling up this
emptyBoard :: Board
emptyBoard = case eb of
  Right b -> b
  Left s  -> error s
  where
    eb = mkBoard $ replicate 9 $ replicate 9 EmptyCell

-- | Generates a randomly filled board
randomBoard :: RandomGen gen => gen -> (Board, gen)
randomBoard gen = case bs of
  []    -> randomBoard g
  (x:_) -> (x, g)
  where
    (bs, g) = fillBoard 81 [emptyBoard] gen

-- | Randomly fills *n* empty cells of a board
fillBoard :: RandomGen gen => Int -> [Board] -> gen -> ([Board], gen)
fillBoard 0 bs gen = (bs, gen)
fillBoard n bs gen = fillBoard (n-1) (concat newB) g2
  where
    (g1, g2) = split gen
    (newB, _) = unzip $ map (`randomFill` g1) bs

-- | Randomly fills the first empty cell. If there are no empty cells, does nothing
randomFill :: RandomGen gen => Board -> gen -> ([Board], gen)
randomFill b gen = case emptyCell of
  Nothing -> ([b], gen)
  Just i  -> let (pv, g1) = shuffle (possibleValues b i) gen
             in (map (fillCell b i) pv, g1)

  where
    emptyCell = findIndex b isCellEmpty

-- | Randomly removes a cell from a fully filled board. The result follows Sudoku rules
removeCell :: RandomGen gen => Board -> gen -> (Maybe Board, gen)
removeCell b gen = go nonEmpty
  where
    (nonEmpty, g1) = shuffle (findIndices b isCellNotEmpty) gen

    go [] = (Nothing, g1)
    go (i:is) = let newB = deleteCell b i
                in if solvable newB then (Just newB, g1)
                   else go is

-- | Removes =n= cells if possible
removeNCells :: RandomGen gen => Int -> Board -> gen -> (Board, gen)
removeNCells n b gen
  | emptyCellsAmount b == n = (b, gen)
  | otherwise = case newB of
      Nothing -> (b, newG)
      Just b' -> removeNCells n b' newG
  where
    (newB, newG) = removeCell b gen

-- | Removes the maximum amount of cells
removeMaxCells :: RandomGen gen => Board -> gen -> (Board, gen)
removeMaxCells b gen = (foldr go b nonEmpty, g1)
  where
    (nonEmpty, g1) = shuffle (findIndices b isCellNotEmpty) gen

    go i board = let newB = deleteCell board i
                 in if solvable newB then newB
                    else board

-- | Generates a Board with at least n hints
generateBoardWithAtLeastNHints :: RandomGen gen => Int -> gen -> (Board, gen)
generateBoardWithAtLeastNHints n gen = removeNCells (81-n) b g1
  where
    (b, g1) = randomBoard gen

-- | Generates an easy puzzle
generateEasyBoard :: RandomGen gen => gen -> (Board, gen)
generateEasyBoard gen = generateBoardWithAtLeastNHints n g
  where
    (n, g) = randomR (36, 45) gen

-- | Generates a medium puzzle
generateMediumBoard :: RandomGen gen => gen -> (Board, gen)
generateMediumBoard gen = generateBoardWithAtLeastNHints n g
  where
    (n, g) = randomR (27, 35) gen

-- | Generates a hard puzzle
generateHardBoard :: RandomGen gen => gen -> (Board, gen)
generateHardBoard gen = removeMaxCells b g -- generateBoardWithAtLeastNHints n g
  where
    (b, g) = randomBoard gen
