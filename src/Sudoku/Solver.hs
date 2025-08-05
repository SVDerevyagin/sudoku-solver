
{-|
Module: Sudoku.Solver
Description: Defines functions `solve` and `solvable`

The functions that solves a Sudoku puzzle
-}
module Sudoku.Solver
  ( solve, solvable
  ) where

import Sudoku.Types (Board, isCellEmpty, setCell, findIndex)
import Sudoku.Utils (possibleValues, emptyCellsAmount, sudokuRules)


-- | Finds next empty cell and fills it with all possible values
fillNextEmptyCell :: Board -> [Board]
fillNextEmptyCell b = case emptyCell of
  Just i  -> let pv = possibleValues b i
             in map (setCell b i) pv
  Nothing -> []
  where
    emptyCell = findIndex b isCellEmpty

-- | Fills an empty cell of all previous found solutions
solvingIteration :: Int -> [Board] -> [Board]
solvingIteration 0 bs = bs
solvingIteration n bs
  | null bs   = []
  | otherwise = solvingIteration (n-1) $ concatMap fillNextEmptyCell bs


-- | Solves a Sudoku puzzle, returns all possible solutions
solveAll :: Board -> [Board]
solveAll b = filter sudokuRules $ solvingIteration (emptyCellsAmount b) [b]

-- | Solves a Sudoku puzzle
solve :: Board -> Either String Board
solve b = case solveAll b of
  []    -> Left "No solution found"
  [res] -> Right res
  _     -> Left "Too many solutions found"

-- | The puzzle has at least one solution
solvable :: Board -> Bool
solvable b = case solveAll b of
  [_] -> True
  _   -> False

