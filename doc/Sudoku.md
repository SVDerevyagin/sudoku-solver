# Sudoku solver

## `Sudoku` module

This module just imports all the modules in the library for convenience.
Also I added a few boards for testing purposes.
The impossible board has number 1 in the first row and numbers 2–9 in the first column. Therefore the cell (1,1) can‘t have any numbers in it.

## `Sudoku.Types` module

This project deals with standard sudoku (9×9) only.
Therefore a cell may have only numbers from 1 to 9.
To avoid wrong values in a cell I use `Finite` type from `finite-typelits` package:

```haskell
import Data.Finite (Finite)

type CellValue = Finite 9
```

The only disadvantage is `CellValue` contains values $[0, 8]$, not $[1, 9]$.
To deal with this discrepancy I added functions:

```haskell
toDigit :: CellValue -> Int
fromDigit :: Int -> Maybe CellValue
```

The cell can be in 3 different states: empty, filled from the beginning, filled by the player.

```haskell
data Cell = EmptyCell
          | Given CellValue
          | Written CellValue
```

The standard sudoku has 9 rows, 9 columns, and 9 blocks.
To avoid wrong indices I added this types:

```haskell
type Row = Finite 9
type Col = Finite 9
type Block = Finite 9
```

The numeration:
```
  012 345 678
 ╔═══╦═══╦═══╗
0║   ║   ║   ║
1║ 0 ║ 1 ║ 2 ║
2║   ║   ║   ║
 ╠═══╬═══╬═══╣
3║   ║   ║   ║
4║ 3 ║ 4 ║ 5 ║
5║   ║   ║   ║
 ╠═══╬═══╬═══╣
6║   ║   ║   ║
7║ 6 ║ 7 ║ 8 ║
8║   ║   ║   ║
 ╚═══╩═══╩═══╝
```

The `Board` type is just an `Array` of `Cell`s wrapped in a `newtype`

```haskell
newtype Board = Board (Array (Row, Col) Cell)
```

## `Sudoku.Utils` module

### Printing a `Board`

For testing I need to print the `Board` on screen. The printing function is pretty obvious

```haskell
printBoard :: Board -> IO ()
printBoard = putStrLn . renderBoard
```

Here `renderBoard` is a function that returns a human-readable string representing a `Board`.

```haskell
renderBoard :: Board -> String
```

Rendering a `Board`:

1. Render every row.
2. Split rendered rows into chunks of size 3.
3. Intercalate the row separator between chunks.

Rendering a row:

1. Render every cell.
2. Split rendered cells into chunks of size 3.
3. Intercalate the column separator between chunks.

Rendering a cell: 

1. `show . toDigit`.
2. Use SGR codes (`ansi-terminal` package) to indicate whether the `Cell` is a `Given` or a `Written`.


### Checking correctness

The function that checks if Sudoku rules are satisfied is

```haskell
sudokuRules :: Board -> Bool
```

It just calls three almost identical functions: `rowsAreCorrect`, `colsAreCorrect`, `blocksAreCorrect`.
Each of them uses helper function `checkAll` with `rowIsCorrect`, `colIsCorrect`, `blockIsCorrect`.

Helper function `checkAll` is just a specialized wrapper around standard function `all`.

Each of the functions `_IsCorrect` checks if all non-empty cells have unique values.

### Random

Function 
```haskell
randomElement :: RandomGen gen => [a] -> gen -> (Maybe a, gen)
```
returns or a random element from a list (`Nothing` if the list is empty).

Function
```haskell
shuffle :: RandomGen gen => [a] -> gen -> (Maybe a, gen)
```
randomly changes the order of list elements.

### Miscellaneous

Function
```haskell
findIndex :: Board -> (Cell -> Bool) -> Maybe (Row, Col)
```
finds an element that satisfies a condition.

Function
```haskell
findIndices :: Board -> (Cell -> Bool) -> [(Row, Col)]
```
finds all elements that satisfy a condition.

Function
```haskell
possibleValues :: Board -> (Cell -> Bool) -> [CellValue]
```
returns all allowed in a cell values.

Function
```haskell
possibleValues :: Board -> (Row, Col) -> [CellValue]
```
returns all values allowed in a cell.

Function
```haskell
emptyCellsAmount :: Board -> Int
```
calculates the amount of empty cells in a puzzle.


## `Sudoku.Solver` module

The `solve` algorithm:

1. Find an empty cell
2. Fill the empty cell with every possible values (creates a list of `Board`s)
3. Repeat for all new `Board`s 

The function `solvable` checks if a `Board` has exactly one solution.


## `Sudoku.Generator` module

This module contains functions for generating new puzzles. 
The generating algorithm:

1. Generate a fully filled board.
2. Remove some values from the board.

