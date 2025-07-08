# Sudoku solver documentation

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
