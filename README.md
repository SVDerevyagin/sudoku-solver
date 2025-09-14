![](https://img.shields.io/badge/Code-Haskell-purple?logo=haskell)

# sudoku-solver

A Haskell-based Sudoku solver and puzzle generator with a terminal interface (TUI).

## Features

- **Type-Safe Core:** Uses `Finite` types from the `finite-typelits` package to
  ensure all row, column, and cell values are valid at compile time, eliminating
  a whole class of runtime errors.
- **Solver Engine:** Implements a backtracking algorithm to find solutions to any
  valid puzzle.
- **Puzzle Generator:** Creates playable puzzles of varying difficulty (Easy,
  Medium, Hard).
- **TUI:** A terminal interface built with the `brick` library, featuring:

    - Value highlighting to help spot all instances of a number.

    - Auto-checking for mistakes.

    - “Finished value” tracking that shows which numbers are fully placed.

- **Correctness Guarantees:** Includes functions to check if a board satisfies all
  Sudoku rules.

## Installation

This project is built with [Stack](https://docs.haskellstack.org/).

1.  **Clone the repository:**
    ```bash
    git clone https://github.com/SVDerevyagin/sudoku-solver.git
    cd sudoku-solver
    ```

2.  **Build and install the executable:**
    ```bash
    stack install
    ```
    This will compile the project and install the `sudoku-solver` binary to your
    Stack's local bin path (typically `~/.local/bin`).

## Usage

### Running the TUI Application

After installation, simply run:
```bash
sudoku-solver
```

### TUI Controls

| Key | Action |
| :-- | :--- |
| **↑↓←→ / wasd / hjkl** | Move the cursor |
| **1-9** | Write the corresponding number in the selected cell |
| **0 / Backspace / Del** | Erase the number in the selected cell (cannot erase `Given` numbers) |
| **E, M, H** | Generate a new **E**asy, **M**edium, or **H**ard puzzle |
| **N** | Get a hint: fills the current cell with the correct value |
| **S** | Solve the entire puzzle instantly |
| **b** | Toggle highlighting for the value in the current cell |
| **q** | Quit the application |

### Using the Library

You can also use the solving and generation algorithms directly in your own Haskell code.

```haskell
import Sudoku

toCell :: Int -> Cell
toCell n = case fromDigit n of
  Just k  -> Given k
  Nothing -> EmptyCell

puzzle :: Either String Board
puzzle = mkBoard
  $ map (map toCell)
  [ [1, 0, 7,   0, 6, 4,   0, 0, 8]
  , [5, 0, 0,   8, 7, 3,   0, 6, 1]
  , [8, 4, 0,   0, 1, 0,   0, 2, 3]

  , [2, 0, 0,   0, 0, 5,   8, 1, 0]
  , [4, 7, 5,   0, 0, 0,   2, 0, 0]
  , [0, 0, 0,   4, 3, 0,   6, 0, 0]

  , [0, 2, 9,   0, 0, 8,   0, 5, 7]
  , [0, 0, 0,   9, 0, 1,   3, 0, 0]
  , [0, 1, 4,   6, 0, 0,   0, 0, 0]
  ]

main :: IO ()
main = do
  case puzzle >>= solve of
    Left err -> putStrLn $ "Error: " ++ err
    Right solution -> do
      putStrLn "Solution:\n"
      printBoard solution
```

The main modules are:
*   `Sudoku.Types`: Core types like `Board`, `Cell`, `Row`, `Col`.
*   `Sudoku.Utils`: Functions for printing, checking correctness, and utilities.
*   `Sudoku.Solver`: The `solve` and `solvable` functions.
*   `Sudoku.Generator`: Functions like `generateEasyBoard`.

For detailed documentation of every module and function, see the included [Sudoku.md](doc/Sudoku.md) file.

## License

This project is licensed under the **BSD-3-Clause License**. See the [LICENSE](LICENSE) file for details.

