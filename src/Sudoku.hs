
module Sudoku
  ( module Sudoku.Types
  , module Sudoku.Utils
  -- * Testing
  , testBoard
  , hardBoard
  , impossibleBoard
  ) where

import Sudoku.Solver
import Sudoku.Types
import Sudoku.Utils


-- | Unwraps a board created by 'mkBoard'
unwrapBoard :: Either String Board -> Board
unwrapBoard (Right b)  = b
unwrapBoard (Left err) = error err

-- | Test board
testBoard :: Board
testBoard = unwrapBoard
          $ mkBoard
          [ [Given 0,   EmptyCell, Given 6,     EmptyCell, Given 5,   Given 3,     EmptyCell, EmptyCell, Given 7  ]
          , [Given 4,   EmptyCell, EmptyCell,   Given 7,   Given 6,   Given 2,     EmptyCell, Given 5,   Given 0  ]
          , [Given 7,   Given 3,   EmptyCell,   EmptyCell, Given 0,   EmptyCell,   EmptyCell, Given 1,   Given 2  ]
          ----
          , [Given 1,   EmptyCell, EmptyCell,   EmptyCell, EmptyCell, Given 4,     Given 7,   Given 0,   EmptyCell]
          , [Given 3,   Given 6,   Given 4,     EmptyCell, EmptyCell, EmptyCell,   Given 1,   EmptyCell, EmptyCell]
          , [EmptyCell, EmptyCell, EmptyCell,   Given 3,   Given 2,   EmptyCell,   Given 5,   EmptyCell, EmptyCell]
          ----
          , [EmptyCell, Given 1,   Given 8,     EmptyCell, EmptyCell, Given 7,     EmptyCell, Given 4,   Given 6  ]
          , [EmptyCell, EmptyCell, EmptyCell,   Given 8,   EmptyCell, Given 0,     Given 2,   EmptyCell, EmptyCell]
          , [EmptyCell, Given 0,   Given 3,     Given 5,   EmptyCell, EmptyCell,   EmptyCell, EmptyCell, EmptyCell]
          ]

-- | Test hard board
hardBoard :: Board
hardBoard = unwrapBoard
          $ mkBoard
          [ [EmptyCell, Given 2,   EmptyCell,   Given 7,   EmptyCell, Given 6,     EmptyCell, EmptyCell, Given 4  ]
          , [EmptyCell, EmptyCell, EmptyCell,   EmptyCell, EmptyCell, Given 4,     EmptyCell, EmptyCell, Given 2  ]
          , [EmptyCell, EmptyCell, EmptyCell,   Given 5,   EmptyCell, EmptyCell,   Given 0,   EmptyCell, EmptyCell]
          ----
          , [Given 5,   EmptyCell, EmptyCell,   Given 3,   EmptyCell, EmptyCell,   Given 1,   EmptyCell, EmptyCell]
          , [Given 1,   EmptyCell, EmptyCell,   EmptyCell, EmptyCell, EmptyCell,   Given 3,   Given 7,   Given 8  ]
          , [EmptyCell, Given 7,   EmptyCell,   EmptyCell, EmptyCell, EmptyCell,   EmptyCell, Given 2,   EmptyCell]
          ----
          , [EmptyCell, EmptyCell, Given 1,     Given 6,   EmptyCell, EmptyCell,   EmptyCell, EmptyCell, EmptyCell]
          , [EmptyCell, EmptyCell, EmptyCell,   EmptyCell, EmptyCell, Given 5,     EmptyCell, EmptyCell, EmptyCell]
          , [EmptyCell, Given 8,   Given 6,     EmptyCell, EmptyCell, EmptyCell,   EmptyCell, Given 3,   Given 1  ]
          ]


-- | Impossible to solve board
impossibleBoard :: Board
impossibleBoard = unwrapBoard
                $ mkBoard
                [ [EmptyCell, Given 1,   Given 2,   Given 3,   Given 4,   Given 5,   Given 6,   Given 7,   Given 8  ]
                , [Given 0,   EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell]
                , [EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell]
                ----
                , [EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell]
                , [EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell]
                , [EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell]
                ----
                , [EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell]
                , [EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell]
                , [EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell, EmptyCell]
                ]
