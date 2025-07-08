
module Sudoku
  ( module Sudoku.Types
  -- * Testing
  , testBoard
  , hardBoard
  , impossibleBoard
  ) where

import Sudoku.Types


-- | Unwraps a board created by 'mkBoard'
unwrapBoard :: Either String Board -> Board
unwrapBoard (Right b)  = b
unwrapBoard (Left err) = error err

-- | Test board
testBoard :: Board
testBoard = unwrapBoard
          $ mkBoard
          [ [Given 1,   EmptyCell, Given 7,     EmptyCell, Given 6,   Given 4,     EmptyCell, EmptyCell, Given 8  ]
          , [Given 5,   EmptyCell, EmptyCell,   Given 8,   Given 7,   Given 3,     EmptyCell, Given 6,   Given 1  ]
          , [Given 8,   Given 4,   EmptyCell,   EmptyCell, Given 1,   EmptyCell,   EmptyCell, Given 2,   Given 3  ]
          ----
          , [Given 2,   EmptyCell, EmptyCell,   EmptyCell, EmptyCell, Given 5,     Given 8,   Given 1,   EmptyCell]
          , [Given 4,   Given 7,   Given 5,     EmptyCell, EmptyCell, EmptyCell,   Given 2,   EmptyCell, EmptyCell]
          , [EmptyCell, EmptyCell, EmptyCell,   Given 4,   Given 3,   EmptyCell,   Given 6,   EmptyCell, EmptyCell]
          ----
          , [EmptyCell, Given 2,   Given 9,     EmptyCell, EmptyCell, Given 8,     EmptyCell, Given 5,   Given 7  ]
          , [EmptyCell, EmptyCell, EmptyCell,   Given 9,   EmptyCell, Given 1,     Given 3,   EmptyCell, EmptyCell]
          , [EmptyCell, Given 1,   Given 4,     Given 6,   EmptyCell, EmptyCell,   EmptyCell, EmptyCell, EmptyCell]
          ]

-- | Test hard board
hardBoard :: Board
hardBoard = unwrapBoard
          $ mkBoard
          [ [EmptyCell, Given 3,   EmptyCell,   Given 8,   EmptyCell, Given 7,     EmptyCell, EmptyCell, Given 5  ]
          , [EmptyCell, EmptyCell, EmptyCell,   EmptyCell, EmptyCell, Given 5,     EmptyCell, EmptyCell, Given 3  ]
          , [EmptyCell, EmptyCell, EmptyCell,   Given 6,   EmptyCell, EmptyCell,   Given 1,   EmptyCell, EmptyCell]
          ----
          , [Given 6,   EmptyCell, EmptyCell,   Given 4,   EmptyCell, EmptyCell,   Given 2,   EmptyCell, EmptyCell]
          , [Given 2,   EmptyCell, EmptyCell,   EmptyCell, EmptyCell, EmptyCell,   Given 4,   Given 8,   Given 9  ]
          , [EmptyCell, Given 8,   EmptyCell,   EmptyCell, EmptyCell, EmptyCell,   EmptyCell, Given 3,   EmptyCell]
          ----
          , [EmptyCell, EmptyCell, Given 2,     Given 7,   EmptyCell, EmptyCell,   EmptyCell, EmptyCell, EmptyCell]
          , [EmptyCell, EmptyCell, EmptyCell,   EmptyCell, EmptyCell, Given 6,     EmptyCell, EmptyCell, EmptyCell]
          , [EmptyCell, Given 9,   Given 7,     EmptyCell, EmptyCell, EmptyCell,   EmptyCell, Given 4,   Given 2  ]
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
