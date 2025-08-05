
{-|
Module: TUI
Description: Defines Text User Interface

Defines Text User Interface
-}

module TUI
  ( SudokuState(..)
  , sudokuApp
  ) where


import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Center as B
import qualified Graphics.Vty as V

import System.Random (StdGen)

import Sudoku


-- | Game state
data SudokuState = State { sBoard       :: Board           -- ^ Current puzzle
                         , sRandomGen   :: StdGen          -- ^ Random generator
                         , sSelected    :: (Row, Col)      -- ^ Current cell
                         , sWon         :: Bool            -- ^ Set to 'True' when the puzzle is solved
                         , sFinished    :: [CellValue]     -- ^ List of finished values. A value is finished when every row, column, and block contain exactly one instance of the value
                         , sHighlighted :: Maybe CellValue -- ^ Highlighted value
                         }

type Resource = ()
type SudokuEvent = ()

-- | Application for 'defaultMain'
sudokuApp :: B.App SudokuState SudokuEvent Resource
sudokuApp = B.App
  { B.appDraw         = draw
  , B.appChooseCursor = \_ _ -> Nothing
  , B.appHandleEvent  = handleEvent
  , B.appStartEvent   = startEvent
  , B.appAttrMap      = attributes
  }

-- | Style declarations
styleEmpty, styleGiven, styleWritten, styleWrong, styleSelected, styleHighlighted :: B.AttrName
styleEmpty       = B.attrName "styleEmpty"
styleGiven       = B.attrName "styleGiven"
styleWritten     = B.attrName "styleWritten"
styleWrong       = B.attrName "styleWrong"
styleSelected    = B.attrName "styleSelected"
styleHighlighted = B.attrName "styleHighlighted"

-- | Attribute definitions
attributes :: SudokuState -> B.AttrMap
attributes _ = B.attrMap V.defAttr
  [ (styleEmpty,   V.defAttr)
  , (styleGiven,   B.fg V.cyan)
  , (styleWritten, B.fg V.white)

  , (styleEmpty   <> styleSelected, B.bg V.brightYellow)
  , (styleGiven   <> styleSelected, B.on V.red   V.brightYellow)
  , (styleWritten <> styleSelected, B.on V.black V.brightYellow)

  , (styleGiven   <> styleWrong, B.on V.black V.brightRed)
  , (styleWritten <> styleWrong, B.on V.black V.brightMagenta)

  , (styleGiven   <> styleSelected <> styleWrong, B.on V.black V.brightWhite)
  , (styleWritten <> styleSelected <> styleWrong, B.on V.black V.brightWhite)

  , (styleHighlighted, B.on V.black V.white)
  ]

-- | Border definitions
boldHBorder, boldVBorder, normHBorder, normVBorder :: B.Widget Resource
boldHBorder = B.withBorderStyle B.unicodeBold B.hBorder
boldVBorder = B.withBorderStyle B.unicodeBold B.vBorder
normHBorder = B.withBorderStyle B.unicode B.hBorder
normVBorder = B.withBorderStyle B.unicode B.vBorder

-- | Main interface
draw :: SudokuState -> [B.Widget Resource]
draw state = [ B.center
             $ ( B.vLimit 1 $ B.center $ B.str "Sudoku" ) B.<=>
               ( B.center (drawBoard (sBoard state) (sSelected state) (sHighlighted state)) B.<+>
                 B.center rightPad )
             ]
  where
    rightPad = (B.hCenter $ if sWon state then won else B.emptyWidget) B.<=> B.hCenter (help state)

-- | Draws a board
drawBoard :: Board              -- ^ a 'Board' to draw
          -> (Row, Col)         -- ^ cursor position
          -> Maybe CellValue    -- ^ highlighted value
          -> B.Widget Resource
drawBoard b ind isHighlighted
  = B.joinBorders
  $ B.withBorderStyle B.unicodeBold
  $ B.border
  $ B.vLimit 35
  $ B.hLimit 53
  $ B.vBox
  $ blocks
  where
    block row col = [ [ ((r,c), b # (r,c), isCellRight b (r,c))
                      | c <- [3*col .. 3*col+2]
                      ]
                    | r <- [3*row .. 3*row+2]
                    ]
    blockList = [ [ block 0 0, block 0 1, block 0 2 ]
                , [ block 1 0, block 1 1, block 1 2 ]
                , [ block 2 0, block 2 1, block 2 2 ]
                ]
    blocks :: [B.Widget Resource]
    blocks = [ B.hBox ( [ drawBlock ind isHighlighted (blockList !! r !! c) B.<+> (if c==2 then B.emptyWidget else boldVBorder)
                        | c <- [0..2]
                        ]
                      ) B.<=> (if r==2 then B.emptyWidget else boldHBorder)
             | r <- [0..2]
             ]

-- | Draws a 3×3 block
drawBlock :: (Row, Col)                   -- ^ cursor position
          -> Maybe CellValue              -- ^ highlighted value
          -> [[((Row,Col), Cell, Bool)]]  -- ^ data for drawing cells in the block
          -> B.Widget Resource
drawBlock ind isHighlighted block = B.vLimit 11 $ B.vBox
  [ B.hLimit 17
  $ B.hBox [ drawTile ind isHighlighted (block !! r !! c) B.<+> (if c==2 then B.emptyWidget else normVBorder)
           | c <- [0..2] ] B.<=> (if r==2 then B.emptyWidget else normHBorder)
  | r <- [0..2]
  ]

-- | Draws a tile
drawTile :: (Row, Col)                -- ^ cursor position
         -> Maybe CellValue           -- ^ highlighted value
         -> ((Row,Col), Cell, Bool)   -- ^ data for drawing a cell in the tile
         -> B.Widget Resource
drawTile ind v (i, cell, isCorrect)
  = B.withAttr style
  $ B.padLeftRight 2 $ B.padTopBottom 1
  $ B.str content
  where
    content = case cell of
      EmptyCell -> " "
      Given   x -> show $ toDigit x
      Written x -> show $ toDigit x
    cellStyle = case cell of
      EmptyCell -> styleEmpty
      Given   _ -> styleGiven
      Written _ -> styleWritten
    selectedStyle = if ind == i
      then styleSelected
      else mempty
    correctStyle = if isCorrect
      then mempty
      else styleWrong
    combStyle = cellStyle <> selectedStyle <> correctStyle
    style = case v of
      Nothing -> combStyle
      Just _  -> if getCellValue cell == v
                 then styleHighlighted
                 else combStyle

-- | Help message
help :: SudokuState -> B.Widget Resource
help state = B.borderWithLabel (B.str "help")
           $ B.padLeftRight 1
           $ B.str
           $ unlines
           [ "move:       ↑↓←→ / wasd / hjkl"
           , "answer:     1–9"
           , "erase:      del / backspace / 0"
           , "generate new sudoku:"
           , "  easy:     E"
           , "  medium:   M"
           , "  hard:     H # sometimes can be relatively long"
           , "highlight:  b"
           , "new hint:   N"
           , "solve:      S"
           , "quit:       q"
           , ""
           , "finished values:   " ++ finishedString state
           ]

-- | This shows when the game is finished
won :: B.Widget Resource
won = B.borderWithLabel (B.str "You won!")
    $ B.padLeftRight 1
    $ B.str "Congratulation!"


-- | String containing all finished values
finishedString :: SudokuState -> String
finishedString state = unwords
                     $ map (show . toDigit)
                     $ sFinished state


startEvent :: B.EventM Resource SudokuState ()
startEvent = B.continueWithoutRedraw


-- | Event handling
handleEvent :: B.BrickEvent Resource SudokuEvent
            -> B.EventM Resource SudokuState ()
handleEvent (B.VtyEvent (V.EvKey key [])) =
  case key of
    V.KChar 'q' -> B.halt

    V.KChar '0' -> B.modify handleEraseCell
    V.KBS       -> B.modify handleEraseCell
    V.KDel      -> B.modify handleEraseCell

    V.KChar '1' -> B.modify (handleSetCell 0)
    V.KChar '2' -> B.modify (handleSetCell 1)
    V.KChar '3' -> B.modify (handleSetCell 2)
    V.KChar '4' -> B.modify (handleSetCell 3)
    V.KChar '5' -> B.modify (handleSetCell 4)
    V.KChar '6' -> B.modify (handleSetCell 5)
    V.KChar '7' -> B.modify (handleSetCell 6)
    V.KChar '8' -> B.modify (handleSetCell 7)
    V.KChar '9' -> B.modify (handleSetCell 8)

    V.KUp       -> B.modify $ \(State b g (r,c) w f h) -> State b g (r-1, c) w f h
    V.KChar 'w' -> B.modify $ \(State b g (r,c) w f h) -> State b g (r-1, c) w f h
    V.KChar 'k' -> B.modify $ \(State b g (r,c) w f h) -> State b g (r-1, c) w f h
    V.KDown     -> B.modify $ \(State b g (r,c) w f h) -> State b g (r+1, c) w f h
    V.KChar 's' -> B.modify $ \(State b g (r,c) w f h) -> State b g (r+1, c) w f h
    V.KChar 'j' -> B.modify $ \(State b g (r,c) w f h) -> State b g (r+1, c) w f h
    V.KLeft     -> B.modify $ \(State b g (r,c) w f h) -> State b g (r, c-1) w f h
    V.KChar 'a' -> B.modify $ \(State b g (r,c) w f h) -> State b g (r, c-1) w f h
    V.KChar 'h' -> B.modify $ \(State b g (r,c) w f h) -> State b g (r, c-1) w f h
    V.KRight    -> B.modify $ \(State b g (r,c) w f h) -> State b g (r, c+1) w f h
    V.KChar 'd' -> B.modify $ \(State b g (r,c) w f h) -> State b g (r, c+1) w f h
    V.KChar 'l' -> B.modify $ \(State b g (r,c) w f h) -> State b g (r, c+1) w f h

    V.KChar 'E' -> B.modify $ \(State _ g i _ _ _) -> let (newB, newG) = generateEasyBoard g
                                                      in State newB newG i False [] Nothing
    V.KChar 'M' -> B.modify $ \(State _ g i _ _ _) -> let (newB, newG) = generateMediumBoard g
                                                      in State newB newG i False [] Nothing
    V.KChar 'H' -> B.modify $ \(State _ g i _ _ _) -> let (newB, newG) = generateHardBoard g
                                                      in State newB newG i False [] Nothing

    V.KChar 'N' -> B.modify $ \st@(State b _ (r,c) _ _ _) -> let Right s = solve b
                                                                 Just cell = getCell s (r,c)
                                                             in handleSetCell cell st

    V.KChar 'S' -> B.modify $ \(State b g i _ _ h) -> let Right s = solve $ eraseWrittenCells b
                                                      in State s g i True [minBound .. maxBound] h

    V.KChar 'b' -> B.modify $ changeHighlighted

    _             -> B.continueWithoutRedraw

handleEvent _ = B.continueWithoutRedraw


-- | Checks if a value is finished
checkCellValue :: Board -> CellValue -> Bool
checkCellValue b v = (==9) $ length $ findIndices b ((==Just v) . getCellValue)

-- | Set a new cell value, can’t change a 'Given' value
handleSetCell :: CellValue -> SudokuState -> SudokuState
handleSetCell v (State b g i _ _ h) = State newB g i isWon finished h
  where
    newB = setCell b i v
    isWon = emptyCellsAmount newB == 0 && sudokuRules newB
    finished = filter (checkCellValue newB) [minBound .. maxBound]

-- | Erases a value in the current cell, can’t erase a 'Given' value
handleEraseCell :: SudokuState -> SudokuState
handleEraseCell (State b g i _ _ h) = State newB g i False finished h
  where
    newB = eraseCell b i
    finished = filter (checkCellValue newB) [minBound .. maxBound]

-- | Turns on and off the highlighting of the value in the current cell
changeHighlighted :: SudokuState -> SudokuState
changeHighlighted st@(State b _ (r,c) _ _ h) = case h of
  Just _  -> st{sHighlighted = Nothing}
  Nothing -> st{sHighlighted = getCell b (r,c)}
