
import Sudoku hiding (shuffle)

import Test.Hspec
import Test.QuickCheck
import Control.Monad (replicateM)
import Data.Array ((//), listArray, elems, array)
import Data.Functor ((<&>))
import System.Random

-- * Newtypes

newtype ValidRow = ValidRow [Cell] deriving Show
newtype RowWithDuplicates = RowWithDuplicates [Cell] deriving Show
newtype FullBoard = FullBoard Board deriving Show

instance Arbitrary ValidRow where
  arbitrary = do
    -- how many non-empty values in the row
    nonEmptyAmount <- choose (0, 9)
    -- unique values in the row
    nonEmptyVals <- shuffle [0..8] -- >>= \vs -> return (take nonEmptyAmount vs)
    -- randomly places for the values
    positions <- shuffle [0..8]
    -- non-empty values can be 'Given' or 'Written'
    constructors <- replicateM nonEmptyAmount arbitrary
    -- creating the row
    let cells = foldr (\(pos, val, useGiven) acc ->
                         acc // [(pos, (if useGiven then Given else Written) val)])
                      (listArray (0,8) $ replicate 9 EmptyCell)
                      (zip3 positions nonEmptyVals constructors)
    return $ ValidRow $ elems cells

instance Arbitrary RowWithDuplicates where
  arbitrary = do
    -- how many non-empty values in the row (at least 2 duplicates)
    nonEmptyAmount <- choose (2, 9)
    -- duplicated number
    dupVal <- choose (0, 8) <&> fromInteger
    -- other values
    otherVals <- shuffle (filter (/= dupVal) [0..8])
    -- randomly places for the values
    positions <- shuffle [0..8]
    -- non-empty values can be 'Given' or 'Written'
    constructors <- replicateM nonEmptyAmount arbitrary
    -- creating the row
    let values = dupVal : dupVal : otherVals
        cells = foldr (\(pos, val, useGiven) acc ->
                         acc // [(pos, (if useGiven then Given else Written) val)])
                      (listArray (0,8) $ replicate 9 EmptyCell)
                      (zip3 positions values constructors)
    return $ RowWithDuplicates $ elems cells

instance Arbitrary FullBoard where
  arbitrary = do
    seed <- arbitrary
    let (b, _) = randomBoard $ mkStdGen seed
    return $ FullBoard b

-- * Helper functions

createBoardWithRow :: [Cell] -> Board
createBoardWithRow cells =
  Board $ array (minBound, maxBound)
    [ ((r,c), if r==0 then cells !! fromIntegral c else EmptyCell)
    | r <- [0..8], c <- [0..8]
    ]

createBoardWithCol :: [Cell] -> Board
createBoardWithCol cells =
  Board $ array (minBound, maxBound)
    [ ((r,c), if c==0 then cells !! fromIntegral r else EmptyCell)
    | r <- [0..8], c <- [0..8]
    ]

createBoardWithBlock :: [Cell] -> Board
createBoardWithBlock cells =
  Board $ array (minBound, maxBound)
    [ ((r,c), if r`div`3 + c`div`3 == 0 then cells !! fromIntegral(3*r+c) else EmptyCell)
    | r <- [0..8], c <- [0..8]
    ]

-- * main

main :: IO ()
main = hspec $ do
  describe "setCell" $ do
    it "changes an empty value" $
      property $ \seed ->
        let (board, _) = generateEasyBoard $ mkStdGen seed
            Just (r,c) = findIndex board isCellEmpty
            newV = 1
            newB = setCell board (r,c) newV
        in getCell newB r c `shouldBe` Just newV

    it "can’t change a 'Given' value" $
      property $ \(FullBoard b) ->
        let Just v = getCell b 0 0
            val = v + 1
            newB = setCell b (0,0) val
        in newB `shouldBe` b

  describe "eraseCell" $ do
    it "erases a 'Written' value" $
      property $ \seed ->
        let (board, _) = generateEasyBoard $ mkStdGen seed
            Right b = solve board
            Just (r,c) = findIndex board isCellEmpty
            newB = eraseCell b (r,c)
        in (getCell newB r c `shouldBe` Nothing) <>
           (newB `shouldNotBe` b)
    it "can’t erase a 'Given' value" $
      property $ \(FullBoard board) ->
        let i@(r,c) = (0,0)
            b = eraseCell board i
        in (getCell b r c `shouldNotBe` Nothing) <>
           (b `shouldBe` board)

  describe "sudokuRules" $ do
    it "returns 'True' for valid rows" $
      property $ \(ValidRow row) ->
        let board = createBoardWithRow row
        in sudokuRules board
    it "returns 'True' for valid columns" $
      property $ \(ValidRow col) ->
        let board = createBoardWithCol col
        in sudokuRules board
    it "returns 'True' for valid blocks" $
      property $ \(ValidRow block) ->
        let board = createBoardWithBlock block
        in sudokuRules board

    it "returns 'False' for rows with duplicate numbers" $
      property $ \(RowWithDuplicates row) ->
        let board = createBoardWithRow row
        in not $ sudokuRules board
    it "returns 'False' for cols with duplicate numbers" $
      property $ \(RowWithDuplicates col) ->
        let board = createBoardWithCol col
        in not $ sudokuRules board
    it "returns 'False' for blocks with duplicate numbers" $
      property $ \(RowWithDuplicates block) ->
        let board = createBoardWithBlock block
        in not $ sudokuRules board

  describe "solve" $ do
    it "fully filled board" $
      forAll arbitrary $ \(FullBoard b) ->
        solve b `shouldBe` Right b

    it "boards with no solution" $
      forAll arbitrary $ \(FullBoard (Board b)) ->
        let b2 = Board $ b // [( (0,0), EmptyCell), ((0,1), EmptyCell)]
            Just c = getCell b2 1 0
            wrongBoard = setCell b2 (0,1) c
        in solve wrongBoard `shouldBe` Left "No solution found"

    it "boards with too many solutions" $
      forAll arbitrary $ \(FullBoard b) ->
        let wrongBoard = foldr (flip deleteCell) b [(r,c) | r<-[0..2], c<-[0..8]]
        in solve wrongBoard `shouldBe` Left "Too many solutions found"

    it "result should be the same as the original full board" $
      forAll arbitrary $ \(FullBoard board) ->
        forAll arbitrary $ \seed ->
          let (puzzle, _) = removeNCells 20 board $ mkStdGen seed
              Right solved = solve puzzle
          in solved `sameBoard` board

  describe "generateEasyBoard" $ do
    it "always has a unique solution" $
      property $ \seed ->
        let (board, _) = generateEasyBoard $ mkStdGen seed
        in length(solve board) `shouldBe` 1

  describe "generateMediumBoard" $ do
    it "always has a unique solution" $
      property $ \seed ->
        let (board, _) = generateMediumBoard $ mkStdGen seed
        in length(solve board) `shouldBe` 1

  describe "generateHardBoard" $ do
    it "always has a unique solution" $
      property $ \seed ->
        let (board, _) = generateHardBoard $ mkStdGen seed
        in length(solve board) `shouldBe` 1
