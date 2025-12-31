{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MathDoku.Core
  ( -- * Core Types
    Board
  , Cell(..)
  , Coord
  , Difficulty(..)
    
    -- * Smart Constructors
  , emptyBoard
  , mkBoard
  , fromList
  , unBoard
    
    -- * Cell Operations
  , getCell
  , cellValue
  , cellToValue
  , isCellEmpty
    
    -- * Immutable Updates
  , setCell
  , placeValue
    
    -- * Sudoku Units
  , rows
  , cols
  , boxes
    
    -- * Validation
  , isValidMove
  , isComplete
  , isValidBoard
    
    -- * Rendering
  , printBoard
  ) where

import Data.Char (ord)
import Data.List (nub, intercalate, transpose)
import Data.Maybe (mapMaybe)
import Control.Monad (when)

-- ============================================================================
-- CORE TYPES
-- ============================================================================

-- | Coordinate as row-column pair (0-8 range)
type Coord = (Int, Int)

-- | Sudoku cell states
data Cell
  = Empty           -- ^ Empty cell available for filling
  | Fixed Int       -- ^ Pre-filled puzzle clue (1-9)
  | Filled Int      -- ^ User-filled cell (1-9)
  deriving (Eq, Show, Read)

-- | 9x9 Sudoku board with size invariant enforced by smart constructors
newtype Board = Board { unBoard :: [[Cell]] }
  deriving (Eq, Show)

-- | Puzzle difficulty levels
data Difficulty
  = Easy | Medium | Hard | Expert
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- ============================================================================
-- SMART CONSTRUCTORS
-- ============================================================================

-- | Empty 9x9 board with all cells in Empty state
emptyBoard :: Board
emptyBoard = Board $ replicate 9 $ replicate 9 Empty

-- | Smart constructor that validates board dimensions and cell values
mkBoard :: [[Cell]] -> Either String Board
mkBoard cells
  | length cells /= 9 = 
      Left "Board must have exactly 9 rows"
  | any ((/= 9) . length) cells = 
      Left "Each row must have exactly 9 cells"
  | not (all isValidCell (concat cells)) = 
      Left "Invalid cell values (must be 1-9)"
  | otherwise = 
      Right $ Board cells
  where
    isValidCell :: Cell -> Bool
    isValidCell Empty = True
    isValidCell (Fixed n) = n >= 1 && n <= 9
    isValidCell (Filled n) = n >= 1 && n <= 9

-- | Parse board from character grid representation
--   '.' = Empty, '1'-'9' = Fixed values
fromList :: [[Char]] -> Either String Board
fromList boardLines
  | length boardLines /= 9 = Left "Must have 9 rows"
  | any ((/= 9) . length) boardLines = Left "Each row must have 9 characters"
  | otherwise = do
      cells <- traverse (traverse parseCell) boardLines
      mkBoard cells
  where
    parseCell :: Char -> Either String Cell
    parseCell '.' = Right Empty
    parseCell c
      | c >= '1' && c <= '9' = Right $ Fixed (ord c - ord '0')
      | otherwise = Left $ "Invalid character: " ++ [c]

-- ============================================================================
-- CELL ACCESS
-- ============================================================================

-- | Get cell at specified coordinate (0-8 range)
--   Throws error on out-of-bounds access (programmer error)
getCell :: Coord -> Board -> Cell
getCell (r, c) (Board{unBoard = b})
  | r < 0 || r > 8 || c < 0 || c > 8 = 
      error "Coordinate out of bounds (expected 0-8)"
  | otherwise = b !! r !! c

-- | Extract numeric value from cell (Empty -> 0)
cellValue :: Coord -> Board -> Int
cellValue coord board = case getCell coord board of
  Empty -> 0
  Fixed n -> n
  Filled n -> n

-- | Convert cell to Maybe Int (Empty -> Nothing)
cellToValue :: Cell -> Maybe Int
cellToValue Empty = Nothing
cellToValue (Fixed n) = Just n
cellToValue (Filled n) = Just n

-- | Check if cell at coordinate is empty
isCellEmpty :: Coord -> Board -> Bool
isCellEmpty coord board = getCell coord board == Empty

-- ============================================================================
-- IMMUTABLE UPDATES
-- ============================================================================

-- | Create new board with cell updated at specified coordinate
--   Original board remains unchanged
setCell :: Coord -> Cell -> Board -> Board
setCell (r, c) cell (Board{unBoard = b}) = Board $
  take r b ++ [take c row ++ [cell] ++ drop (c + 1) row] ++ drop (r + 1) b
  where row = b !! r

-- | Place numeric value (1-9) at coordinate as Filled cell
placeValue :: Coord -> Int -> Board -> Board
placeValue coord n board
  | n < 1 || n > 9 = error "Value must be between 1-9"
  | otherwise = setCell coord (Filled n) board

-- ============================================================================
-- SUDOKU UNITS
-- ============================================================================

-- | Extract all 9 rows as Cell lists
rows :: Board -> [[Cell]]
rows (Board{unBoard = b}) = b

-- | Extract all 9 columns as Cell lists
cols :: Board -> [[Cell]]
cols (Board{unBoard = b}) = transpose b

-- | Extract all 9 3x3 boxes as Cell lists
--   Uses block-row/block-column indexing: br*3+r, bc*3+c
boxes :: Board -> [[Cell]]
boxes (Board{unBoard = b}) = 
  [ [b !! (br*3 + r) !! (bc*3 + c) 
    | r <- [0..2], c <- [0..2]]
  | br <- [0..2], bc <- [0..2]
  ]

-- ============================================================================
-- VALIDATION
-- ============================================================================

-- | Check if placing value n at coordinate is valid Sudoku move
isValidMove :: Coord -> Int -> Board -> Bool
isValidMove (r, c) n board
  | n < 1 || n > 9 = False
  | otherwise =
      let rowUsed = mapMaybe cellToValue (rows board !! r)
          colUsed = mapMaybe cellToValue (cols board !! c)
          boxIdx  = (r `div` 3) * 3 + (c `div` 3)
          boxUsed = mapMaybe cellToValue (boxes board !! boxIdx)
      in n `notElem` (rowUsed ++ colUsed ++ boxUsed)

-- | Check if board is completely filled (no Empty cells)
isComplete :: Board -> Bool
isComplete (Board{unBoard = b}) = all (all (/= Empty)) b

-- | Validate complete board: no duplicates in any row/column/box
isValidBoard :: Board -> Bool
isValidBoard board = 
  all isValidUnit (rows board ++ cols board ++ boxes board)
  where
    isValidUnit :: [Cell] -> Bool
    isValidUnit unit = 
      let nums = mapMaybe cellToValue unit
      in length nums == length (nub nums)

-- ============================================================================
-- RENDERING
-- ============================================================================

-- | Pretty-print board to console with box separators
printBoard :: Board -> IO ()
printBoard board = do
  putStrLn "┌───────┬───────┬───────┐"
  mapM_ printRowGroup [0..2]
  putStrLn "└───────┴───────┴───────┘"
  where
    b = unBoard board
    
    -- Print group of 3 rows with horizontal separator
    printRowGroup :: Int -> IO ()
    printRowGroup g = do
      mapM_ (printRow g) [0..2]
      when (g < 2) $ putStrLn "├───────┼───────┼───────┤"
    
    -- Print single row divided into 3-cell groups
    printRow :: Int -> Int -> IO ()
    printRow g ri = do
      let row = b !! (g*3 + ri)
          groups = [take 3 $ drop (i*3) row | i <- [0..2]]
      putStrLn $ "│ " ++ intercalate " │ " 
                  (map (unwords . map cellDisplay) groups) ++ " │"
    
    -- Cell display format
    cellDisplay :: Cell -> String
    cellDisplay Empty = "."
    cellDisplay (Fixed n) = show n ++ "*"
    cellDisplay (Filled n) = show n
