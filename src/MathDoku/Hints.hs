{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MathDoku.Hints
    ( HintType(..)
    , Hint(..)
    , HintError(..)
    , getHint
    , getAllHintTypes
    , hintCost
    , isParity
    , isPrime
    , isDivisibleBy
    , inRange
    ) where

import MathDoku.Core
import MathDoku.Solver (resolverSudoku)

-- ============================================================================
-- HINT TYPES
-- ============================================================================

data HintType = Parity | Prime | Divisible3 | Divisible2 | NarrowRange | MediumRange | Reveal
    deriving (Eq, Show, Read, Ord, Enum, Bounded)

data HintError = CellAlreadyFilled Coord 
                             | CellOutOfBounds Coord 
                             | PuzzleHasNoSolution 
                             | InvalidHintType HintType
    deriving (Eq, Show)

data Hint = Hint { hintCoord :: Coord
                                 , hintType :: HintType
                                 , hintMessage :: String
                                 , hintValue :: Int }
    deriving (Eq, Show)

-- ============================================================================
-- MAIN HINT LOGIC
-- ============================================================================

-- | Get hint for specific cell using solution
getHint :: HintType -> Board -> Coord -> Either HintError Hint
getHint hType board coord@(r, c)
    | r < 0 || r > 8 || c < 0 || c > 8 = Left $ CellOutOfBounds coord
    | not (isCellEmpty coord board) = Left $ CellAlreadyFilled coord
    | otherwise =
            case resolverSudoku board of
                Nothing -> Left PuzzleHasNoSolution
                Just sol -> Right $ constructHint hType coord sol

constructHint :: HintType -> Coord -> Board -> Hint
constructHint hType coord sol = Hint
    { hintCoord = coord
    , hintType = hType
    , hintValue = cellValue coord sol
    , hintMessage = message }
    where
        value = cellValue coord sol
        message = case hType of
            Parity -> "The number is " ++ isParity value
            Prime -> if isPrime value then "PRIME" else "COMPOSITE"
            Divisible3 -> if value `mod` 3 == 0 then "Divisible by 3" else "Not divisible by 3"
            Divisible2 -> if even value then "Even" else "Odd"
            NarrowRange -> 
                let lo = max 1 (value-1); hi = min 9 (value+1)
                in show value ++ " is between " ++ show lo ++ "-" ++ show hi
            MediumRange -> 
                let lo = max 1 (value-2); hi = min 9 (value+2)
                in show value ++ " is between " ++ show lo ++ "-" ++ show hi
            Reveal -> "Solution: " ++ show value

-- ============================================================================
-- MATH FUNCTIONS
-- ============================================================================

-- | Check if number is even/odd
isParity :: Int -> String
isParity n = if even n then "even" else "odd"

-- | Check if number is prime
isPrime :: Int -> Bool
isPrime n 
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = not $ any (\d -> n `mod` d == 0) [3,5..n-2]

-- | Check divisibility
isDivisibleBy :: Int -> Int -> Bool
isDivisibleBy _ 0 = False
isDivisibleBy n d = n `mod` d == 0

-- | Check range
inRange :: Int -> Int -> Int -> Bool
inRange lo hi n = n >= lo && n <= hi

-- ============================================================================
-- UTILITIES
-- ============================================================================

-- | Hint cost in points
hintCost :: HintType -> Int
hintCost Parity = 1
hintCost Prime = 1
hintCost Divisible3 = 1
hintCost Divisible2 = 1
hintCost NarrowRange = 2
hintCost MediumRange = 2
hintCost Reveal = 3

-- | All available hint types
getAllHintTypes :: [HintType]
getAllHintTypes = [minBound..maxBound]