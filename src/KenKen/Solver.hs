module KenKen.Solver where

import KenKen.Types
import KenKen.Grid
import KenKen.Cage
import KenKen.Validation
import Data.Maybe (isNothing)
import Data.List (minimumBy, sortBy)
import Data.Ord (comparing)
import Control.Monad (guard)

-- Solve a puzzle using backtracking
solvePuzzle :: Puzzle -> Maybe Grid
solvePuzzle puzzle = solve (emptyGrid (puzzleSize puzzle)) (puzzleCages puzzle)

-- Main solving algorithm
solve :: Grid -> [Cage] -> Maybe Grid
solve grid cages
  | isGridFull grid = if allCagesSatisfied grid cages then Just grid else Nothing
  | otherwise = tryValues
  where
    -- Find the empty cell with fewest possibilities (MRV heuristic)
    emptyPos = selectNextCell grid cages
    possibleValues = getPossibleValues grid cages emptyPos
    
    tryValues = firstJust [solve (setCellValue grid emptyPos (Just val)) cages | 
                           val <- possibleValues]

-- Select next cell using Minimum Remaining Values heuristic
selectNextCell :: Grid -> [Cage] -> Position
selectNextCell grid cages = 
  minimumBy (comparing $ \pos -> length (getPossibleValues grid cages pos))
            (getEmptyPositions grid)

-- Get possible values for a position
getPossibleValues :: Grid -> [Cage] -> Position -> [Value]
getPossibleValues grid cages pos =
  let size = gridSize grid
      basicValid = [v | v <- [1..size], canPlaceValue grid pos v]
      cage = findCageByPosition cages pos
  in case cage of
       Nothing -> basicValid
       Just c -> filter (\v -> isValidForCageConstraint grid c pos v) basicValid

-- Enhanced cage constraint checking
isValidForCageConstraint :: Grid -> Cage -> Position -> Value -> Bool
isValidForCageConstraint grid cage pos val =
  let tempGrid = setCellValue grid pos (Just val)
      filledPositions = [p | p <- Set.toList (cageCells cage), 
                        isJust (getCellValue tempGrid p)]
      remainingPositions = length (cageCells cage) - length filledPositions
  in case cageOperation cage of
    None -> val == cageTarget cage
    Add -> checkAddConstraint tempGrid cage val remainingPositions
    Multiply -> checkMultiplyConstraint tempGrid cage val remainingPositions
    Subtract -> remainingPositions == 0 && evaluateCage tempGrid cage
    Divide -> remainingPositions == 0 && evaluateCage tempGrid cage
    _ -> True

-- Check addition constraint with pruning
checkAddConstraint :: Grid -> Cage -> Value -> Int -> Bool
checkAddConstraint grid cage _ remaining
  | remaining == 0 = evaluateCage grid cage
  | otherwise = 
      let currentSum = sum (getCageValues grid cage)
          maxPossible = currentSum + remaining * gridSize grid
          minPossible = currentSum + remaining
      in minPossible <= cageTarget cage && maxPossible >= cageTarget cage

-- Check multiplication constraint with pruning
checkMultiplyConstraint :: Grid -> Cage -> Value -> Int -> Bool
checkMultiplyConstraint grid cage _ remaining
  | remaining == 0 = evaluateCage grid cage
  | otherwise =
      let currentProduct = product (getCageValues grid cage)
          size = gridSize grid
          maxPossible = currentProduct * (size ^ remaining)
      in cageTarget cage <= maxPossible && cageTarget cage `mod` currentProduct == 0

-- Get a hint for the current game state
getHint :: GameState -> Maybe (Position, Value)
getHint gs =
  case solvePuzzle (Puzzle (gsSize gs) (gsCages gs) Nothing) of
    Nothing -> Nothing
    Just solution -> 
      let emptyPositions = getEmptyPositions (gsGrid gs)
      in case emptyPositions of
           [] -> Nothing
           (pos:_) -> do
             val <- getCellValue solution pos
             return (pos, val)

-- Check if current state has a valid solution
hasSolution :: GameState -> Bool
hasSolution gs =
  let puzzle = Puzzle (gsSize gs) (gsCages gs) Nothing
      currentGrid = gsGrid gs
  in case solvePuzzle puzzle of
       Nothing -> False
       Just solution -> all (\(pos, val) -> 
                           isNothing val || getCellValue solution pos == val)
                           (assocs currentGrid)

-- Helper function
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Nothing:xs) = firstJust xs
firstJust (Just x:_) = Just x