module KenKen.Validation where

import KenKen.Types
import KenKen.Grid
import KenKen.Cage
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isJust)

-- Validate a move
validateMove :: GameState -> Position -> Value -> MoveResult
validateMove gs pos val
  | not (isValidPosition (gsSize gs) pos) = Invalid "Position out of bounds"
  | val < 1 || val > gsSize gs = Invalid "Value out of range"
  | not (canPlaceValue (gsGrid gs) pos val) = Invalid "Value already exists in row/column"
  | otherwise = 
      let newGrid = setCellValue (gsGrid gs) pos (Just val)
          cage = findCageByPosition (gsCages gs) pos
      in case cage of
           Nothing -> Invalid "No cage found for position"
           Just c -> if isCageComplete newGrid c && not (evaluateCage newGrid c)
                     then Invalid "Cage constraint violated"
                     else if isGridFull newGrid && allCagesSatisfied newGrid (gsCages gs)
                          then Completed
                          else Valid

-- Find all errors in current grid
findErrors :: GameState -> Set Position
findErrors gs = Set.fromList $ rowColErrors ++ cageErrors
  where
    grid = gsGrid gs
    size = gsSize gs
    
    -- Find row/column duplicates
    rowColErrors = [(r, c) | r <- [1..size], c <- [1..size],
                    let val = getCellValue grid (r, c),
                    isJust val,
                    hasDuplicateInRow grid r c || hasDuplicateInColumn grid r c]
    
    -- Find cage constraint violations
    cageErrors = concat [Set.toList (cageCells cage) | 
                         cage <- gsCages gs,
                         isCageComplete grid cage,
                         not (evaluateCage grid cage)]

-- Check for duplicate in row
hasDuplicateInRow :: Grid -> Int -> Int -> Bool
hasDuplicateInRow grid row col =
  case getCellValue grid (row, col) of
    Nothing -> False
    Just val -> length [c | c <- [1..gridSize grid], 
                       c /= col,
                       getCellValue grid (row, c) == Just val] > 0

-- Check for duplicate in column
hasDuplicateInColumn :: Grid -> Int -> Int -> Bool
hasDuplicateInColumn grid row col =
  case getCellValue grid (row, col) of
    Nothing -> False
    Just val -> length [r | r <- [1..gridSize grid],
                       r /= row,
                       getCellValue grid (r, col) == Just val] > 0

-- Check if puzzle is solved
isPuzzleSolved :: GameState -> Bool
isPuzzleSolved gs = isGridFull (gsGrid gs) && 
                    Set.null (findErrors gs) &&
                    allCagesSatisfied (gsGrid gs) (gsCages gs)

-- Validate puzzle structure
validatePuzzle :: Puzzle -> Either String ()
validatePuzzle puzzle
  | puzzleSize puzzle < 3 = Left "Puzzle size must be at least 3"
  | puzzleSize puzzle > 9 = Left "Puzzle size must be at most 9"
  | null (puzzleCages puzzle) = Left "Puzzle must have at least one cage"
  | not (allCellsCovered puzzle) = Left "All cells must belong to exactly one cage"
  | not (validCageOperations puzzle) = Left "Invalid cage operations"
  | otherwise = Right ()

-- Check if all cells are covered by exactly one cage
allCellsCovered :: Puzzle -> Bool
allCellsCovered puzzle =
  let allPositions = Set.fromList $ getAllPositions (puzzleSize puzzle)
      cagePositions = Set.unions $ map cageCells (puzzleCages puzzle)
      counts = map (\pos -> length $ filter (Set.member pos . cageCells) (puzzleCages puzzle)) 
                   (Set.toList allPositions)
  in allPositions == cagePositions && all (== 1) counts

-- Validate cage operations
validCageOperations :: Puzzle -> Bool
validCageOperations puzzle = all validCage (puzzleCages puzzle)
  where
    validCage cage = case (cageOperation cage, Set.size (cageCells cage)) of
      (None, 1) -> cageTarget cage >= 1 && cageTarget cage <= puzzleSize puzzle
      (Add, _) -> cageTarget cage > 0
      (Multiply, _) -> cageTarget cage > 0
      (Subtract, 2) -> cageTarget cage >= 0
      (Divide, 2) -> cageTarget cage > 0
      (Subtract, _) -> False
      (Divide, _) -> False
      _ -> True