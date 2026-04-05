module KenKen.Cage where

import KenKen.Types
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isJust, fromJust)
import Data.List (foldl')

-- Get values from cage cells
getCageValues :: Grid -> Cage -> [Value]
getCageValues grid cage = 
  [fromJust val | pos <- Set.toList (cageCells cage), 
   let val = getCellValue grid pos, isJust val]

-- Check if cage is complete (all cells filled)
isCageComplete :: Grid -> Cage -> Bool
isCageComplete grid cage = 
  all (isJust . getCellValue grid) (Set.toList (cageCells cage))

-- Evaluate cage constraint
evaluateCage :: Grid -> Cage -> Bool
evaluateCage grid cage
  | not (isCageComplete grid cage) = True  -- Incomplete cages are not invalid yet
  | otherwise = case cageOperation cage of
      Add -> sum values == cageTarget cage
      Subtract -> length values == 2 && abs (head values - values !! 1) == cageTarget cage
      Multiply -> product values == cageTarget cage
      Divide -> length values == 2 && 
                 let (a, b) = (head values, values !! 1)
                 in (a `div` b == cageTarget cage && a `mod` b == 0) ||
                    (b `div` a == cageTarget cage && b `mod` a == 0)
      None -> length values == 1 && head values == cageTarget cage
  where values = getCageValues grid cage

-- Find cage containing a position
findCageByPosition :: [Cage] -> Position -> Maybe Cage
findCageByPosition cages pos = 
  case filter (\c -> pos `Set.member` cageCells c) cages of
    [] -> Nothing
    (c:_) -> Just c

-- Get cage ID for a position
getCageId :: [Cage] -> Position -> Maybe Int
getCageId cages pos = cageId <$> findCageByPosition cages pos

-- Check if all cages are satisfied
allCagesSatisfied :: Grid -> [Cage] -> Bool
allCagesSatisfied grid cages = all (evaluateCage grid) cages

-- Get possible values for a cage cell considering cage constraints
getCagePossibleValues :: Grid -> Cage -> Position -> [Value]
getCagePossibleValues grid cage pos = 
  let size = gridSize grid
      basicPossible = [v | v <- [1..size], canPlaceValue grid pos v]
  in filter (isValidForCage grid cage pos) basicPossible

-- Check if a value is valid for the cage constraint
isValidForCage :: Grid -> Cage -> Position -> Value -> Bool
isValidForCage grid cage pos val =
  let tempGrid = setCellValue grid pos (Just val)
  in case cageOperation cage of
    None -> val == cageTarget cage
    _ -> if isCageComplete tempGrid cage
         then evaluateCage tempGrid cage
         else True  -- Can't determine yet if incomplete