module KenKen.Grid where

import KenKen.Types
import Data.Array
import Data.Maybe (isJust, fromJust)
import Data.List (nub)

-- Get all values in a row
getRow :: Grid -> Int -> [Value]
getRow grid row = [fromJust val | col <- [1..snd (snd (bounds grid))], 
                   let val = grid ! (row, col), isJust val]

-- Get all values in a column
getColumn :: Grid -> Int -> [Value]
getColumn grid col = [fromJust val | row <- [1..fst (snd (bounds grid))], 
                      let val = grid ! (row, col), isJust val]

-- Check if a value can be placed at a position
canPlaceValue :: Grid -> Position -> Value -> Bool
canPlaceValue grid (r, c) val = 
  let size = fst (snd (bounds grid))
      rowVals = getRow grid r
      colVals = getColumn grid c
  in val >= 1 && val <= size && 
     val `notElem` rowVals && 
     val `notElem` colVals

-- Count filled cells
filledCells :: Grid -> Int
filledCells grid = length [val | val <- elems grid, isJust val]

-- Check if grid is completely filled
isGridFull :: Grid -> Bool
isGridFull grid = all isJust (elems grid)

-- Get empty positions
getEmptyPositions :: Grid -> [Position]
getEmptyPositions grid = [pos | (pos, val) <- assocs grid, not (isJust val)]

-- Clear a cell
clearCell :: Grid -> Position -> Grid
clearCell grid pos = setCellValue grid pos Nothing

-- Get grid size
gridSize :: Grid -> Size
gridSize grid = fst (snd (bounds grid))

-- Create a grid from a list of values
fromList :: Size -> [[Maybe Value]] -> Grid
fromList n values = array ((1,1), (n,n)) 
  [((i,j), values !! (i-1) !! (j-1)) | i <- [1..n], j <- [1..n]]

-- Convert grid to list
toList :: Grid -> [[Maybe Value]]
toList grid = 
  let n = gridSize grid
  in [[grid ! (i,j) | j <- [1..n]] | i <- [1..n]]

-- Check if all values in list are unique (ignoring Nothing)
allUnique :: [Value] -> Bool
allUnique vals = length vals == length (nub vals)