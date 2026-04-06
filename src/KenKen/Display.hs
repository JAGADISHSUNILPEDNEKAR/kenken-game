module KenKen.Display where

import KenKen.Types
import KenKen.Cage
import KenKen.Grid
import Data.Array (assocs, bounds, (!))
import Data.Maybe (fromMaybe, isJust)
import Data.List (intercalate, nub, sort)
import qualified Data.Set as Set
import System.Console.ANSI
import Control.Monad (when, unless, replicateM_)

-- Display the game board
displayBoard :: GameState -> IO ()
displayBoard gs = do
  clearScreen
  setCursorPosition 0 0
  putStrLn $ "KenKen Puzzle (" ++ show (gsSize gs) ++ "x" ++ show (gsSize gs) ++ ")"
  putStrLn ""
  displayGrid gs
  putStrLn ""
  displayStatus gs
  putStrLn ""
  displayControls

-- Display the grid with cage boundaries and values
displayGrid :: GameState -> IO ()
displayGrid gs = do
  let size = gsSize gs
  
  -- Display column numbers
  putStr "     "
  mapM_ (\c -> putStr $ "  " ++ show c ++ " ") [1..size]
  putStrLn ""
  
  -- Display top border
  putStr "   +"
  replicateM_ size (putStr "----+")
  putStrLn ""
  
  -- Display each row
  mapM_ (displayRow gs) [1..size]

-- Display a single row
displayRow :: GameState -> Int -> IO ()
displayRow gs row = do
  let size = gsSize gs
      grid = gsGrid gs
      cages = gsCages gs
      errors = gsErrors gs
      selected = gsSelectedCell gs
  
  -- Row number
  putStr $ " " ++ show row ++ " |"
  
  -- Cell contents
  mapM_ (\col -> do
    let pos = (row, col)
        val = getCellValue grid pos
        isError = pos `Set.member` errors
        isSelected = Just pos == selected
    
    -- Set color based on state
    if isError then setSGR [SetColor Foreground Vivid Red] else return ()
    if isSelected then setSGR [SetColor Background Dull Yellow] else return ()
    
    -- Display cage info or value
    if row == 1 && col == 1 || isFirstInCage cages pos
      then displayCageInfo gs pos
      else putStr $ " " ++ maybe " " show val ++ " "
    
    setSGR [Reset]
    
    -- Display vertical separator
    if hasRightBorder cages pos size then putStr "|" else putStr " "
    ) [1..size]
  
  putStrLn ""
  
  -- Display horizontal borders
  putStr "   +"
  mapM_ (\col -> do
    if hasBottomBorder cages (row, col) size
      then putStr "----+"
      else putStr "    +"
    ) [1..size]
  putStrLn ""

-- Display cage information (operation and target)
displayCageInfo :: GameState -> Position -> IO ()
displayCageInfo gs pos = do
  case findCageByPosition (gsCages gs) pos of
    Nothing -> putStr "    "
    Just cage -> do
      let opStr = operationSymbol (cageOperation cage)
          targetStr = show (cageTarget cage)
          info = take 3 $ targetStr ++ opStr ++ "  "
      putStr $ " " ++ info

-- Check if position is first in its cage (top-left)
isFirstInCage :: [Cage] -> Position -> Bool
isFirstInCage cages pos =
  case findCageByPosition cages pos of
    Nothing -> False
    Just cage -> pos == minimum (Set.toList $ cageCells cage)

-- Check if position has a right border
hasRightBorder :: [Cage] -> Position -> Size -> Bool
hasRightBorder cages (r, c) size =
  c == size || getCageId cages (r, c) /= getCageId cages (r, c + 1)

-- Check if position has a bottom border
hasBottomBorder :: [Cage] -> Position -> Size -> Bool
hasBottomBorder cages (r, c) size =
  r == size || getCageId cages (r, c) /= getCageId cages (r + 1, c)

-- Get operation symbol
operationSymbol :: Operation -> String
operationSymbol Add = "+"
operationSymbol Subtract = "-"
operationSymbol Multiply = "×"
operationSymbol Divide = "÷"
operationSymbol None = ""

-- Display game status
displayStatus :: GameState -> IO ()
displayStatus gs = do
  let filled = filledCells (gsGrid gs)
      total = gsSize gs * gsSize gs
      progress = show filled ++ "/" ++ show total ++ " cells filled"
  
  putStrLn $ "Progress: " ++ progress
  
  if gsCompleted gs then do
    setSGR [SetColor Foreground Vivid Green]
    putStrLn "🎉 Congratulations! Puzzle solved! 🎉"
    setSGR [Reset]
  else return ()
  
  unless (Set.null $ gsErrors gs) $ do
    setSGR [SetColor Foreground Vivid Red]
    putStrLn "⚠ Errors found in red cells"
    setSGR [Reset]

-- Display controls
displayControls :: IO ()
displayControls = do
  putStrLn "Controls:"
  putStrLn "  Arrow keys: Move cursor"
  putStrLn "  1-9: Enter number"
  putStrLn "  0/Delete: Clear cell"
  putStrLn "  h: Hint"
  putStrLn "  u: Undo"
  putStrLn "  r: Redo"
  putStrLn "  s: Save game"
  putStrLn "  l: Load game"
  putStrLn "  n: New game"
  putStrLn "  q: Quit"

-- Display available puzzles
displayPuzzleList :: [String] -> IO ()
displayPuzzleList puzzles = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "Available Puzzles:"
  putStrLn ""
  mapM_ (\(i, p) -> putStrLn $ show i ++ ". " ++ p) $ zip [1..] puzzles
  putStrLn ""
  putStrLn "Enter puzzle number or 'q' to quit:"

-- Display error message
displayError :: String -> IO ()
displayError msg = do
  setSGR [SetColor Foreground Vivid Red]
  putStrLn $ "Error: " ++ msg
  setSGR [Reset]
  putStrLn "Press any key to continue..."

-- Display success message
displaySuccess :: String -> IO ()
displaySuccess msg = do
  setSGR [SetColor Foreground Vivid Green]
  putStrLn $ "✓ " ++ msg
  setSGR [Reset]

-- Simple text-based grid (for debugging)
displaySimpleGrid :: Grid -> IO ()
displaySimpleGrid grid = do
  let size = gridSize grid
  mapM_ (\r -> do
    mapM_ (\c -> putStr $ maybe "." show (getCellValue grid (r, c)) ++ " ") [1..size]
    putStrLn ""
    ) [1..size]