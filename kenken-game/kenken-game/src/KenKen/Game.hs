module KenKen.Game where

import KenKen.Types
import KenKen.Grid
import KenKen.Validation
import KenKen.Solver
import KenKen.Display
import KenKen.FileIO
import KenKen.Parser
import System.IO
import System.Console.ANSI
import Control.Monad (when, unless)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set

-- Game history for undo/redo
data GameHistory = GameHistory
  { past :: [GameState]
  , future :: [GameState]
  } deriving (Show)

-- Initialize game from puzzle
initGame :: Puzzle -> GameState
initGame puzzle = GameState
  { gsGrid = emptyGrid (puzzleSize puzzle)
  , gsSize = puzzleSize puzzle
  , gsCages = puzzleCages puzzle
  , gsSelectedCell = Just (1, 1)
  , gsErrors = Set.empty
  , gsCompleted = False
  }

-- Main game loop
gameLoop :: GameState -> GameHistory -> IO ()
gameLoop gs history = do
  -- Update errors
  let gsWithErrors = gs { gsErrors = findErrors gs }
  
  -- Check completion
  let gsWithCompletion = gsWithErrors { gsCompleted = isPuzzleSolved gsWithErrors }
  
  -- Display board
  displayBoard gsWithCompletion
  
  -- Handle completion
  when (gsCompleted gsWithCompletion) $ do
    putStrLn "Press any key to continue..."
    _ <- getChar
    return ()
  
  -- Get input
  unless (gsCompleted gsWithCompletion) $ do
    input <- getInput
    processInput gsWithCompletion history input

-- Get user input
getInput :: IO Char
getInput = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  return c

-- Process user input
processInput :: GameState -> GameHistory -> Char -> IO ()
processInput gs history input = case input of
  -- Number input
  c | c >= '1' && c <= '9' -> handleNumberInput gs history (read [c])
  '0' -> handleClearCell gs history
  
  -- Navigation
  'w' -> handleMove gs history (-1, 0)
  's' -> handleMove gs history (1, 0)
  'a' -> handleMove gs history (0, -1)
  'd' -> handleMove gs history (0, 1)
  
  -- Special keys (would need proper key handling)
  '\ESC' -> do
    nextChar <- getChar
    when (nextChar == '[') $ do
      arrowKey <- getChar
      case arrowKey of
        'A' -> handleMove gs history (-1, 0)  -- Up
        'B' -> handleMove gs history (1, 0)   -- Down
        'C' -> handleMove gs history (0, 1)   -- Right
        'D' -> handleMove gs history (0, -1)  -- Left
        _ -> gameLoop gs history
  
  -- Commands
  'h' -> handleHint gs history
  'u' -> handleUndo gs history
  'r' -> handleRedo gs history
  's' -> handleSave gs history
  'l' -> handleLoad gs history
  'n' -> handleNewGame gs history
  'q' -> handleQuit
  
  -- Unknown input
  _ -> gameLoop gs history

-- Handle number input
handleNumberInput :: GameState -> GameHistory -> Value -> IO ()
handleNumberInput gs history val = 
  case gsSelectedCell gs of
    Nothing -> gameLoop gs history
    Just pos -> 
      case validateMove gs pos val of
        Invalid msg -> do
          displayError msg
          _ <- getChar
          gameLoop gs history
        _ -> do
          let newGrid = setCellValue (gsGrid gs) pos (Just val)
              newGs = gs { gsGrid = newGrid }
              newHistory = GameHistory 
                { past = gs : past history
                , future = []
                }
          gameLoop newGs newHistory

-- Handle clearing a cell
handleClearCell :: GameState -> GameHistory -> IO ()
handleClearCell gs history =
  case gsSelectedCell gs of
    Nothing -> gameLoop gs history
    Just pos -> do
      let newGrid = clearCell (gsGrid gs) pos
          newGs = gs { gsGrid = newGrid }
          newHistory = GameHistory 
            { past = gs : past history
            , future = []
            }
      gameLoop newGs newHistory

-- Handle cursor movement
handleMove :: GameState -> GameHistory -> (Int, Int) -> IO ()
handleMove gs history (dr, dc) =
  case gsSelectedCell gs of
    Nothing -> gameLoop gs history
    Just (r, c) -> do
      let newR = max 1 $ min (gsSize gs) (r + dr)
          newC = max 1 $ min (gsSize gs) (c + dc)
          newGs = gs { gsSelectedCell = Just (newR, newC) }
      gameLoop newGs history

-- Handle hint request
handleHint :: GameState -> GameHistory -> IO ()
handleHint gs history = do
  case getHint gs of
    Nothing -> do
      displayError "No hint available"
      _ <- getChar
    Just (pos, val) -> do
      displaySuccess $ "Hint: Try " ++ show val ++ " at position " ++ show pos
      _ <- getChar
  gameLoop gs history

-- Handle undo
handleUndo :: GameState -> GameHistory -> IO ()
handleUndo gs history =
  case past history of
    [] -> do
      displayError "Nothing to undo"
      _ <- getChar
      gameLoop gs history
    (prev:rest) -> do
      let newHistory = GameHistory 
            { past = rest
            , future = gs : future history
            }
      gameLoop prev newHistory

-- Handle redo
handleRedo :: GameState -> GameHistory -> IO ()
handleRedo gs history =
  case future history of
    [] -> do
      displayError "Nothing to redo"
      _ <- getChar
      gameLoop gs history
    (next:rest) -> do
      let newHistory = GameHistory 
            { past = gs : past history
            , future = rest
            }
      gameLoop next newHistory

-- Handle save
handleSave :: GameState -> GameHistory -> IO ()
handleSave gs history = do
  putStrLn "Enter filename to save:"
  filename <- getLine
  result <- saveGame filename gs
  case result of
    Left err -> displayError err
    Right _ -> displaySuccess "Game saved successfully"
  _ <- getChar
  gameLoop gs history

-- Handle load
handleLoad :: GameState -> GameHistory -> IO ()
handleLoad gs history = do
  putStrLn "Enter filename to load:"
  filename <- getLine
  result <- loadGame filename
  case result of
    Left err -> do
      displayError err
      _ <- getChar
      gameLoop gs history
    Right puzzle -> do
      displaySuccess "Game loaded successfully"
      _ <- getChar
      let newGs = initGame puzzle
      gameLoop newGs (GameHistory [] [])

-- Handle new game
handleNewGame :: GameState -> GameHistory -> IO ()
handleNewGame _ _ = do
  mainMenu

-- Handle quit
handleQuit :: IO ()
handleQuit = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "Thanks for playing KenKen!"

-- Main menu
mainMenu :: IO ()
mainMenu = do
  clearScreen
  setCursorPosition 0 0
  putStrLn "KenKen Puzzle Game"
  putStrLn "=================="
  putStrLn ""
  putStrLn "1. Load puzzle from file"
  putStrLn "2. Play sample puzzle (4x4)"
  putStrLn "3. Play sample puzzle (6x6)"
  putStrLn "4. Quit"
  putStrLn ""
  putStrLn "Enter your choice:"
  
  choice <- getChar
  case choice of
    '1' -> do
      putStrLn "Enter puzzle filename:"
      filename <- getLine
      result <- loadPuzzle filename
      case result of
        Left err -> do
          displayError err
          _ <- getChar
          mainMenu
        Right puzzle -> startGame puzzle
    '2' -> startGame samplePuzzle4x4
    '3' -> startGame samplePuzzle6x6
    '4' -> handleQuit
    _ -> mainMenu

-- Start a new game
startGame :: Puzzle -> IO ()
startGame puzzle = do
  let gs = initGame puzzle
      history = GameHistory [] []
  gameLoop gs history