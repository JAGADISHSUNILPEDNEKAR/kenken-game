module KenKen.FileIO where

import KenKen.Types
import KenKen.Parser
import KenKen.Validation
import System.Directory
import System.FilePath
import Control.Exception
import Data.Time

-- Load puzzle from file
loadPuzzle :: FilePath -> IO (Either String Puzzle)
loadPuzzle path = do
  exists <- doesFileExist path
  if not exists
    then return $ Left $ "File not found: " ++ path
    else do
      content <- try (readFile path) :: IO (Either IOException String)
      case content of
        Left err -> return $ Left $ "Error reading file: " ++ show err
        Right text -> 
          case parsePuzzleFile text of
            Left err -> return $ Left $ "Parse error: " ++ err
            Right puzzle -> 
              case validatePuzzle puzzle of
                Left err -> return $ Left $ "Invalid puzzle: " ++ err
                Right _ -> return $ Right puzzle

-- Save puzzle to file
savePuzzle :: FilePath -> Puzzle -> IO (Either String ())
savePuzzle path puzzle = do
  result <- try (writeFile path $ generatePuzzleFile puzzle) :: IO (Either IOException ())
  case result of
    Left err -> return $ Left $ "Error writing file: " ++ show err
    Right _ -> return $ Right ()

-- Load game state
loadGame :: FilePath -> IO (Either String Puzzle)
loadGame = loadPuzzle  -- For now, same as loading puzzle

-- Save game state
saveGame :: FilePath -> GameState -> IO (Either String ())
saveGame path gs = do
  let puzzle = Puzzle (gsSize gs) (gsCages gs) (Just $ gsGrid gs)
  savePuzzle path puzzle

-- Get list of puzzle files
getPuzzleFiles :: FilePath -> IO [FilePath]
getPuzzleFiles dir = do
  exists <- doesDirectoryExist dir
  if not exists
    then return []
    else do
      files <- getDirectoryContents dir
      return $ filter ((".ken" ==) . takeExtension) files

-- Create puzzle directory if it doesn't exist
createPuzzleDirectory :: IO ()
createPuzzleDirectory = do
  let dir = "puzzles"
  exists <- doesDirectoryExist dir
  unless exists $ createDirectory dir

-- Sample puzzles
samplePuzzle4x4 :: Puzzle
samplePuzzle4x4 = Puzzle 4 
  [ Cage 1 Add 5 (Set.fromList [(1,1), (1,2)])
  , Cage 2 Multiply 6 (Set.fromList [(1,3), (2,3)])
  , Cage 3 Subtract 1 (Set.fromList [(1,4), (2,4)])
  , Cage 4 Divide 2 (Set.fromList [(2,1), (2,2)])
  , Cage 5 Add 7 (Set.fromList [(3,1), (4,1)])
  , Cage 6 Multiply 12 (Set.fromList [(3,2), (3,3), (4,2)])
  , Cage 7 Add 5 (Set.fromList [(3,4), (4,4)])
  , Cage 8 None 4 (Set.fromList [(4,3)])
  ] Nothing

samplePuzzle6x6 :: Puzzle
samplePuzzle6x6 = Puzzle 6
  [ Cage 1 Add 11 (Set.fromList [(1,1), (1,2), (2,1)])
  , Cage 2 Multiply 30 (Set.fromList [(1,3), (1,4), (2,3)])
  , Cage 3 Subtract 1 (Set.fromList [(1,5), (1,6)])
  , Cage 4 Divide 2 (Set.fromList [(2,2), (3,2)])
  , Cage 5 Add 16 (Set.fromList [(2,4), (2,5), (3,4)])
  , Cage 6 None 6 (Set.fromList [(2,6)])
  , Cage 7 Multiply 60 (Set.fromList [(3,1), (4,1), (4,2)])
  , Cage 8 Add 13 (Set.fromList [(3,3), (4,3), (4,4)])
  , Cage 9 Subtract 3 (Set.fromList [(3,5), (3,6)])
  , Cage 10 Add 12 (Set.fromList [(4,5), (5,5), (5,6)])
  , Cage 11 Divide 3 (Set.fromList [(4,6), (5,4)])
  , Cage 12 Multiply 10 (Set.fromList [(5,1), (6,1)])
  , Cage 13 Add 7 (Set.fromList [(5,2), (5,3)])
  , Cage 14 Add 11 (Set.fromList [(6,2), (6,3), (6,4)])
  , Cage 15 Multiply 30 (Set.fromList [(6,5), (6,6)])
  ] Nothing

-- Auto-save functionality
autoSavePath :: IO FilePath
autoSavePath = do
  time <- getCurrentTime
  let timestamp = formatTime defaultTimeLocale "%Y%m%d_%H%M%S" time
  return $ "saves/autosave_" ++ timestamp ++ ".ken"

-- Auto-save game
autoSave :: GameState -> IO ()
autoSave gs = do
  createDirectoryIfMissing True "saves"
  path <- autoSavePath
  _ <- saveGame path gs
  return ()

-- Load most recent save
loadMostRecentSave :: IO (Either String Puzzle)
loadMostRecentSave = do
  exists <- doesDirectoryExist "saves"
  if not exists
    then return $ Left "No saves directory found"
    else do
      files <- getPuzzleFiles "saves"
      case files of
        [] -> return $ Left "No save files found"
        _ -> do
          let sorted = reverse $ sort files  -- Most recent first
              mostRecent = "saves" </> head sorted
          loadPuzzle mostRecent