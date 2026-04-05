module Main where

import KenKen
import System.Console.ANSI
import System.IO

main :: IO ()
main = do
  -- Setup terminal
  hSetBuffering stdout NoBuffering
  hideCursor
  
  -- Create directories
  createPuzzleDirectory
  
  -- Start game
  mainMenu
  
  -- Cleanup
  showCursor
  clearScreen
  setCursorPosition 0 0