{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Web.Scotty
import Network.Wai.Middleware.Cors
import Network.HTTP.Types
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text.Lazy as TL
import qualified Data.Set as Set
import System.Random (randomRIO)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import Control.Monad (filterM)
import System.Directory (doesFileExist)

import KenKen.Types
import KenKen.Grid
import KenKen.Cage
import KenKen.Solver
import KenKen.Parser

-- API Response Types
data ApiResponse a = ApiResponse
  { apiStatus :: String
  , apiData :: Maybe a
  , apiError :: Maybe String
  } deriving (Generic)

instance ToJSON a => ToJSON (ApiResponse a)
instance FromJSON a => FromJSON (ApiResponse a)

main :: IO ()
main = scotty 3001 $ do
  middleware $ cors $ const $ Just $ simpleCorsResourcePolicy
    { corsOrigins = Just (["http://localhost:4321"], True)
    , corsMethods = ["GET", "POST", "OPTIONS"]
    , corsRequestHeaders = ["Content-Type"]
    }

  -- Get a random puzzle from file
  get "/api/puzzle" $ do
    files <- liftIO $ listDirectory "puzzles"
    let kenFiles = filter (\f -> takeExtension f == ".ken") files
    if null kenFiles
      then json (ApiResponse "error" Nothing (Just "No puzzles found") :: ApiResponse ())
      else do
        idx <- liftIO $ randomRIO (0, length kenFiles - 1)
        let selectedFile = "puzzles" </> (kenFiles !! idx)
        content <- liftIO $ readFile selectedFile
        case parsePuzzleFile content of
          Left err -> json (ApiResponse "error" Nothing (Just err) :: ApiResponse ())
          Right puzzle -> json $ ApiResponse "success" (Just puzzle) Nothing

  -- Solve a puzzle
  post "/api/solve" $ do
    puzzle <- jsonData :: ActionM Puzzle
    case solvePuzzle puzzle of
      Nothing -> json (ApiResponse "error" Nothing (Just "No solution found") :: ApiResponse ())
      Just sol -> json $ ApiResponse "success" (Just (gridToNestedList sol)) Nothing

  -- Get a hint for the current puzzle state
  post "/api/hint" $ do
    gameState <- jsonData :: ActionM GameState
    case getHint gameState of
      Nothing -> json (ApiResponse "error" Nothing (Just "No hint found") :: ApiResponse ())
      Just (pos, val) -> json $ ApiResponse "success" (Just (pos, val)) Nothing

  -- Health check
  get "/health" $ do
    text "OK"
