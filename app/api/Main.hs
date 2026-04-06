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
  middleware simpleCors

  -- Get a sample puzzle
  get "/api/puzzle" $ do
    -- For now, return a hardcoded 4x4 puzzle or load one from file
    let puzzle = Puzzle 4 
          [ Cage 1 Add 5 (Set.fromList [(1,1),(1,2)])
          , Cage 2 Multiply 6 (Set.fromList [(1,3),(2,3)])
          , Cage 3 Subtract 1 (Set.fromList [(1,4),(2,4)])
          , Cage 4 None 2 (Set.fromList [(2,1)])
          , Cage 5 Divide 2 (Set.fromList [(2,2),(3,2)])
          , Cage 6 Add 7 (Set.fromList [(3,1),(4,1)])
          , Cage 7 Multiply 4 (Set.fromList [(3,3),(3,4)])
          , Cage 8 Subtract 2 (Set.fromList [(4,2),(4,3)])
          , Cage 9 None 4 (Set.fromList [(4,4)])
          ] Nothing
    json $ ApiResponse "success" (Just puzzle) Nothing

  -- Solve a puzzle
  post "/api/solve" $ do
    puzzle <- jsonData :: ActionM Puzzle
    case solvePuzzle puzzle of
      Nothing -> json (ApiResponse "error" Nothing (Just "No solution found") :: ApiResponse ())
      Just sol -> json $ ApiResponse "success" (Just (gridToNestedList sol)) Nothing

  -- Health check
  get "/health" $ do
    text "OK"
