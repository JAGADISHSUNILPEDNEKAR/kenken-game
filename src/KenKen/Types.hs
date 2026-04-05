{-# LANGUAGE DeriveGeneric #-}
module KenKen.Types where

import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics
import Data.Aeson
import qualified Data.Vector as V

-- Basic types
type Size = Int
type Position = (Int, Int)
type Value = Int
type Cell = Maybe Value

-- Grid representation
type Grid = Array Position Cell

-- Operations for cages
data Operation = Add | Subtract | Multiply | Divide | None
  deriving (Eq, Show, Read, Generic)

instance ToJSON Operation
instance FromJSON Operation

-- Cage definition
data Cage = Cage
  { cageId :: Int
  , cageOperation :: Operation
  , cageTarget :: Int
  , cageCells :: Set Position
  } deriving (Eq, Show, Generic)

instance ToJSON Cage
instance FromJSON Cage

-- Game state
data GameState = GameState
  { gsGrid :: Grid
  , gsSize :: Size
  , gsCages :: [Cage]
  , gsSelectedCell :: Maybe Position
  , gsErrors :: Set Position
  , gsCompleted :: Bool
  } deriving (Show, Generic)

-- We'll hand-roll the GameState JSON to handle the Grid (Array)
instance ToJSON GameState where
  toJSON gs = object
    [ "grid" .= gridToNestedList (gsGrid gs)
    , "size" .= gsSize gs
    , "cages" .= gsCages gs
    , "selectedCell" .= gsSelectedCell gs
    , "errors" .= gsErrors gs
    , "completed" .= gsCompleted gs
    ]

instance FromJSON GameState where
  parseJSON = withObject "GameState" $ \v -> GameState
    <$> (nestedListToGrid <$> v .: "size" <*> v .: "grid")
    <*> v .: "cages"
    <*> v .: "selectedCell"
    <*> v .: "errors"
    <*> v .: "completed"

-- Move result
data MoveResult = Valid | Invalid String | Completed
  deriving (Eq, Show, Generic)

instance ToJSON MoveResult
instance FromJSON MoveResult

-- Puzzle definition
data Puzzle = Puzzle
  { puzzleSize :: Size
  , puzzleCages :: [Cage]
  , puzzleSolution :: Maybe [[Cell]] -- Use nested list for JSON
  } deriving (Show, Generic)

instance ToJSON Puzzle
instance FromJSON Puzzle

-- Helper functions
emptyGrid :: Size -> Grid
emptyGrid n = array ((1,1), (n,n)) [((i,j), Nothing) | i <- [1..n], j <- [1..n]]

getCellValue :: Grid -> Position -> Cell
getCellValue grid pos = grid ! pos

setCellValue :: Grid -> Position -> Cell -> Grid
setCellValue grid pos val = grid // [(pos, val)]

isValidPosition :: Size -> Position -> Bool
isValidPosition size (r, c) = r >= 1 && r <= size && c >= 1 && c <= size

getAllPositions :: Size -> [Position]
getAllPositions n = [(i,j) | i <- [1..n], j <- [1..n]]

-- JSON Helpers for Grid (Array)
gridToNestedList :: Grid -> [[Cell]]
gridToNestedList grid = 
  let ((r1, c1), (r2, c2)) = bounds grid
  in [[grid ! (r, c) | c <- [c1..c2]] | r <- [r1..r2]]

nestedListToGrid :: Size -> [[Cell]] -> Grid
nestedListToGrid n rows = 
  listArray ((1,1), (n,n)) (concat rows)