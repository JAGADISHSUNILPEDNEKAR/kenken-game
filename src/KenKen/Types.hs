{-# LANGUAGE DeriveGeneric #-}
module KenKen.Types where

import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics

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

-- Cage definition
data Cage = Cage
  { cageId :: Int
  , cageOperation :: Operation
  , cageTarget :: Int
  , cageCells :: Set Position
  } deriving (Eq, Show, Generic)

-- Game state
data GameState = GameState
  { gsGrid :: Grid
  , gsSize :: Size
  , gsCages :: [Cage]
  , gsSelectedCell :: Maybe Position
  , gsErrors :: Set Position
  , gsCompleted :: Bool
  } deriving (Show, Generic)

-- Move result
data MoveResult = Valid | Invalid String | Completed
  deriving (Eq, Show)

-- Puzzle definition
data Puzzle = Puzzle
  { puzzleSize :: Size
  , puzzleCages :: [Cage]
  , puzzleSolution :: Maybe Grid
  } deriving (Show, Generic)

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