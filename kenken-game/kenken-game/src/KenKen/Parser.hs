module KenKen.Parser where

import KenKen.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (void)

type Parser = Parsec Void String

-- Parse a complete puzzle file
parsePuzzleFile :: String -> Either String Puzzle
parsePuzzleFile input = 
  case parse puzzleParser "puzzle" input of
    Left err -> Left (errorBundlePretty err)
    Right puzzle -> Right puzzle

-- Main puzzle parser
puzzleParser :: Parser Puzzle
puzzleParser = do
  _ <- space
  size <- sizeParser
  _ <- space
  cages <- many cageParser
  _ <- space
  _ <- eof
  return $ Puzzle size cages Nothing

-- Parse puzzle size
sizeParser :: Parser Size
sizeParser = do
  _ <- string "size"
  _ <- space
  n <- L.decimal
  _ <- newline
  return n

-- Parse a cage
cageParser :: Parser Cage
cageParser = do
  _ <- string "cage"
  _ <- space
  cid <- L.decimal
  _ <- space
  op <- operationParser
  _ <- space
  target <- L.decimal
  _ <- space
  cells <- cellsParser
  _ <- newline
  return $ Cage cid op target (Set.fromList cells)

-- Parse operation
operationParser :: Parser Operation
operationParser = choice
  [ Add <$ char '+'
  , Subtract <$ char '-'
  , Multiply <$ char '*'
  , Divide <$ char '/'
  , None <$ char '='
  ]

-- Parse cell positions
cellsParser :: Parser [Position]
cellsParser = between (char '[') (char ']') $ 
  positionParser `sepBy` (char ',' >> space)

-- Parse a single position
positionParser :: Parser Position
positionParser = do
  _ <- char '('
  r <- L.decimal
  _ <- char ','
  c <- L.decimal
  _ <- char ')'
  return (r, c)

-- Generate puzzle file content
generatePuzzleFile :: Puzzle -> String
generatePuzzleFile puzzle = unlines $
  [ "size " ++ show (puzzleSize puzzle) ] ++
  map cageToString (puzzleCages puzzle)
  where
    cageToString cage = 
      "cage " ++ show (cageId cage) ++ " " ++
      operationToChar (cageOperation cage) ++ " " ++
      show (cageTarget cage) ++ " " ++
      positionsToString (Set.toList $ cageCells cage)
    
    operationToChar Add = "+"
    operationToChar Subtract = "-"
    operationToChar Multiply = "*"
    operationToChar Divide = "/"
    operationToChar None = "="
    
    positionsToString positions = 
      "[" ++ intercalate "," (map positionToString positions) ++ "]"
    
    positionToString (r, c) = "(" ++ show r ++ "," ++ show c ++ ")"

-- Helper function for intercalate
intercalate :: String -> [String] -> String
intercalate _ [] = ""
intercalate _ [x] = x
intercalate sep (x:xs) = x ++ sep ++ intercalate sep xs

-- Parse a saved game state (simple format)
parseGameState :: String -> Either String [(Position, Value)]
parseGameState input = 
  case parse gameStateParser "gamestate" input of
    Left err -> Left (errorBundlePretty err)
    Right moves -> Right moves

-- Game state parser
gameStateParser :: Parser [(Position, Value)]
gameStateParser = do
  _ <- space
  moves <- many moveParser
  _ <- space
  _ <- eof
  return moves

-- Parse a single move
moveParser :: Parser (Position, Value)
moveParser = do
  pos <- positionParser
  _ <- space
  _ <- char ':'
  _ <- space
  val <- L.decimal
  _ <- newline
  return (pos, val)

-- Generate game state string
generateGameState :: Grid -> String
generateGameState grid = unlines
  [ "(" ++ show r ++ "," ++ show c ++ "): " ++ show v |
    ((r, c), Just v) <- assocs grid ]