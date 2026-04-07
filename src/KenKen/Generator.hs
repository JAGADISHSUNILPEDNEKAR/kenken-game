module KenKen.Generator (generatePuzzle) where

import KenKen.Types
import Data.Array
import Data.Set (Set)
import qualified Data.Set as Set
import System.Random
import Control.Monad (foldM)

-- Shuffle a list randomly
shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  idx <- randomRIO (0, length xs - 1)
  let (before, x:after) = splitAt idx xs
  rest <- shuffle (before ++ after)
  return (x : rest)

-- Latin square generation
type LGrid = Array Position Int

generateLatinSquare :: Size -> IO LGrid
generateLatinSquare n = do
  let emptyG = listArray ((1,1), (n,n)) (repeat 0)
  Just finalG <- solveLatin emptyG (getAllPositions n) n
  return finalG

solveLatin :: LGrid -> [Position] -> Size -> IO (Maybe LGrid)
solveLatin grid [] _ = return (Just grid)
solveLatin grid (pos@(r,c):pos') n = do
  let usedInRow = [grid ! (r, c') | c' <- [1..n], c' /= c, grid ! (r, c') /= 0]
      usedInCol = [grid ! (r', c) | r' <- [1..n], r' /= r, grid ! (r', c) /= 0]
      used = Set.fromList (usedInRow ++ usedInCol)
      validNums = [x | x <- [1..n], not (Set.member x used)]
  
  if null validNums
    then return Nothing
    else do
      shuffledNums <- shuffle validNums
      tryNums grid pos pos' n shuffledNums

tryNums :: LGrid -> Position -> [Position] -> Size -> [Int] -> IO (Maybe LGrid)
tryNums _ _ _ _ [] = return Nothing
tryNums grid pos pos' n (x:xs) = do
  let newGrid = grid // [(pos, x)]
  res <- solveLatin newGrid pos' n
  case res of
    Just finalG -> return (Just finalG)
    Nothing -> tryNums grid pos pos' n xs

-- Cage Generation
maxCageSize :: Difficulty -> Int
maxCageSize Easy = 2
maxCageSize Medium = 4
maxCageSize Hard = 5

randomPick :: [a] -> IO (Maybe a)
randomPick [] = return Nothing
randomPick xs = do
  idx <- randomRIO (0, length xs - 1)
  return $ Just (xs !! idx)

getNeighbors :: Position -> Size -> [Position]
getNeighbors (r, c) size = 
  filter (\(r', c') -> r' >= 1 && r' <= size && c' >= 1 && c' <= size)
    [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]

buildCage :: Set Position -> Position -> Size -> Int -> IO (Set Position)
buildCage unassigned current size targetSize = loop (Set.singleton current)
  where
    loop :: Set Position -> IO (Set Position)
    loop currentGroup = do
      if Set.size currentGroup >= targetSize
        then return currentGroup
        else do
          let neighbors = Set.fromList [ n | p <- Set.toList currentGroup
                                           , n <- getNeighbors p size
                                           , Set.member n unassigned
                                           , not (Set.member n currentGroup) ]
          if Set.null neighbors
            then return currentGroup
            else do
              nextPos <- randomPick (Set.toList neighbors)
              case nextPos of
                Just np -> loop (Set.insert np currentGroup)
                Nothing -> return currentGroup

partitionGrid :: Size -> Difficulty -> IO [Set Position]
partitionGrid size diff = do
  let allPos = Set.fromList (getAllPositions size)
      maxS = maxCageSize diff
  loop maxS allPos []
  where
    loop maxS unassigned acc = do
      if Set.null unassigned
        then return acc
        else do
          startPos <- randomPick (Set.toList unassigned)
          case startPos of
            Nothing -> return acc
            Just sp -> do
              targetSize <- randomRIO (1, maxS)
              newGroup <- buildCage unassigned sp size targetSize
              let nextUnassigned = Set.difference unassigned newGroup
              loop maxS nextUnassigned (newGroup : acc)

-- Determine operation and target for a cage
generateCage :: Int -> Set Position -> LGrid -> Difficulty -> IO Cage
generateCage cId cells grid diff = do
  let vals = map (\p -> grid ! p) (Set.toList cells)
      len = length vals
  (op, target) <- 
    if len == 1 
      then return (None, head vals)
      else if len == 2
        then do
          let [a, b] = vals
              mx = max a b
              mn = min a b
          let possibleOps = [Add, Multiply, Subtract] ++
                            [Divide | mx `mod` mn == 0]
          chosen <- randomPick possibleOps
          let finalOp = case chosen of 
                          Just o -> o
                          Nothing -> Add
          
          let targetVal = case finalOp of
                            Add -> a + b
                            Subtract -> mx - mn
                            Multiply -> a * b
                            Divide -> mx `div` mn
                            _ -> a + b
          return (finalOp, targetVal)
        else do
          let possibleOps = [Add, Multiply]
          chosen <- randomPick possibleOps
          let finalOp = case chosen of Just o -> o; Nothing -> Add
          let targetVal = case finalOp of
                            Add -> sum vals
                            Multiply -> product vals
                            _ -> sum vals
          return (finalOp, targetVal)
  return $ Cage cId op target cells

generatePuzzle :: Size -> Difficulty -> IO Puzzle
generatePuzzle size diff = do
  lGrid <- generateLatinSquare size
  groups <- partitionGrid size diff
  
  cages <- sequence $ zipWith (\idx g -> generateCage idx g lGrid diff) [1..] groups
  
  let sol = [[Just (lGrid ! (r, c)) | c <- [1..size]] | r <- [1..size]]
  
  return $ Puzzle size cages (Just sol)
