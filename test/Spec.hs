import Test.Hspec
import KenKen
import KenKen.Types (Difficulty(..), puzzleSize, puzzleCages, getCellValue)
import KenKen.Generator (generatePuzzle)
import KenKen.Solver (solvePuzzle)
import qualified Data.Set as Set

main :: IO ()
main = hspec $ do
  describe "Grid operations" $ do
    it "creates empty grid of correct size" $ do
      let grid = emptyGrid 4
      gridSize grid `shouldBe` 4
      filledCells grid `shouldBe` 0
    
    it "sets and gets cell values" $ do
      let grid = emptyGrid 4
          grid' = setCellValue grid (1,1) (Just 3)
      getCellValue grid' (1,1) `shouldBe` Just 3
    
    it "checks valid positions" $ do
      isValidPosition 4 (1,1) `shouldBe` True
      isValidPosition 4 (0,1) `shouldBe` False
      isValidPosition 4 (5,1) `shouldBe` False
  
  describe "Cage operations" $ do
    it "evaluates addition cage correctly" $ do
      let cage = Cage 1 Add 5 (Set.fromList [(1,1), (1,2)])
          grid = setCellValue (setCellValue (emptyGrid 4) (1,1) (Just 2)) (1,2) (Just 3)
      evaluateCage grid cage `shouldBe` True
    
    it "evaluates subtraction cage correctly" $ do
      let cage = Cage 2 Subtract 1 (Set.fromList [(1,3), (1,4)])
          grid = setCellValue (setCellValue (emptyGrid 4) (1,3) (Just 3)) (1,4) (Just 2)
      evaluateCage grid cage `shouldBe` True
    
    it "finds cage by position" $ do
      let cages = [Cage 1 Add 5 (Set.fromList [(1,1), (1,2)])]
      case findCageByPosition cages (1,1) of
        Just cage -> cageId cage `shouldBe` 1
        Nothing -> error "Cage not found"

    it "evaluates complex multiplication constraint correctly" $ do
      let cage = Cage 3 Multiply 12 (Set.fromList [(2,1), (2,2), (3,1)])
          grid1 = setCellValue (setCellValue (setCellValue (emptyGrid 4) (2,1) (Just 3)) (2,2) (Just 4)) (3,1) (Just 2)
      evaluateCage grid1 cage `shouldBe` False
      let grid2 = setCellValue grid1 (3,1) (Just 1)
      evaluateCage grid2 cage `shouldBe` True

    it "evaluates division constraint correctly independent of position order" $ do
      let cage = Cage 4 Divide 2 (Set.fromList [(3,3), (3,4)])
          grid1 = setCellValue (setCellValue (emptyGrid 4) (3,3) (Just 4)) (3,4) (Just 2)
      evaluateCage grid1 cage `shouldBe` True
      let grid2 = setCellValue (setCellValue (emptyGrid 4) (3,3) (Just 2)) (3,4) (Just 4)
      evaluateCage grid2 cage `shouldBe` True
  
  describe "Validation" $ do
    it "validates row constraint" $ do
      let grid = setCellValue (emptyGrid 4) (1,1) (Just 2)
      canPlaceValue grid (1,2) 2 `shouldBe` False
      canPlaceValue grid (1,2) 3 `shouldBe` True
    
    it "validates column constraint" $ do
      let grid = setCellValue (emptyGrid 4) (1,1) (Just 2)
      canPlaceValue grid (2,1) 2 `shouldBe` False
      canPlaceValue grid (2,1) 3 `shouldBe` True
    
    it "detects completed puzzle" $ do
      let puzzle = samplePuzzle4x4
          gs = initGame puzzle
      isPuzzleSolved gs `shouldBe` False
  
  describe "Parser" $ do
    it "parses puzzle file correctly" $ do
      let input = "size 4\ncage 1 + 5 [(1,1),(1,2)]\n"
      case parsePuzzleFile input of
        Right puzzle -> puzzleSize puzzle `shouldBe` 4
        Left err -> error err
    
    it "generates puzzle file correctly" $ do
      let cage = Cage 1 Add 5 (Set.fromList [(1,1), (1,2)])
          puzzle = Puzzle 4 [cage] Nothing
          output = generatePuzzleFile puzzle
      output `shouldContain` "size 4"
      output `shouldContain` "cage 1 + 5"

    it "fails on invalid puzzle formatting" $ do
      let input = "size 4\ncage 1 ? 5 [(1,1),(1,2)]\n"
      case parsePuzzleFile input of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "Should have failed to parse invalid operation"

  describe "Generator and Solver" $ do
    it "generates a valid 4x4 Easy puzzle that the solver resolves" $ do
      puzzle <- generatePuzzle 4 Easy
      puzzleSize puzzle `shouldBe` 4
      length (puzzleCages puzzle) `shouldSatisfy` (> 0)
      let mSol = solvePuzzle puzzle
      case mSol of
        Just _ -> True `shouldBe` True
        Nothing -> expectationFailure "Solver could not find solution for generated puzzle"

    it "generates a valid 6x6 Medium puzzle" $ do
      puzzle <- generatePuzzle 6 Medium
      puzzleSize puzzle `shouldBe` 6
      length (puzzleCages puzzle) `shouldSatisfy` (> 0)

    it "correctly returns Nothing for unsolvable puzzles" $ do
      let cage1 = Cage 1 Add 20 (Set.fromList [(1,1), (1,2)])
          puzzle = Puzzle 4 [cage1] Nothing
      solvePuzzle puzzle `shouldBe` Nothing