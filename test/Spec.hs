import Test.Hspec
import KenKen
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
          -- Would need to fill in complete solution
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