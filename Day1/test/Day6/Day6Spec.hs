{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day6.Day6Spec (spec) where
-- import Data.Set (Set) 
-- import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import SpecHelper
import Day6.Puzzle

-- shouldBeSet :: (Show a, Ord a) => [a] -> [a] -> IO ()
-- shouldBeSet a b = S.fromList a `shouldBe` S.fromList b
spec :: Spec
spec = describe "Day6" $
  context "Puzzle 1" $ do
    context "Layer 1" $ do
      let
        start = layer1 $ NE.fromList [(1,1),(3,3),(5,5)] 
        expectedStart = Layer1 [Starting (Start (1,1) 'a')
                        ,Starting (Start (3,3) 'b')
                        ,Starting (Start (5,5) 'c')] M.empty
      it "should have empty scores to start" $ 
            start `shouldBe` expectedStart
      it "should have a next step with scores" $
            displayPixels (Bounds (1,5) (1,5) (nextSteps start)) `shouldBe` [here|
0....
.....
..0..
.....
....0           |]
      it "should have a next step with scores" $
          displayPixels (Bounds (1,5) (1,5) (concatMap nextSteps (nextSteps start))) `shouldBe` [here|
01...
1.1..
.101.
..1.1
...10           |]
    
                
    context "Display" $ do
        it "should display an empty bounded box" $
            displayPixels (Bounds (0,4) (0,4) ())  `shouldBe` [here|
.....
.....
.....
.....
.....
|]
        it "should display a single 'A' " $
            displayPixels (Bounds (0,4) (0,4) (M.fromList [((1,1)::XY, 'A')])) `shouldBe` [here|
.....
.A...
.....
.....
.....
|]

        it "step2 should display a single 'A'" $
            displayPixels (Bounds (0,4) (0,4) (step2 $ NE.fromList [(1,1)])) `shouldBe` [here|
.....
.A...
.....
.....
.....
|]
        it "step3 should display a single 'A'" $
            displayPixels (Bounds (0,4) (0,4) (step3.step2 $ NE.fromList [(1,1)])) `shouldBe` [here|
.....
.A...
.....
.....
.....
|]

        it "step4 should display some 2nd gen 'a's" $
          displayPixels (Bounds (0,4) (0,4) (step4.step3.step2 $ NE.fromList [(1,1)])) `shouldBe` [here|
.a...
a.a..
.a...
.....
.....
|]
        it "step4 should display some 3rd gen 'a's" $
          displayPixels (Bounds (0,4) (0,4) (step4.step4.step3.step2 $ NE.fromList [(1,1)])) `shouldBe` [here|
a.a..
...a.
a.a..
.a...
.....
|]

        it "simple simulation should display distances" $
            displayPixels (simulate.mkLayer2 $  Puzzle $ NE.fromList [(0,0),(1,1),(4,4)]) `shouldBe` [here|
01234
10123
21232
32321
43210
|]
        
        it "simple simulation should display claims" $
            displayPixels (mkLayer3.simulate.mkLayer2 $  Puzzle $ NE.fromList [(0,0),(1,1),(4,4)]) `shouldBe` [here|
A····
·Bbb·
·bb·c
·b·cc
··ccC
|]

    -- context "Moves" $ do
        -- it "0 should be identity" $ map ($ (0::Int,0)) (moves 0) `shouldBeSet` [(0,0)]
        -- it "1 should be 1 steps"  $ map ($ (0::Int,0)) (moves 1) `shouldBeSet` [(0,1),(0,-1), (1, 0),(-1,0)]
        -- it "2 should be 2 steps"  $ map ($ (0::Int,0)) (moves 2) `shouldBeSet` [(0,2),(0,-2), (2, 0),(-2,0), (1,1), (1,-1), (-1,-1), (-1,1)]
--     context "Pixels" $ do
--         it "should display empty space" $ displayPixels (Bounds (Coords (0,0) ()) (Coords (5,5) ()) ()) `shouldBe` [here|
           
--         |]
    context "OnlyScore" $
        it "should map score zero to 0" $
           pixel (OnlyScore 0 $ Right 6) `shouldBe` '0'
    context "Example" $ do
      let 
        example1Input = [here|
1,1
1,6
8,3
3,4
5,5
8,9
        |]
      it "should look like" $ show <$> (parseOnly puzzleP example1Input) `shouldBe` Right [here| 
Puzzle ((1,1) :| [(1,6),(8,3),(3,4),(5,5),(8,9)])
|]
        