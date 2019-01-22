{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day6.Day6Spec (spec) where
-- import Data.Set (Set) 

import Control.Lens
import qualified Data.List.NonEmpty as NE
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Map as M
import qualified Data.Set as S
import SpecHelper
import Day6.Puzzle
import Day6.Input
import Debug.Trace

-- shouldBeSet :: (Show a, Ord a) => [a] -> [a] -> IO ()
-- shouldBeSet a b = S.fromList a `shouldBe` S.fromList b
spec :: Spec
spec = describe "Day6" $
  context "Puzzle 1" $ do
    let             
        mkPuzzle :: [XY] -> Puzzle
        mkPuzzle = Puzzle . NE.fromList
        dim5 = (0,4)

    context "Display" $ do
        let 
            puzzle3 = [(0,0),(1,1),(4,4)]
            puzzle40 = [(0,0), (20,20), (39,39)]
        it "should have bounds" $ 
           boundsBorders (Bounds dim5 dim5 ()) `shouldBe` [(0,0),(0,1),(0,2),(0,3),(0,4),(4,0),(4,1),(4,2),(4,3),(4,4),(0,0),(1,0),(2,0),(3,0),(4,0),(0,4),(1,4),(2,4),(3,4),(4,4)]
        it "should display an empty bounded box" $
            displayPixels (Bounds dim5 dim5 ())  `shouldBe` [here|
.....
.....
.....
.....
.....
|]
        it "should display a single 'A' " $
            displayPixels (Bounds dim5 dim5 (M.fromList [((1,1)::XY, 'A')])) `shouldBe` [here|
.....
.A...
.....
.....
.....
|]

        it "step2 should display a single 'A'" $
            displayPixels (Bounds dim5 dim5 (step2 $ NE.fromList [(1,1)])) `shouldBe` [here|
.....
.A...
.....
.....
.....
|]


        it "dumb solution should display claims" $
            (displayPixels.mkLayer3b.mkLayer2b $  mkPuzzle puzzle3) `shouldBe` [here|
A····
·Bbb·
·bb·c
·b·cc
··ccC
|]
        it "should solve a simple simulation" $
             (solveB.mkPuzzle $ puzzle3) `shouldBe` ('b', 6)
        it "should remove edge elements" $
             (nonSolutions.mkLayer3b.mkLayer2b.mkPuzzle $ puzzle3) `shouldBe` S.fromList ['a','c']

    context "OnlyScore" $
        it "should map score zero to 0" $
           pixel (OnlyScore 0 $ Right 6) `shouldBe` '0'
    context "Star" $ do
        context "Input1" $ do
            let input = parseOnly puzzleP puzzleData
            it "should get parsed (begin)" $ (NE.take 3 . view puzzleXYs <$> input) `shouldBe` Right [(118,274),(102,101),(216,203)]
            it "should get parsed (end)" $ (NE.take 3 . NE.reverse . view puzzleXYs <$> input) `shouldBe` Right [(268,94),(229,97),(164,206)]
            it "should have an answer" $ (solveB) <$> input `shouldBe` Right ('b', 3251)

        context "Input2" $ do
            let input2 = parseOnly puzzleP puzzleData2
            it "should display" $ head.lines.displayPixels.mkLayer3b.mkLayer2b <$> input2 `shouldBe` Right [here|
Bbbbbbbaaaaaaaaaaaaaeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee
|]


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
Puzzle {_puzzleXYs = (1,1) :| [(1,6),(8,3),(3,4),(5,5),(8,9)]}
|]
        