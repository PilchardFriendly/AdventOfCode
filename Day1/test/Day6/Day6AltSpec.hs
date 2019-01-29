{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Day6.Day6AltSpec (spec) where
import Data.Attoparsec.Text

import           Control.Applicative ( (<|>))
import Control.Arrow ((&&&))

import Linear (V2(..), _x, _y)
import qualified Data.Ix as Ix
import Data.Semigroup (Min(..), Max(..))
import Data.Semigroup.Foldable (foldMap1, Foldable1(..))
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import Day6.MinMax
import Day6.Input (puzzleData)
import Counting
import Maxing
import SpecHelper

type XY = V2 Integer

distance :: XY -> XY -> Integer
distance a b = sum . abs $ a - b

type Box = V2 XY

mkBox ::  Foldable1 t => t XY -> Box
mkBox las = V2 (V2 x1 y1) (V2 x2 y2)
   where 
     ((Min x1, Max x2), (Min y1, Max y2)) = foldMap1 (minMax.view _x &&& minMax.view _y) las

boxBorderRange :: Box -> [XY]
boxBorderRange (V2 (V2 x1 y1) (V2 x2 y2)) = concat
   [[V2 x y | x <- [x1,x2], y <-[y1 .. y2]]
   ,[V2 x y | y <- [y1,y2], x <-[x1+1 .. x2-1]]]

boxShrink :: Box -> Box
boxShrink (V2 a b) = V2 (a+1) (b-1)

boxRange :: Box -> [XY]
boxRange (V2 a b) = Ix.range (a,b)

labelXY :: NonEmpty XY -> XY -> Maybe XY
labelXY pts p = do
    (closest, _) :| [] <- Just    -- This pattern will match on a single item in the shortest distane group.
                        . NE.head 
                        . NE.groupWith1 snd 
                        . NE.sortWith snd 
                        $ distances
    pure $ closest
    where
        distances = pts <&> (id &&& distance p)
        
regionScoreXY :: NonEmpty XY -> XY -> Integer
regionScoreXY pts p = sum $ (distance p) <$>  pts

scoreXYs :: NonEmpty XY -> [XY] -> Map XY Int
scoreXYs pts = counts.mapMaybe (labelXY pts)

solution :: NonEmpty XY -> Map XY Int
solution pts = M.difference insides outsides
  where
    box = mkBox pts
    outsides = score . boxBorderRange $ box
    insides = score . boxRange . boxShrink $ box
    score :: [XY] -> Map XY Int
    score = scoreXYs pts

solve :: NonEmpty XY -> Maybe (XY, Int)
solve = findMaxValueSnd . solution

solution2 :: Integer -> NonEmpty XY -> [XY]
solution2 n pts = fmap fst . filter (\x -> snd x < n) $ scores box
  where
    box = mkBox pts
    scores = fmap (id &&& regionScoreXY pts) . boxRange

solve2 :: Integer -> NonEmpty XY -> Maybe Int
solve2 n = Just . length . solution2 n



v2P :: Parser XY
v2P =
    V2
        <$> (decimal <* skipSpace)
        <*> (string "," *> skipSpace *> decimal <* (endOfLine <|> endOfInput))

puzzleP :: Parser (NonEmpty XY)
puzzleP = NE.fromList <$> many1' v2P

spec :: Spec
spec = describe "Day6 Alternative" $ do
  let 
    example = uncurry V2 <$> (1,1) :| [(1,6),(8,3),(3,4),(5,5),(8,9)]
    origin = V2 0 0
    twoPts = (V2 0 1) :| (V2 5 6) : []
    twoPtBox = mkBox twoPts
    thr3Pts = (V2 0 1) :| (V2 2 3) : []
    thr3By3Box = mkBox thr3Pts
    singlePt = origin :| []
    singlePtBox = mkBox singlePt
  context "Box" $ do
    context "mkBox" $ do
       it "should work for a single point" $ singlePtBox `shouldBe` origin `V2` origin
       it "should work for two points" $ twoPtBox `shouldBe` (V2 0 1) `V2` (V2 5 6)
    context "boxRange" $ do
       it "should workd for two points" $ boxRange twoPtBox `shouldBe` [V2 x y | x <- [0..5], y <- [1..6]]
    context "shrinkBox" $ do
       it "should work for two points" $ (boxShrink) twoPtBox `shouldBe` (V2 1 2) `V2` (V2 4 5)
    context "boxBorderRange" $ do
       it "should work for 3x3" $ (S.fromList . boxBorderRange) thr3By3Box `shouldBe` S.fromList [V2 x y | x <- [0..2], y <- [1..3], not (x == 1 && y == 2)]
  context "XY" $ do
      context "distance" $ do
        it "between same points should be 0" $ distance origin origin `shouldBe` 0
        it "between same x points should delta y" $ distance (V2 10 0) (V2 10 10) `shouldBe` 10
        it "between same y points should delta x" $ distance (V2 0 10) (V2 10 10) `shouldBe` 10
        it "between delta (2,3) should be 5" $ (uncurry distance . \v -> (v, v + V2 2 3)) origin `shouldBe` 5 
  context "labelXY" $ do
    context ("from " ++ show singlePt) $ do
      it "of origin should be the origin" $ (labelXY singlePt origin) `shouldBe` Just origin
      it "of any point should be the origin" $ (labelXY singlePt (V2 1 1)) `shouldBe` Just origin
    context ("from " ++ show twoPts) $ do
      it "of origin should be the (0,1)" $ (labelXY twoPts) origin `shouldBe` Just (V2 0 1)
      it "of (3,4) should be the (5,6)" $ (labelXY twoPts) (V2 3 4) `shouldBe` Just (V2 5 6)
    context ("from " ++ show thr3Pts) $ do
        it "of origin should be  (0,1)" $ (labelXY thr3Pts) origin `shouldBe` Just (V2 0 1)
        it "of (1,2) should be Nothing" $ (labelXY thr3Pts) (V2 1 2) `shouldBe` Nothing
        it "check distance (1,2) of all points should be 2" $ (distance (V2 1 2) <$> thr3Pts) `shouldBe` 2 :| 2 : []
  context "scoreXYs" $ do
    context "3x3" $ do
      it "should be look like:" $ (scoreXYs thr3Pts) (boxRange thr3By3Box) `shouldBe` M.fromList [(V2 0 1,3),(V2 2 3,3)]
  context "solution 2" $ do
    context "for" $ do
      context (show example) $ do
        it "should be 33" $ (solve2 32) example `shouldBe` Just 16
        
      context "real thing" $ do
        let subject = parseOnly puzzleP puzzleData
        it "should be ??" $ (solve2 10000) <$> subject `shouldBe` Right (Just 47841)
  
  context "solution" $ do
    context "for" $ do
      context (show example) $ do
        it "should be" $ (solution) example `shouldBe` M.fromList [(V2 3 4,9),(V2 5 5,17)]
        it "solved should be" $ (solve) example `shouldBe` Just ((V2 5 5), 17)
      context (show twoPts) $ do
        it "should be empty" $ (solution) twoPts `shouldBe` M.empty
      context (show thr3Pts) $ do
        it "should be empty" $ (solution) thr3Pts `shouldBe` M.empty
      context (show $ singlePt <> thr3Pts) $ do
        it "should be empty" $ (solution) (singlePt <> thr3Pts) `shouldBe` M.empty
      context "real thing" $ do
        context "box" $ do
          let subject = mkBox <$> (parseOnly puzzleP puzzleData)
          it "should be (41,46)->(349,359)" $ subject `shouldBe` Right (V2 41 46 `V2` V2 349 359)
          it "should have border range" $ (length.boxBorderRange) <$> subject `shouldBe` Right (2 * (308 + 313))
        it "should be...(V2 102 101,3251)" $ solve <$> (parseOnly puzzleP puzzleData) `shouldBe` Right (Just (V2 102 101,3251))

      
