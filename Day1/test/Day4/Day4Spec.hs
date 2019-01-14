{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Day4.Day4Spec (
    spec
    ) 
where

import           Control.Applicative
import           Control.Lens

import qualified Data.Map                      as Map
import           Data.Modular
import           Data.Attoparsec.Text          
import           Data.Time.Lens
import           Data.Range.Range
import           Day4.Input (puzzleData)

import Data.String.Here (here)
import qualified Data.Text as T
import Data.Text (pack)
import Day4.Puzzle
import SpecHelper


spec :: Spec
spec = describe "Something" $ do
  let 
    expectedDate = baseDate
    mkRange :: (FlexDateTime -> FlexDateTime) -> (FlexDateTime -> FlexDateTime) -> SleepRange
    mkRange start end = SleepRange $ SpanRange (baseOffset $ baseDate & flexDT %~ start) (baseOffset $ baseDate & flexDT %~ end)
    mkDate f = baseDate & flexDT %~ f
    simpleShift = [here|
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
|]
  context "puzzle 1 " $ do
    let  exampleShift = [SleepRange $ SpanRange (5*60) (25*60)]
    context "sleep per shift" $
      it "should 5..25 -> 19 (not 20)" $ asleepFor exampleShift `shouldBe` 19
    context "minutes per shift" $
      it "should 5..25 ->  " $ asleepMinutes (Prelude.head exampleShift) `shouldBe` ([5..24] :: [Int/60]) 
    context "counts" $ do
      it "should 5..6 -> [(5,1)]" $ counts [5 :: ℤ/60] `shouldBe` Map.fromList [(5,1)]
      it "should [5,6,5] -> [(5,2), (6,1)]" $ counts [5 :: ℤ/60, 6, 5] `shouldBe` Map.fromList [(5,2), (6, 1)]

    context "solve simple" $ do
      let simpleShifts = [Shift baseDate 10 [mkRange (minutes +~5) (minutes +~25) , mkRange (minutes +~5) (minutes +~6)]]
      it "should have have solution of 5" $ solution simpleShifts `shouldBe` 50
      context "minutesForGuard" $ 
        allSamplesShouldBe (\(a,c,b) -> Map.lookup b $ minutesForGuard (toSolveable a) c)
          [Raw (simpleShifts, 10, 5) (Just 2)
          ,Raw (simpleShifts, 10, 1) Nothing]

    context "range length" $
      allSamplesShouldBe finiteLength 
        [ Raw (SingletonRange 10) 0
        , Raw (SpanRange 10 20) 10]
    context "actual puzzle" $ do
      it "should ahve a solution (FOR REAL)" $
         solution <$> parseOnly parseInputB puzzleData `shouldBe` Right 125444
      -- it "should have a solution 2 (errorCall" $
      --    evaluate (solution2 <$> parseOnly parseInputB puzzleData) `shouldThrow` anyErrorCall
      it "should have a solution 2" $
         solution2b <$> parseOnly parseInputB puzzleData `shouldBe` Right (Just (25 :: Int/60, 733))

      

  context "parsing" $ do
    context "datetime" $ 
      it (show expectedDate) $ parseOnly timestampP "[1518-11-01 00:00]" `shouldBe` Right expectedDate
    context "wake event" $ 
      allSamplesShouldBe (parseOnly wakeEventP) 
        [ Raw (pack "Guard #10 begins shift") (Right (BeginsShift 10))
        , Raw (pack "wakes up") ( Right WakesUp)
        , Raw (pack "falls asleep") (Right FallsAsleep)]
    context "shift" $ do

      it "should parse" 
        $ parseOnly (shiftP mkSleep) simpleShift 
        `shouldBe` Right (Shift baseDate 10 [SleepRange $ SpanRange (5*60) (25*60)])
      it "should parseB" 
        $ parseOnly (many (eventP wakeEventP)) simpleShift 
        `shouldBe` Right 
          [ At expectedDate (BeginsShift 10)
          , At (mkDate $ minutes +~5) FallsAsleep
          , At (mkDate $ minutes +~25) WakesUp ]
    context "show" $ 
      context "shift" $ 
        it "should show"
          $ (show <$> parseOnly (shiftP mkSleep ) simpleShift) 
          `shouldBe` Right (T.unpack simpleShift ++ "\n")

  context "example" $ do
    let 
      parsedExample = parseOnly parseInput example
      parsedBExample = parseOnly parseInputB example
      example = [here|
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up 
|]

    context "parsing" $ do
      it "should start with" $
        parsedExample `shouldBe` Right 
          [ Shift baseDate 10 [ mkRange (minutes +~5 ) (minutes +~25)
                     , mkRange (minutes +~30) (minutes +~55) ]
          , Shift (mkDate ((days +~1).(minutes -~2))) 99 [ mkRange ((days +~1).(minutes +~40)) ((days +~1).(minutes +~50))]
          , Shift (mkDate ((days +~2).(minutes +~5))) 10 [ mkRange ((days +~2).(minutes +~24)) ((days +~2).(minutes +~29))]
          , Shift (mkDate ((days +~3).(minutes +~2))) 99 [ mkRange ((days +~3).(minutes +~36)) ((days +~3).(minutes +~46))]
          , Shift (mkDate ((days +~4).(minutes +~3))) 99 [ mkRange ((days +~4).(minutes +~45)) ((days +~4).(minutes +~55))]
          ]
      it "should sort input" $ 
        parsedBExample `shouldBe` parsedExample
      it "should have solution" $
        solution <$> parsedExample `shouldBe` Right 240
      it "should have solution (parseB)" $
        solution <$> parsedBExample `shouldBe` solution <$> parsedExample
      it "should have solution 2b (parseB)" $
        solution2b <$> parsedBExample `shouldBe` Right (Just (45 :: Int/60, 99))
