{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
module Day4.Day4Spec (
    spec
    ) 
where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Data.Maybe                     ( catMaybes, listToMaybe ) 

import           Data.Functor
import           Data.Foldable      
import           Data.Map                      (Map, (!))
import qualified Data.Map                      as Map
import           Data.Modular
import           Data.Monoid
import           Data.Attoparsec.Text          as P
import           Data.List                     (sortBy)
import           Data.Time                     (UTCTime, UTCTime(..),Day, Day(..), DiffTime, fromGregorian, hoursToTimeZone)
import           Data.Time.Clock               (secondsToDiffTime, diffUTCTime, NominalDiffTime)
import           Data.Time.Lens
import           Data.Fixed      
import           Data.Range.Range
import           Data.Range.Algebra
import Debug.Trace
import  Day4.Input (puzzleData)

import Data.String.Here
import Data.Text as T
import Data.Text (pack)
import SpecHelper

type EventTime = UTCTime


data WakeEvent a = BeginsShift a | WakesUp | FallsAsleep
  deriving (Eq, Show)

data Event a = At EventTime a
  deriving (Eq, Show, Ord)

eventAt :: Event a -> EventTime
eventAt (At t _)= t 

eventPayload :: Event a -> a
eventPayload (At _ a)= a

type Guard = Integer

type GuardEvent = Event (WakeEvent Guard)

type SleepRange = Range NominalDiffTime

type Min60 = Mod Int 60

data Shift a = Shift Guard a
  deriving (Eq,Show, Functor)

-- Puzzle 1
finiteRangeLength :: Num a => Range a -> a
finiteRangeLength (SpanRange a b) = abs (b- a)
finiteRangeLength (SingletonRange _) = 0
finiteRangeLength _ = 0

toMinutes :: NominalDiffTime -> Int 
toMinutes t = floor $ toRational t/60

asleepFor :: [SleepRange] -> Int
asleepFor sleep = sum $ pred . toMinutes . finiteRangeLength <$> sleep


asleepMinutes :: SleepRange -> [Min60]
asleepMinutes (SpanRange a b) = toMod <$> fromRanges [SpanRange (toMinutes a) (toMinutes b - 1)]
asleepMinutes _ = []

countMinutes :: [Min60] -> Map Min60 Int
countMinutes ms = build $ project <$> ms
  where
    project = (,1)
    build = Map.fromListWith (+)

minutesForGuard :: Map Guard [SleepRange] -> Guard -> Map Min60 Int
minutesForGuard base g = countMinutes $ foldMap asleepMinutes $ base ! g

toSolvable :: [Shift [SleepRange]] -> Map Guard [SleepRange]
toSolvable ss = build $ project <$> ss
  where 
    project s@(Shift g rs) = (g, rs)
    build = Map.fromListWith (++)

sndOrdering :: Ord b => (a, b) -> (a, b) -> Ordering
sndOrdering (_,a) (_, b) = compare a b

findMaxValue :: Ord b => Map a b -> a
findMaxValue m = fst . maximumBy sndOrdering $ Map.toList m

solution :: [Shift [SleepRange]] -> Int
solution ss = (fromInteger findSleepiestGuard) * (unMod $ findSleepiestMinute findSleepiestGuard)
  where
    findSleepiestGuard = findMaxValue $ asleepFor <$> base
    findSleepiestMinute g = findMaxValue $ minutesForGuard base g 
    base = toSolvable ss


-- Parsing
timestampP :: Parser EventTime
timestampP = do
  yyyy <- "[" *> decimal
  mm <- "-" *> decimal
  dd <- "-" *> decimal
  hh <- " " *> decimal
  mins <- ":" *> decimal
  _ <- string "]"
  return $ UTCTime (fromGregorian yyyy mm dd) (fromInteger $ sum [hh*24, mins*60]) 

eventP :: Parser a -> Parser (Event a)
eventP aP = At <$> timestampP <*> aP <* skipSpace

wakeEventP :: Parser (WakeEvent Guard)
wakeEventP =  
  skipSpace *> choice 
    [ BeginsShift <$> ("Guard #" *> decimal <* " begins shift")
    , string "wakes up" $> WakesUp
    , string "falls asleep" $> FallsAsleep]

shiftStream :: (EventTime -> NominalDiffTime) -> [GuardEvent] -> [Shift [SleepRange]]
shiftStream offset es = go es []
  where
    go :: [GuardEvent] -> [Shift [SleepRange]] -> [Shift [SleepRange]]
    go (At _ (BeginsShift g) : rest) ss = go rest (Shift g [] : ss)
    go es@(At t1 FallsAsleep : At t2 WakesUp : rest) ss@(shift:shifts) = go rest (update t1 t2 shift : shifts)
    go _ ss = ss
    update t1 t2 (Shift g rs) = traceShowId (Shift g (SpanRange (offset t1) (offset t2):rs))

eventTimestampCompare :: Event a -> Event a -> Ordering
eventTimestampCompare (At t1 _) (At t2 _ )= compare t1 t2 

shiftP :: (EventTime -> NominalDiffTime) -> Parser (Shift [SleepRange])
shiftP offset = do
   start <- guardP
   sleep <- many sleepRangeP
   return $ Shift (eventPayload start) (mergeRanges sleep)

  where 
    guardP :: Parser (Event Guard)
    guardP = eventP (" Guard #" *> decimal <* " begins shift" <* endOfLine)
    sleepRangeP :: Parser SleepRange
    sleepRangeP = SpanRange 
      <$> ( offset . eventAt <$> eventP (string " falls asleep") <* skipSpace)
      <*> ( offset . eventAt  <$> eventP (string " wakes up") <* skipSpace)
      

parseInput :: Day -> Parser [Shift [SleepRange]]
parseInput day = many (shiftP offset)
  where
    relativeTo :: UTCTime
    relativeTo = UTCTime day 0
    offset = flip diffUTCTime relativeTo

parseInputB:: Day -> Parser [Shift [SleepRange]]
parseInputB day = shiftStream offset . sortBy eventTimestampCompare <$> many (eventP wakeEventP)
  where
    relativeTo :: UTCTime
    relativeTo = UTCTime day 0
    offset = flip diffUTCTime relativeTo



parseLine :: Parser GuardEvent
parseLine = eventP (skipSpace *> wakeEventP) <* (endOfLine <|> endOfInput)

spec :: Spec
spec = describe "Something" $ do
  let 
    baseDate = UTCTime (fromGregorian 1518 11 01)  0
    expectedDate = baseDate
    expectedBegin = BeginsShift 10
    mkRange :: (FlexDateTime -> FlexDateTime) -> (FlexDateTime -> FlexDateTime) -> SleepRange
    mkRange start end = SpanRange (baseOffset $ baseDate & flexDT %~ start) (baseOffset $ baseDate & flexDT %~ end)
    mkDate f = baseDate & flexDT %~ f
    baseOffset = flip diffUTCTime baseDate

  context "puzzle 1 " $ do
    let  exampleShift = [SpanRange (5*60) (25*60)]
    context "sleep per shift" $
      it "should 5..25 -> 19 (not 20)" $ asleepFor exampleShift `shouldBe` 19
    context "minutes per shift" $
      it "should 5..25 ->  " $ asleepMinutes (Prelude.head exampleShift) `shouldBe` (toMod <$> [5..24]) 
    context "countMinutes" $ do
      it "should 5..6 -> [(5,1)]" $ countMinutes [toMod 5] `shouldBe` Map.fromList [(toMod 5,1)]
      it "should [5,6,5] -> [(5,2), (6,1)]" $ countMinutes [toMod 5, toMod 6, toMod 5] `shouldBe` Map.fromList [(toMod 5,2), (toMod 6, 1)]

    context "solve simple" $ do
      let simpleShifts = [Shift 10 [mkRange (minutes +~5) (minutes +~25) , mkRange (minutes +~5) (minutes +~6)]]
      it "should have have solution of 5" $ solution simpleShifts `shouldBe` 50
      context "minutesForGuard" $ 
        allSamplesShouldBe (\(a,c,b) -> Map.lookup b $ minutesForGuard (toSolvable a) c)
          [Raw (simpleShifts, 10, 5) (Just 2)
          ,Raw (simpleShifts, 10, 1) Nothing]

    context "range length" $
      allSamplesShouldBe finiteRangeLength 
        [ Raw (SingletonRange 10) 0
        , Raw (SpanRange 10 20) 10]
    context "actual puzzle" $
      it "should ahve a solution (FOR REAL)" $
       pending
       -- Prelude.take 5 <$> parseOnly (parseInputB (fromGregorian 1518 03 28)) puzzleData `shouldBe` Right []

      

  context "parsing" $ do
    context "datetime" $ 
      it (show expectedDate) $ parseOnly timestampP "[1518-11-01 00:00]" `shouldBe` Right expectedDate
    context "wake event" $ 
      allSamplesShouldBe (parseOnly wakeEventP) 
        [ Raw (pack "Guard #10 begins shift") (Right (BeginsShift 10))
        , Raw (pack "wakes up") ( Right WakesUp)
        , Raw (pack "falls asleep") (Right FallsAsleep)]
    context "shift" $ do
      let simpleShift = [here|
[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
|]
      it "should parse" 
        $ parseOnly (shiftP baseOffset) simpleShift 
        `shouldBe` Right (Shift 10 [SpanRange (5*60) (25*60)])
      it "should parseB" 
        $ parseOnly (many (eventP wakeEventP)) simpleShift 
        `shouldBe` Right 
          [ At expectedDate (BeginsShift 10)
          , At (mkDate $ minutes +~5) FallsAsleep
          , At (mkDate $ minutes +~25) WakesUp ]

  context "example" $ do
    let 
      parsedExample = parseOnly (parseInput (fromGregorian 1518 11 01)) example
      parsedBExample = parseOnly (parseInputB (fromGregorian 1518 11 01)) example
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
          [ Shift 10 [ mkRange (minutes +~5 ) (minutes +~25)
                     , mkRange (minutes +~30) (minutes +~55) ]
          , Shift 99 [ mkRange ((days +~1).(minutes +~40)) ((days +~1).(minutes +~50))]
          , Shift 10 [ mkRange ((days +~2).(minutes +~24)) ((days +~2).(minutes +~29))]
          , Shift 99 [ mkRange ((days +~3).(minutes +~36)) ((days +~3).(minutes +~46))]
          , Shift 99 [ mkRange ((days +~4).(minutes +~45)) ((days +~4).(minutes +~55))]
          ]
      it "should sort input" $ 
        parsedBExample `shouldBe` Right (
          Prelude.reverse [ Shift 10 [ mkRange (minutes +~30) (minutes +~55)
                                     , mkRange (minutes +~5 ) (minutes +~25)
                                      ]
          , Shift 99 [ mkRange ((days +~1).(minutes +~40)) ((days +~1).(minutes +~50))]
          , Shift 10 [ mkRange ((days +~2).(minutes +~24)) ((days +~2).(minutes +~29))]
          , Shift 99 [ mkRange ((days +~3).(minutes +~36)) ((days +~3).(minutes +~46))]
          , Shift 99 [ mkRange ((days +~4).(minutes +~45)) ((days +~4).(minutes +~55))]
          ]
        )
      it "should have solution" $
        solution <$> parsedExample `shouldBe` Right 240
      it "should have solution (B)" $
        solution <$> parsedBExample `shouldBe` solution <$> parsedExample
