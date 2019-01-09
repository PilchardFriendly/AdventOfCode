{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Day4.Day4Spec (
    spec
    ) 
where

import           Data.Maybe                     ( catMaybes, listToMaybe )
import           Data.Foldable                     
import           Data.Attoparsec.Text          as P
import           Data.Time                     (LocalTime,LocalTime(..), TimeOfDay(..), fromGregorian)
import           Data.Fixed      


import Data.String.Here
import Data.Text as T
import Data.Text (pack)
import SpecHelper

type EventTime = LocalTime

data WakeEvent a = BeginsShift a | WakesUp | FallsAsleep
  deriving (Eq, Show)

data Event a = At EventTime a
  deriving (Eq, Show)

type Guard = Integer

type GuardEvent = Event (WakeEvent Guard)

type SleepRange a = (a, a)

type Shift a = (a, [SleepRange EventTime])

timestampP :: Parser EventTime
timestampP = do
  yyyy <- "[" *> decimal
  mm <- "-" *> decimal
  dd <- "-" *> decimal
  hh <- " " *> decimal
  mins <- ":" *> decimal
  _ <- string "]"
  return $ LocalTime (fromGregorian yyyy mm dd) (TimeOfDay hh mins 0) 

eventP :: Parser a -> Parser (Event a)
eventP aP = At <$> timestampP <*> aP

wakeEventP :: Parser (WakeEvent Guard)
wakeEventP =  
  choice 
    [ BeginsShift <$> ("Guard #" *> decimal <* " begins shift")
    , string "wakes up" *> pure WakesUp
    , string "falls asleep" *> pure FallsAsleep
    ]

parseInput :: Parser [GuardEvent]
parseInput = sepBy (eventP (skipSpace *> wakeEventP)) endOfLine

spec :: Spec
spec = describe "Something" $ do
  let expectedDate = LocalTime (fromGregorian 1518 11 01) ( TimeOfDay 00 00 (MkFixed 00))
  context "parsing" $ do
    context "datetime" $ 
      it (show expectedDate) $ parseOnly timestampP "[1518-11-01 00:00]" `shouldBe` Right expectedDate
    context "wake event" $ 
      allSamplesShouldBe (parseOnly wakeEventP) 
        [ Raw (pack "Guard #10 begins shift") (Right (BeginsShift 10))
        , Raw (pack "wakes up") ( Right WakesUp)
        , Raw (pack "falls asleep") (Right FallsAsleep)]
    context "input line" $
      it "should parse" 
        $ parseOnly parseInput "[1518-11-01 00:00] Guard #10 begins shift" 
        `shouldBe` Right [At expectedDate $ BeginsShift 10]

  context "example" $ do
    let 
      parsedExample = parseOnly parseInput example
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
[1518-11-05 00:55] wakes up|]

    context "parsing" $
      it "should start with" $
        listToMaybe <$> parsedExample `shouldBe` Right (Just (At expectedDate $ BeginsShift 10))
