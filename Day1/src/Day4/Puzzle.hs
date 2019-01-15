{- HLINT ignore "Unused LANGUAGE pragma" -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}

module Day4.Puzzle where

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Lens.TH

import           Data.Functor
import           Data.Function                  ( on )
import           Data.Foldable                  ( foldl
                                                , maximumBy
                                                )
import           Data.Map                       ( Map
                                                , (!)
                                                )
import qualified Data.Map                      as Map
import           Data.Modular
import           Data.Monoid
import           Data.Ord                       ( comparing )
import           Data.Attoparsec.Text          as P
import           Data.List                      ( sortBy
                                                , sort
                                                , sortOn
                                                )
import           Data.Time
import           Data.Time.Clock                ( secondsToDiffTime
                                                , diffUTCTime
                                                , NominalDiffTime
                                                )
import           Data.Time.Lens
import           Data.Fixed
import           Data.Range.Range

import           Data.String.Here               ( here )
import qualified Data.Text                     as T
import           Data.Text                      ( pack )

import           Safe.Foldable


type EventTime = UTCTime
timestampP :: Parser EventTime
timestampP = do
  yyyy <- "[" *> decimal
  mm   <- "-" *> decimal
  dd   <- "-" *> decimal
  hh   <- " " *> decimal
  mins <- ":" *> decimal
  _    <- string "]"
  return $ UTCTime (fromGregorian yyyy mm dd)
                   (secondsToDiffTime $ (hh * 60 + mins) * 60)

{- Guard -}
type Guard = Integer

{- Wake Event -}
data WakeEvent a = BeginsShift a | WakesUp | FallsAsleep
  deriving (Eq)

wakeEventP :: Parser (WakeEvent Guard)
wakeEventP = skipSpace *> choice
  [ BeginsShift <$> ("Guard #" *> decimal <* " begins shift")
  , string "wakes up" $> WakesUp
  , string "falls asleep" $> FallsAsleep
  ]

instance Show a => Show (WakeEvent a)
  where
  show (BeginsShift a) = "Guard #" ++ show a ++ " begins shift"
  show WakesUp         = "wakes up"
  show FallsAsleep     = "falls asleep"

{- Event -}
data Event a = At { _when::EventTime, _what :: a }
  deriving (Eq)

makeLenses ''Event

instance Show a => Show (Event a)
  where
  show (At t a) =
    "["
      ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" t
      ++ "] "
      ++ show a
      ++ "\n"

instance (Eq a) => Ord (Event a)
  where
  compare = compare `on` view when

eventP :: Parser a -> Parser (Event a)
eventP aP = At <$> timestampP <*> aP <* skipSpace


type GuardEvent = Event (WakeEvent Guard)

{- SleepRange -}
newtype SleepRange = SleepRange { _range :: Range NominalDiffTime }
  deriving (Eq, Show)

makeLenses ''SleepRange

instance (Ord SleepRange)
  where
  compare a b | a == b = EQ
  compare (SleepRange (SpanRange a b)) (SleepRange (SpanRange c d)) =
    mappend (compare a c) (compare d b)
  compare _ _ = EQ

toWakeEvents :: SleepRange -> [Event (WakeEvent Guard)]
toWakeEvents (SleepRange (SpanRange a b)) =
  [At (addUTCTime a baseDate) FallsAsleep, At (addUTCTime b baseDate) WakesUp]
toWakeEvents _ = []

{- Time Functions -}

baseDate :: UTCTime
baseDate = UTCTime (fromGregorian 1518 11 01) 0
baseOffset = flip diffUTCTime baseDate

type Min60 = Int / 60

{- Shift -}
data Shift a = Shift { _shiftStart :: UTCTime
                     , _shiftGuard::Guard
                     , _shiftWhat :: a}
  deriving (Eq, Functor)
makeLenses ''Shift

instance Show (Shift [SleepRange])
  where
  show s = concat $ show <$> (begin s : naps s)
   where
    begin s = At (view shiftStart s) (BeginsShift $ view shiftGuard s)
    naps s = mconcat $ toWakeEvents <$> view shiftWhat s


instance Ord (Shift [SleepRange])
  where
  compare = flip compare `on` view shiftStart

shiftP :: (EventTime -> EventTime -> SleepRange) -> Parser (Shift [SleepRange])
shiftP mkRange = do
  start  <- guardP
  sleeps <- many sleepRangeP
  return $ Shift (start ^. when) (start ^. what) sleeps

 where
  guardP :: Parser (Event Guard)
  guardP = eventP (" Guard #" *> decimal <* " begins shift" <* endOfLine)
  sleepRangeP :: Parser SleepRange
  sleepRangeP =
    mkRange
      <$> (view when <$> eventP (string " falls asleep") <* skipSpace)
      <*> (view when <$> eventP (string " wakes up") <* skipSpace)



finiteLength :: Num a => Range a -> a
finiteLength (SpanRange a b) = abs (b - a)
finiteLength _               = 0


-- Puzzle 1


mkTimeRange :: UTCTime -> UTCTime -> Range NominalDiffTime
mkTimeRange = SpanRange `on` baseOffset

mkSleep :: UTCTime -> UTCTime -> SleepRange
mkSleep = (SleepRange .) . mkTimeRange


toMinutes :: NominalDiffTime -> Int
toMinutes t = floor $ toRational t / 60

asleepFor :: [SleepRange] -> Int
asleepFor sleep = sum $ pred . toMinutes . finiteLength . _range <$> sleep


asleepMinutes :: SleepRange -> [Min60]
asleepMinutes (SleepRange (SpanRange a b)) =
  toMod <$> fromRanges [SpanRange (toMinutes a) (toMinutes b - 1)]
asleepMinutes _ = []

counts :: (Ord a) => [a] -> Map a Int
counts ms = build $ project <$> ms
 where
  project = (, 1)
  build   = Map.fromListWith (+)

minutesForGuard :: Map Guard [SleepRange] -> Guard -> Map Min60 Int
minutesForGuard = (minutesForGuard' .) . (!)

minutesForGuard' :: [SleepRange] -> Map Min60 Int
minutesForGuard' = counts . foldMap asleepMinutes

type Solveable = Map Guard [SleepRange]
toSolveable :: [Shift [SleepRange]] -> Solveable
toSolveable ss = build $ project <$> ss
 where
  project = view shiftGuard &&& view shiftWhat
  build   = Map.fromListWith (++)

findMaxValue :: Ord b => Map a b -> a
findMaxValue = fst . maximumBy (comparing snd) . Map.toList

solution :: [Shift [SleepRange]] -> Int
solution ss = fromInteger findSleepiestGuard
  * unMod (findSleepiestMinute findSleepiestGuard)
 where
  findSleepiestGuard  = findMaxValue $ asleepFor <$> base
  findSleepiestMinute = findMaxValue . minutesForGuard base
  base                = toSolveable ss

data Result2 = Result2 {
    _r2guard :: Guard,
    _r2Count :: Int }
  deriving (Eq, Show)
makeLenses ''Result2

instance Ord Result2 where
  compare = comparing (view r2Count)

solution2b :: [Shift [SleepRange]] -> Maybe (Min60, Guard)
solution2b ss = second fst <$> solution2b'
  (foldMap go $ Map.toList $ minutesForGuard' <$> toSolveable ss)
 where
  go :: (a, Map b c) -> [(a, (b, c))]
  go (a, bc) = (a, ) <$> Map.toList bc

solution2b' :: [(Guard, (Min60, Int))] -> Maybe (Min60, (Guard, Int))
solution2b' as =
  maximumByMay (comparing (snd . snd))
    $ Map.toList
    $ Map.mapMaybe scoreMinute
    $ Map.fromListWith mappend (twizzle <$> as)
 where
  twizzle :: (a, (b, c)) -> (b, [(a, c)])
  twizzle (k, (v1, v2)) = (v1, [(k, v2)])
  scoreMinute :: [(Guard, Int)] -> Maybe (Guard, Int)
  scoreMinute = uniqueHead snd <$> sortOn (negate . snd)

  uniqueHead :: (Show a, Eq b) => (a -> b) -> [a] -> Maybe a
  uniqueHead f = go
   where
    go (a : b : _) | f a == f b = Nothing
                   | otherwise  = Just a
    go [a] = Just a
    go _   = Nothing
-- Parsing




shiftStream
  :: (EventTime -> EventTime -> SleepRange)
  -> [GuardEvent]
  -> [Shift [SleepRange]]
shiftStream mkRange es = go es []
 where
  go :: [GuardEvent] -> [Shift [SleepRange]] -> [Shift [SleepRange]]
  go (At t (BeginsShift g) : rest) ss = go rest (Shift t g [] : ss)
  go (At t1 FallsAsleep : At t2 WakesUp : rest) (shift : shifts) =
    go rest (update t1 t2 shift : shifts)
  go _ ss = reverse ss
  update :: EventTime -> EventTime -> Shift [SleepRange] -> Shift [SleepRange]
  update t1 t2 = over shiftWhat (sort . cons (mkRange t1 t2))


parseInput :: Parser [Shift [SleepRange]]
parseInput = many (shiftP mkSleep)

parseInputB :: Parser [Shift [SleepRange]]
parseInputB = shiftStream mkSleep . sort <$> many (eventP wakeEventP)

parseLine :: Parser GuardEvent
parseLine = eventP (skipSpace *> wakeEventP) <* (endOfLine <|> endOfInput)

{- Puzzle -}
newtype P4D1 = Puzzle { _P4D1Shifts :: [Shift [SleepRange]]}

makeLenses ''P4D1

p4D1p :: Parser P4D1
p4D1p = Puzzle . sort <$> parseInput
