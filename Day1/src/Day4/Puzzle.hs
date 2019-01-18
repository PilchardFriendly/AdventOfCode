{- HLINT ignore "Unused LANGUAGE pragma" -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Day4.Puzzle where


import           Control.Applicative
import           Control.Arrow
import           Control.Lens            hiding ( ifoldr )
import           Control.Lens.TH                ( makeLenses )


import           Data.Functor
import           Data.Function                  ( on )
import           Data.Foldable                  ( maximumBy )
import           Data.Monoid                    ( Sum )
import           Data.Map                       ( Map
                                                , (!)
                                                )
import qualified Data.Map                      as Map
import           Data.Modular
import           Data.Ord                       ( comparing )
import           Data.Attoparsec.Text          as P
import           Data.List                      ( sort
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

import           Data.Vector.Sized              ( Vector
                                                , ifoldr
                                                )
import           Data.Finite                    ( Finite
                                                , getFinite
                                                )
import           Safe.Foldable
import           Day4.SleepRange
import           Day4.Time
import           Day4.Minutes


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
data Event a = At { _eventWhen::EventTime, _what :: a }
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
  compare = compare `on` view eventWhen

eventP :: Parser a -> Parser (Event a)
eventP aP = At <$> timestampP <*> aP <* skipSpace


type GuardEvent = Event (WakeEvent Guard)


toWakeEvents :: SleepRange -> [Event (WakeEvent Guard)]
toWakeEvents (SleepRange (SpanRange a b)) =
  [At (addUTCTime a baseDate) FallsAsleep, At (addUTCTime b baseDate) WakesUp]
toWakeEvents _ = []


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
  return $ Shift (start ^. eventWhen) (start ^. what) sleeps

 where
  guardP :: Parser (Event Guard)
  guardP = eventP (" Guard #" *> decimal <* " begins shift" <* endOfLine)
  sleepRangeP :: Parser SleepRange
  sleepRangeP =
    mkRange
      <$> (view eventWhen <$> eventP (string " falls asleep") <* skipSpace)
      <*> (view eventWhen <$> eventP (string " wakes up") <* skipSpace)





-- Puzzle 1

type GuardSleep = Vector 60 Integer


counts :: (Num b, Ord a) => [a] -> Map a b
counts ms = build $ project <$> ms
 where
  project = (, 1)
  build   = Map.fromListWith (+)

minutesForGuard :: Map Guard GuardSleep -> Guard -> Map Min60 Integer
minutesForGuard = (minutesForGuard' .) . (!)

minutesForGuard' :: GuardSleep -> Map Min60 Integer
minutesForGuard' = ifoldr go Map.empty
 where
  go :: Finite 60 -> b -> Map Min60 b -> Map Min60 b
  go n = Map.insert (toMod $ getFinite n)

type Solveable = Map Guard GuardSleep
toSolveable :: [Shift [SleepRange]] -> Solveable
toSolveable ss = build $ project <$> ss
 where
  project = view shiftGuard &&& view shiftWhat
  build   = Map.map minuteCount . Map.fromListWith (++)

findMaxValue :: Ord b => Map a b -> a
findMaxValue = fst . maximumBy (comparing snd) . Map.toList

solution :: [Shift [SleepRange]] -> Integer
solution ss = fromInteger findSleepiestGuard
  * unMod (findSleepiestMinute findSleepiestGuard)
 where
  findSleepiestGuard  = findMaxValue $ sum <$> base
  findSleepiestMinute = findMaxValue . minutesForGuard base
  base                = toSolveable ss

solution2b :: [Shift [SleepRange]] -> Maybe (Min60, Guard)
solution2b ss = second fst <$> solution2b'
  (foldMap go $ Map.toList $ minutesForGuard' <$> toSolveable ss)
 where
  go :: (a, Map b c) -> [(a, (b, c))]
  go (a, bc) = (a, ) <$> Map.toList bc

solution2b'
  :: (Ord b, Num b) => [(Guard, (Min60, b))] -> Maybe (Min60, (Guard, b))
solution2b' as =
  maximumByMay (comparing (snd . snd))
    $ Map.toList
    $ Map.mapMaybe scoreMinute
    $ Map.fromListWith mappend (twizzle <$> as)
 where
  twizzle :: (a, (b, c)) -> (b, [(a, c)])
  twizzle (k, (v1, v2)) = (v1, [(k, v2)])
  scoreMinute :: (Num b, Eq b, Ord b) => [(Guard, b)] -> Maybe (Guard, b)
  scoreMinute = uniqueHead snd <$> sortOn (negate . snd)

  uniqueHead :: (Eq b) => (a -> b) -> [a] -> Maybe a
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





