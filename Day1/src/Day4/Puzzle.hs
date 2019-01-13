{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}

module Day4.Puzzle where


import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Lens.TH
import           Data.Maybe                     ( catMaybes
                                                , listToMaybe
                                                )

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
import           Data.Attoparsec.Text          as P
import           Data.List                      ( sortBy
                                                , sort
                                                )
import           Data.Time
import           Data.Time.Clock                ( secondsToDiffTime
                                                , diffUTCTime
                                                , NominalDiffTime
                                                )
import           Data.Time.Lens
import           Data.Fixed
import           Data.Range.Range
import           Day4.Input                     ( puzzleData )

import           Data.String.Here               ( here )
import qualified Data.Text                     as T
import           Data.Text                      ( pack )

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
    show (BeginsShift a) = "Guard #" ++ (show a) ++ " begins shift"
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
data SleepRange = SleepRange { _range :: Range NominalDiffTime }
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

type Min60 = Mod Int 60





{- Shift -}
data Shift a = Shift { _shiftStart :: UTCTime
                     , _shiftGuard::Guard
                     , _shiftWhat :: a}
  deriving (Eq, Functor)
makeLenses ''Shift

instance Show (Shift [SleepRange])
  where
    show s = concat $ show <$> (begin s : naps s)
        -- ++ concat (show <$> mconcat (toWakeEvents <$> (s ^. shiftWhat)))
        where 
            begin s= At (view shiftStart s) (BeginsShift $ view shiftGuard s)
            naps s = mconcat $ toWakeEvents <$> view shiftWhat s


instance Ord (Shift [SleepRange])
  where
    compare = flip compare `on` (view shiftStart)

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




timesToSleep :: UTCTime -> UTCTime -> Range NominalDiffTime
timesToSleep t1 t2 = SpanRange (baseOffset t1) (baseOffset t2)

mkSleep :: UTCTime -> UTCTime -> SleepRange
mkSleep = (SleepRange .) . timesToSleep


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
minutesForGuard base g = minutesForGuard' $ base ! g

minutesForGuard' :: [SleepRange] -> Map Min60 Int
minutesForGuard' ss = counts $ foldMap asleepMinutes $ ss

toSolvable :: [Shift [SleepRange]] -> Map Guard [SleepRange]
toSolvable ss = build $ project <$> ss
  where
    project s@(Shift at g rs) = (g, rs)
    build = Map.fromListWith (++)

sndOrdering :: Ord b => (a, b) -> (a, b) -> Ordering
sndOrdering = compare `on` snd

findMaxValue :: Ord b => Map a b -> a
findMaxValue m = fst . maximumBy sndOrdering $ Map.toList m

solution :: [Shift [SleepRange]] -> Int
solution ss =
    (fromInteger findSleepiestGuard)
        * (unMod $ findSleepiestMinute findSleepiestGuard)
  where
    findSleepiestGuard = findMaxValue $ asleepFor <$> base
    findSleepiestMinute g = findMaxValue $ minutesForGuard base g
    base = toSolvable ss

data Result2 = Result2 {
    _r2guard :: Guard, 
    _r2Count :: Int }
  deriving (Eq, Show)
makeLenses ''Result2

instance Ord Result2 where
    compare = compare `on` (view r2Count)

-- Map Min60 -> (Guard, Int)
solution2 :: [Shift [SleepRange]] -> Int
solution2 ss = 0
  where
    base = toSolvable ss
    findThing :: (Min60, Result2)
    findThing = maximum (toThing <$> those)
    toThing (g, m60, c) = (m60, Result2 g c)
    those :: [(Guard, Min60, Int)]
    those = do 
        (g, min60_2_c) <- Map.assocs $ Map.map minutesForGuard' base
        (min60, c) <- Map.assocs min60_2_c
        return (g, min60, c)
        

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
data P4D1 = Puzzle { _P4D1Shifts :: [Shift [SleepRange]]}

makeLenses ''P4D1

p4D1p :: Parser P4D1
p4D1p = Puzzle . sort <$> parseInput