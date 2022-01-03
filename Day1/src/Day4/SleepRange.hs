{-# LANGUAGE TemplateHaskell #-}

module Day4.SleepRange where

import Control.Lens.TH (makeLenses)
import Data.Function (on)
import Data.Modular (toMod)
import Data.Range
import Data.Time
import Data.Time.Clock (NominalDiffTime)
import Day4.Time

newtype SleepRange = SleepRange {_range :: Range NominalDiffTime}
  deriving (Eq, Show)

makeLenses ''SleepRange

mkTimeRange :: UTCTime -> UTCTime -> Range NominalDiffTime
mkTimeRange = (+=+) `on` baseOffset

mkSleep :: UTCTime -> UTCTime -> SleepRange
mkSleep = (SleepRange .) . mkTimeRange

toMinutes :: NominalDiffTime -> Integer
toMinutes t = floor $ toRational t / 60

instance (Ord SleepRange) where
  compare a b | a == b = EQ
  compare (SleepRange (SpanRange (Bound a _) (Bound b _))) (SleepRange (SpanRange (Bound c _) (Bound d _))) =
    mappend (compare a c) (compare d b)
  compare _ _ = EQ

asleepMinutes :: SleepRange -> [Min60]
asleepMinutes (SleepRange (SpanRange (Bound a _) (Bound b _))) =
  toMod <$> fromRanges [(+=+) (toMinutes a) (toMinutes b - 1)]
asleepMinutes _ = []
