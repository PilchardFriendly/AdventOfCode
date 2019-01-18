{-# LANGUAGE TemplateHaskell #-}
module Day4.SleepRange where

import           Control.Lens.TH                ( makeLenses )
import           Data.Range.Range
import           Data.Time
import           Data.Time.Clock                ( NominalDiffTime )
import           Data.Function                  ( on )
import           Day4.Time
import           Data.Modular                   ( toMod )
newtype SleepRange = SleepRange { _range :: Range NominalDiffTime }
  deriving (Eq, Show)

makeLenses ''SleepRange

mkTimeRange :: UTCTime -> UTCTime -> Range NominalDiffTime
mkTimeRange = SpanRange `on` baseOffset

mkSleep :: UTCTime -> UTCTime -> SleepRange
mkSleep = (SleepRange .) . mkTimeRange

toMinutes :: NominalDiffTime -> Integer
toMinutes t = floor $ toRational t / 60

instance (Ord SleepRange)
  where
  compare a b | a == b = EQ
  compare (SleepRange (SpanRange a b)) (SleepRange (SpanRange c d)) =
    mappend (compare a c) (compare d b)
  compare _ _ = EQ

asleepMinutes :: SleepRange -> [Min60]
asleepMinutes (SleepRange (SpanRange a b)) =
  toMod <$> fromRanges [SpanRange (toMinutes a) (toMinutes b - 1)]
asleepMinutes _ = []
