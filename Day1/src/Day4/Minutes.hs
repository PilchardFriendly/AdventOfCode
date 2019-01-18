{-# LANGUAGE DataKinds #-}
module Day4.Minutes where

import           Data.Modular
import           Day4.SleepRange
import           Data.Vector.Sized
import           Data.Finite (finite)
import           Day4.Sized

type MinuteCount a = Vector 60 a

minuteCount :: (Num a, Show a) => [SleepRange] -> Vector 60 a
minuteCount sleep =
    countFinite  $ finite . unMod <$> foldMap asleepMinutes sleep
