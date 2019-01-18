{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Day4.Time where

import           Data.Time
import Data.Modular
{- Time Functions -}

baseDate :: UTCTime
baseDate = UTCTime (fromGregorian 1518 11 01) 0
baseOffset = flip diffUTCTime baseDate

type Min60 = Integer / 60