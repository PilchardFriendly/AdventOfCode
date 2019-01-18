{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Day4.Event where

import           Control.Lens
import           Control.Lens.TH                ( makeLenses )
import           Data.Function                  ( on )
import Data.Attoparsec.Text
import           Data.Time
import Day4.Time


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