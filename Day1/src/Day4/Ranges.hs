module Day4.Ranges where

import Data.Range.Range

finiteLength :: Num a => Range a -> a
finiteLength (SpanRange a b) = abs (b - a)
finiteLength _               = 0
