module Day6.MinMax (module Day6.MinMax, module Data.Semigroup) where

import           Data.Semigroup                 ( Min(..), Max(..))    
import Control.Arrow
type MinMax a = (Min a, Max a)

minMax :: a -> MinMax a
minMax a = (Min a, Max a)

rangeMinMax :: (Num a, Enum a) => MinMax a -> [a]
rangeMinMax a = enumFromTo lo hi
  where
    (lo,hi) = (getMin *** getMax) a

inMinMax :: Ord a => MinMax a -> a -> Bool
inMinMax (lo,hi) a | getMin lo <= a && a <= getMax hi = True
                    | otherwise = False
    
