module Day6.OnlyScore where

import Day6.Scoreable
import Day6.Partitionable
import Data.List (nub)
import           Data.Map                       ( Map
    , (!?)
    )
import qualified Data.Map as M

data OnlyScore a = OnlyScore Integer (Either [a] a) {- so we keep the best, and the score of the next worst-}
  deriving (Eq, Show)

mkOnlyScore :: (Scoreable a) => a -> OnlyScore a
mkOnlyScore a = OnlyScore (score a) (Right a)

instance Eq a => Semigroup (OnlyScore a) where
    o1@(OnlyScore s as) <> o2@(OnlyScore s2 bs)
        | s == s2 && as == bs = o1
        | s == s2             = OnlyScore s (go as bs)
        | s < s2              = o2
        | s > s2              = o1
      where
        go (Right a ) (Right b ) = Left [a, b]
        go (Left  as) (Right b ) = Left $ nub (b : as)
        go (Left  as) (Left  bs) = Left $ nub (as ++ bs)
        go (Right a ) (Left  bs) = Left $ nub (a : bs)  
        
accumulateUniqueScore
        :: (Eq v, Partitionable v k, Ord k, Scoreable v)
        => Map k (OnlyScore v)
        -> v
        -> Map k (OnlyScore v)
accumulateUniqueScore m v =
        M.insertWith (<>) (intoPartition v) (mkOnlyScore v) m        