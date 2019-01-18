{-# LANGUAGE TupleSections #-}
module Counting where
import           Data.Map                       ( Map, fromListWith )

counts :: (Num b, Ord a) => [a] -> Map a b
counts ms = build $ project <$> ms
 where
  project = (, 1)
  build   = fromListWith (+)