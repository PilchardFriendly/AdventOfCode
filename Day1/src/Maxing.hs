module Maxing where
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.List                      ( maximumBy )
import           Data.Ord
import qualified Data.List.NonEmpty  as NE

findMaxValue :: Ord b => Map a b -> Maybe a
findMaxValue = fmap fst . findMaxValueSnd

findMaxValueSnd :: Ord b => Map a b -> Maybe (a, b)
findMaxValueSnd = fmap (maximumBy (comparing snd)) . NE.nonEmpty . Map.toList

