module Maxing where
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Data.List                      ( maximumBy )
import           Data.Ord

findMaxValue :: Ord b => Map a b -> a
findMaxValue = fst . findMaxValueSnd

findMaxValueSnd :: Ord b => Map a b -> (a, b)
findMaxValueSnd = maximumBy (comparing snd) . Map.toList

