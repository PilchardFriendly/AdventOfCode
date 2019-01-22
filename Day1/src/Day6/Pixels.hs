{-# LANGUAGE FlexibleInstances #-}
module Day6.Pixels where

import Day6.XY
import Day6.Bounds
import Data.List (intercalate)
import Data.Maybe
import qualified Data.Map as M
import Data.Map ((!?))

class Pixels a where
    pixels :: a -> [(XY,Char)]    


class AsPixel a where
    pixel :: a-> Char

displayPixels :: (HasBounds a, Pixels a) => a -> String
displayPixels a = intercalate "\n" $ line <$> (boundsYs box)
    where
    box   = bounds a
    chars = M.fromList $ pixels a
    pixelAt xy = fromMaybe ' ' $ chars !? xy
    line y = [ pixelAt (x, y) | x <- (boundsXs box) ] 
    

instance Pixels a => Pixels [a]
    where
     pixels = concatMap pixels    

instance Pixels ()
     where
     pixels _ = []     

instance (AsPixel Char) 
    where
        pixel = id

instance (AsPixel v) => Pixels (M.Map XY v)
    where
        pixels m= M.toList (M.map pixel m)