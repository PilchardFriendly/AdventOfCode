{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
module Day6.Pixels.Puzzle where

import           Data.Map                       ( Map
    , (!?)
    )
import qualified Data.Map as M
import Data.Char
import Control.Lens
import Day6.Pixels
import Day6.XY
import Day6.Bounds
import Day6.OnlyScore

      
      
instance Pixels a => Pixels (Bounds a)
      where
        pixels b = [ ((x, y), '.') | x <- boundsXs b, y <- boundsYs b ]
            ++ pixels (b ^. boundsA)      

instance AsPixel (OnlyScore a) 
    where
        pixel (OnlyScore n _) = chr (ord '0' - fromInteger n)
