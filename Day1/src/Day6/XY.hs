module Day6.XY where

import           Control.Lens
import           Control.Lens.TH                ( makeLenses )

type XY = (Integer, Integer)

mkUnitCoord :: Integer -> Integer -> XY
mkUnitCoord x y = (x, y)

coordX :: Lens' (XY) Integer
coordX = _1
coordY :: Lens' (XY) Integer
coordY = _2
    