{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}

module Day6.Bounds where

import           Control.Lens
import           Control.Lens.TH                ( makeLenses )
import Day6.MinMax
import Day6.XY

data Bounds b = Bounds { _boundsX ::MinMax Integer, _boundsY :: MinMax Integer, _boundsA :: b }
    deriving (Eq, Show, Functor)

class HasBounds a where
    bounds :: a -> Bounds ()

instance HasBounds (Bounds a) where
    bounds (Bounds c1 c2 stuff) = Bounds c1 c2 ()    
    
makeLenses ''Bounds

boundsX1 :: Lens' (Bounds a) (Min Integer)
boundsX1 = boundsX . _1
boundsY1 :: Lens' (Bounds a) (Min Integer)
boundsY1 = boundsY . _1
boundsX2 :: Lens' (Bounds a) (Max Integer)
boundsX2 = boundsX . _2
boundsY2 :: Lens' (Bounds a) (Max Integer)
boundsY2 = boundsY . _2

boundsXs :: Bounds a -> [Integer]
boundsXs b = rangeMinMax $ b ^. boundsX
boundsYs :: Bounds a -> [Integer]
boundsYs b = rangeMinMax $ b ^. boundsY    

inBoundsXY :: Bounds a -> XY -> Bool
inBoundsXY b (x,y) =  inMinMax (_boundsX b) x && inMinMax (_boundsY b) y
            
