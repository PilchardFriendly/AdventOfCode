{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TupleSections #-}

module Day6.Bounds where

import           Control.Lens
import           Control.Lens.TH                ( makeLenses )
import Day6.MinMax
import Day6.XY
import Debug.Trace

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

boundsXYs :: Bounds a -> [XY]
boundsXYs b = [(x,y)| x <- boundsXs b, y <- boundsYs b]

inBoundsXY :: Bounds a -> XY -> Bool
inBoundsXY (Bounds xs ys _) (x,y) =  {- trace ("xs: " ++ show xs ++ " ys: " ++ show ys ++ " xy: " ++ show (x,y)) -} inMinMax xs x && inMinMax ys y

boundsBorders :: Bounds a -> [XY]
boundsBorders b@(Bounds (x1,x2) (y1,y2) _) = concat [ 
    (getMin x1,) <$>  boundsYs b
    ,(getMax x2,) <$> boundsYs b
    ,(,getMin y1) <$> boundsXs b
    ,(,getMax y2) <$> boundsXs b]
            
