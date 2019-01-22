{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Day6.Puzzle
    ( module Data.Attoparsec.Text
    , module Day6.Puzzle
    , module Day6.XY
    , module Day6.Bounds
    , module Day6.Pixels
    , module Day6.Pixels.Puzzle
    , module Day6.Steppable
    , module Day6.OnlyScore
    )
where
import           Data.Attoparsec.Text
import           Numeric.Natural
import           Data.Char
import           Data.List                      (
                                                  intercalate
                                                )
import qualified Data.List.NonEmpty            as NE
import           Data.Map                       ( Map
                                                , (!?)
                                                )
import           Data.Maybe
import qualified Data.Map                      as M
import           Data.Semigroup                 ( Min(..)
                                                , Max(..)
                                                )
import           Data.Semigroup.Foldable        ( foldMap1 )

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Lens.TH                ( makeLenses )

import Day6.MinMax
import Day6.Scoreable
import Day6.OnlyScore
import Day6.Bounds
import Day6.XY
import Day6.Pixels
import Day6.Pixels.Puzzle
import Day6.Partitionable
import Day6.Steppable


data Rotation = Rotate0 | Rotate90 | Rotate180 | Rotate270
  deriving (Eq, Show, Enum)

rotate :: Num a => Rotation -> (a, a) -> (a, a)
rotate Rotate0   (x, y) = (x, y)
rotate Rotate90  (x, y) = (-y, x)
rotate Rotate180 (x, y) = (-x, -y)
rotate Rotate270 (x, y) = (y, -x)


data Move = Move Rotation Natural {- Must be at least 1-} Natural
  deriving (Eq, Show)

data Start = Start XY Char
  deriving (Eq, Show)

data Position = Starting Start
              | Moving Move Start
  deriving (Eq, Show)

positionStart :: Lens' Position Start
positionStart = lens sa asa
  where
    sa (Starting s) = s
    sa (Moving _ s) = s
    asa (Starting _) s = Starting s
    asa (Moving m _) s = Moving m s


instance Scoreable Move where
    score (Move r v h) = (toInteger v) + (toInteger h)
    
instance Scoreable Position where
    score (Moving m _) = -score m
    score (Starting _) = 0


instance Steppable Move where
    nextSteps (Move r v h) = [Move r (v + 1) h, Move r v (h + 1)]
instance Steppable Position where
    nextSteps (Starting s) =
        (\r -> Moving (Move r 1 0) s)
            <$> [Rotate0, Rotate90, Rotate180, Rotate270]
    nextSteps (Moving m s) = (\m' -> Moving m' s) <$> nextSteps m



instance Partitionable Position XY
  where
    intoPartition = go
      where
        tupleToInt = over both toInteger
        go :: Position -> (Integer, Integer)
        go (Starting (Start o _)                  ) = o
        go (Moving (Move r v h) (Start (oX, oY) _)) = (uX + oX, uY + oY)
            where (uX, uY) = rotate r $ tupleToInt (h, v)





newtype Puzzle = Puzzle (NE.NonEmpty XY)
  deriving (Eq, Show)



instance Pixels Start where
    pixels s@(Start xy _) = [(xy,pixel s)]

instance AsPixel Start
  where
    pixel (Start _ c) = toUpper c

instance Pixels Position where
    pixels (  Starting s           ) = pixels s
    pixels p@(Moving _ (Start xy c)) = [(intoPartition p :: XY, pixel p)]

instance AsPixel Position 
  where
    pixel (Starting s) = pixel s
    pixel (Moving _ s) = toLower $ pixel s




-- instance Pixels a => Pixels (Bounds a)



mkBounds :: Puzzle -> Bounds Puzzle
mkBounds p@(Puzzle coords) = Bounds xs ys p
    where (xs, ys) = foldMap1 (minMax *** minMax) coords


pairP :: Parser XY
pairP =
    mkUnitCoord
        <$> (decimal <* skipSpace)
        <*> (string "," *> decimal <* (endOfLine <|> endOfInput))

puzzleP :: Parser Puzzle
puzzleP = Puzzle . NE.fromList <$> many1' pairP

type Step1 = NE.NonEmpty XY
type Step2 = [Start]
type Step3 = [Position]
type Step4 = [Position]

step2 :: Step1 -> Step2
step2 = imap go . NE.toList
  where
    go :: Int -> XY -> Start
    go n xy = Start xy $ chr (n + ord 'a')

step3 :: Step2 -> Step3
step3 = map Starting

step4 :: Step3 -> Step4
step4 = concatMap nextSteps

data Layer1 = Layer1 {
    _layer1Positions :: [Position], 
    _layer1Map :: (Map XY (OnlyScore Position))}
  deriving (Eq, Show)
instance (Pixels Layer1) 
  where
    pixels = pixels._layer1Map

makeLenses ''Layer1

layer1 :: Step1 -> Layer1
layer1 xys = Layer1 ps M.empty where ps = step3 . step2 $ xys

-- Layer1FilterBounds :: Bounds a -> Layer1 -> Layer1
-- layer1FilterBounds b (Layer1 ps m) = Layer1 (filter ) 
instance Steppable Layer1 where
    nextSteps (Layer1 ps m)= [Layer1 ps' m']
        where
            m'  = foldl accumulateUniqueScore m ps
            ps' = concatMap nextSteps ps


newtype Layer2 = MkLayer2 { _unLayer2 :: Bounds Layer1 }
  deriving stock (Eq, Show) 
  deriving newtype (HasBounds, Pixels)

makeLenses ''Layer2


inBoundsPosition :: Bounds a -> Position -> Bool
inBoundsPosition b = (inBoundsXY b) . intoPartition


expand' :: (Functor t) => Lens' s a -> (a -> t a) -> s -> t s
expand' lens f s = flip (set lens) s <$> f (view lens s)


instance Steppable Layer2 where
    nextSteps l2@(MkLayer2 b) =  expand' (unLayer2.boundsA) next l2
        where
            next :: Layer1 -> [Layer1]
            next = filter (not.isFinished) . 
                   fmap trimBounds . 
                   nextSteps
            trimBounds = over (layer1Positions) $ filter (inBoundsPosition b) 
            isFinished :: Layer1 -> Bool
            isFinished = null . view layer1Positions 
        
mkLayer2 :: Puzzle -> Layer2 
mkLayer2 p@(Puzzle b )= go p
    where
        go :: Puzzle -> Layer2
        go = MkLayer2 . ((over boundsA) (layer1.toStep1)) . mkBounds
        toStep1 :: Puzzle -> Step1
        toStep1 (Puzzle xys)= xys

simulate :: (Steppable a) => a -> a
simulate s = go s (nextSteps s)
    where 
        go :: (Steppable a) => a -> [a]-> a
        go s [] = s
        go s (next:_) = go next (nextSteps next)

solve :: Puzzle -> Layer2
solve p@(Puzzle b )= go p
    where
        go = simulate.mkLayer2

newtype Claim = MkClaim { _unClaim :: Either [Position] Position }   
  deriving (Show,Eq)

instance AsPixel Claim 
  where
    pixel = either (const '·') pixel . _unClaim

newtype Layer3 = MkLayer3 { _unLayer3 :: Bounds (Map XY Claim) }
  deriving stock (Eq, Show)
  deriving newtype (HasBounds, Pixels)

mkLayer3 :: Layer2 -> Layer3
mkLayer3 = MkLayer3 . over ( boundsA ) (go._layer1Map) . _unLayer2
  where 
     go :: Map XY (OnlyScore Position)-> Map XY Claim
     go = M.map elem
     elem :: OnlyScore Position -> Claim
     elem (OnlyScore _ e )= MkClaim e
-- instance Pixels Layer1
--     where
--         pixels l1 = 