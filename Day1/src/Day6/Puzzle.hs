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
{-# LANGUAGE BangPatterns #-}

module Day6.Puzzle
    ( module Day6.Puzzle
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
import           Data.Either
import           Data.Function
import           Data.List                      ( intercalate
                                                , maximumBy
                                                )
import qualified Data.List.NonEmpty            as NE

import           Data.Map                       ( Map
                                                , (!?)
                                                )
import           Data.Maybe
import qualified Data.Map                      as M
import           Data.Ord
import           Data.Semigroup                 ( Min(..)
                                                , Max(..)
                                                , Sum(..)
                                                , Endo(..)
                                                )
import           Data.Semigroup.Foldable        ( foldMap1 )
import           Data.Set (Set)
import qualified          Data.Set as S

import           Control.Applicative
import           Control.Arrow
import           Control.Lens
import           Control.Lens.TH                ( makeLenses )

import           Day6.MinMax
import           Day6.Scoreable
import           Day6.OnlyScore
import           Day6.Bounds
import           Day6.XY
import           Day6.Pixels
import           Day6.Pixels.Puzzle
import           Day6.Partitionable
import           Day6.Steppable
import Maxing
import Debug.Trace


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

startChar :: Lens' Start Char
startChar = lens f g
  where
    f (Start _ c) = c
    g (Start s _) c = (Start s c)

data Position = MkPosition XY Start
  deriving (Eq, Show)

positionStart :: Lens' Position Start
positionStart = lens sa asa
  where
    sa (MkPosition _ s) = s
    asa (MkPosition xy _) s = MkPosition xy s 


instance Scoreable Position where
    score (MkPosition (x,y) (Start (ox,oy) _)) = 0 - (abs $ ox-x) - (abs $ oy -y)


instance Partitionable Position XY
  where
    intoPartition = go
      where
        go :: Position -> (Integer, Integer)
        go (MkPosition xy _) = xy



newtype Puzzle = Puzzle { _puzzleXYs :: NE.NonEmpty XY }
  deriving (Eq, Show)
makeLenses ''Puzzle



instance Pixels Start where
    pixels s@(Start xy _) = [(xy, pixel s)]

instance AsPixel Start
  where
    pixel (Start _ c) = toUpper c

instance Pixels Position where
    pixels p@(MkPosition xy (Start _ c)) = [(xy, pixel p)]

instance AsPixel Position
  where
    pixel p@(MkPosition xy s@(Start oxy _)) | xy == oxy = pixel s
                                            | otherwise = toLower $ pixel s




-- instance Pixels a => Pixels (Bounds a)


inBoundsPosition :: Bounds a -> Position -> Bool
inBoundsPosition b = inBoundsXY b . intoPartition


mkBounds :: Puzzle -> Bounds Puzzle
mkBounds p@(Puzzle coords) = Bounds xs ys p
    where (xs, ys) = foldMap1 (minMax *** minMax) coords


pairP :: Parser XY
pairP =
    mkUnitCoord
        <$> (decimal <* skipSpace)
        <*> (string "," *> skipSpace *> decimal <* (endOfLine <|> endOfInput))

puzzleP :: Parser Puzzle
puzzleP = Puzzle . NE.fromList <$> many1' pairP

type Step1 = NE.NonEmpty XY
type Step2 = [Start]

step2 :: Step1 -> Step2
step2 = imap go . NE.toList
  where
    go :: Int -> XY -> Start
    go n xy = Start xy $ charFor n
    charFor n | n < 26 = chr (n + ord 'a')
              | otherwise = chr (n + 0x4e00)

data Layer1b = Layer1b {
    _layer1bStart :: Start
    ,_layer1bMap :: Map XY (OnlyScore Position)}
    deriving (Eq, Show)

makeLenses ''Layer1b
instance Pixels Layer1b 
  where
    pixels = pixels . _layer1bStart

layer1b :: Bounds Step1 -> Bounds [Layer1b]
layer1b b = (over boundsA) go b
  where
    go :: Step1 -> [Layer1b]
    go =  fmap step.step2 
    step :: Start -> Layer1b
    step s = Layer1b s . M.fromList $
             ((intoPartition &&& mkOnlyScore ) . flip MkPosition s) <$> 
             boundsXYs b

newtype Layer2b = MkLayer2b { _unLayer2b :: Bounds [Layer1b] }
  deriving stock (Eq, Show)
  deriving newtype (HasBounds, Pixels)

makeLenses ''Layer2b

expand' :: (Functor t) => Lens' s a -> (a -> t a) -> s -> t s
expand' lens f s = flip (set lens) s <$> f (view lens s)


mkLayer2b :: Puzzle -> Layer2b
mkLayer2b p@(Puzzle b) = go p
    where
    go :: Puzzle -> Layer2b
    go = MkLayer2b . layer1b . (over boundsA) toStep1 . mkBounds
    toStep1 :: Puzzle -> Step1
    toStep1 (Puzzle xys) = xys    

type LeftMapFold k v = (Ord k, Semigroup v) => Map k v -> (k, v) -> Map k v

type LeftFoldToMap k1 v1 k v
    =  (Ord k1, Ord k, Semigroup v)
    => (v1 -> Maybe (k, v))
    -> Map k1 v1
    -> Map k v


charForSoleClaim :: Claim -> Maybe Char
charForSoleClaim = preview (_Right . positionStart . startChar) . _unClaim

foldlToMap :: LeftFoldToMap k1 v1 k v
foldlToMap f = foldl go M.empty . mapMaybe f . M.elems
  where
    go :: LeftMapFold c i
    go m (c, i) = M.insertWith (<>) c i m

solution :: Set Char -> Map XY Claim -> (Char, Integer)
solution exclude = (second getSum) . go
  where
    go :: Map XY Claim -> (Char, Sum Integer)
    go = findMaxValueSnd . notMember . foldlToMap score
    notMember :: Map Char (Sum Integer) -> Map Char (Sum Integer)
    notMember m = S.foldr M.delete m exclude 

    score :: Claim -> Maybe (Char, Sum Integer)
    score =
        fmap (, Sum 1) . charForSoleClaim

nonSolutions :: Layer3 -> Set Char
nonSolutions = (uncurry go) . (boundsBorders &&& _boundsA) . _unLayer3
        where
            go :: [XY] -> Map XY Claim -> Set Char
            go xys m = S.fromList $ (mapMaybe charForSoleClaim ) $ (m M.!) <$> xys

solveB :: Puzzle -> (Char, Integer)
solveB =  go
   where
      go = (uncurry solution) . (nonSolutions &&& _boundsA . _unLayer3) . mkLayer3b . mkLayer2b

newtype Claim = MkClaim { _unClaim :: Either [Position] Position }
  deriving (Show,Eq)

instance AsPixel Claim
  where
    pixel = either (const '·') pixel . _unClaim

newtype Layer3 = MkLayer3 { _unLayer3 :: Bounds (Map XY Claim) }
  deriving stock (Eq, Show)
  deriving newtype (HasBounds, Pixels)


mkLayer3b :: Layer2b -> Layer3
mkLayer3b = MkLayer3 . over (boundsA) (go . fmap _layer1bMap) . _unLayer2b
 where
    go :: [Map XY (OnlyScore Position)] -> Map XY Claim
    go =  M.map elem . M.unionsWith (<>)
    elem :: OnlyScore Position -> Claim
    elem (OnlyScore _ e) = MkClaim e

