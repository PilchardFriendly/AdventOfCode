{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Year2021.Day3.Puzzle where

import Data.Attoparsec.Text (Parser, choice, count, decimal, endOfLine, many', many1, parseOnly, sepBy, sepBy1, signed, string)
import Data.Bits (Bits (bit, complement, setBit, shift, shiftL, shiftR, testBit, xor, zeroBits, (.&.), (.|.)), FiniteBits)
import Data.Data (Proxy (Proxy))
import Data.Finite (Finite, finite, finites, getFinite, modulo)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor (($>))
import Data.Functor.Sum (Sum)
import Data.List (sortOn, unfoldr)
import Data.Maybe (Maybe (..))
import Data.Semigroup (Max)
import Data.Text (Text)
import qualified Data.Vector.Sized as Sized (Vector, foldl, foldr, fromTuple, index, length, replicate, reverse, toList, unfoldrN, zipWith)
import GHC.TypeLits (KnownNat, Nat, natVal)
import Year2021.Day3.Input (puzzleData)

data Day3 = Day3

-- Finites
maxFinite :: forall n. KnownNat n => Finite n
maxFinite = last finites

-- Bits
shiftL1 :: Bits a => a -> a
shiftL1 = flip shiftL 1

shiftR1 :: Bits a => a -> a
shiftR1 = flip shiftR 1

-- Advent
newtype Diagnostic (n :: Nat) = Diagnostic Int
  deriving (Show)

mapBitsToSized :: forall n b. KnownNat n => (Finite n -> Bool -> b) -> Diagnostic n -> Sized.Vector n b
mapBitsToSized f (Diagnostic a) = Sized.unfoldrN go maxFinite
  where
    go :: Finite n -> (b, Finite n)
    go fn = (,) (f fn $ testBit a (fromIntegral fn)) (fn - finite 1)

diagnostic :: forall n. KnownNat n => Int -> Diagnostic n
diagnostic = Diagnostic

diagnosticParser :: Parser (Diagnostic n)
diagnosticParser = go <$> digits
  where
    go :: [Int] -> Diagnostic n
    go = Diagnostic . shiftR1 . foldl step zeroBits
    step :: Bits a => a -> a -> a
    step a b = shiftL1 a .|. b
    digits :: Bits a => Parser [a]
    digits =
      many1 $
        choice
          [ string "0" $> zeroBits,
            string "1" $> bit 1
          ]

newtype Score (n :: Nat) = Score Int
  deriving (Show)

-- >>> (Analysis @5 (5, Sized.fromTuple(1,2,3,4,5))) <> (Analysis (5, Sized.fromTuple (1,2,3,4,5)))
-- Analysis (10,Vector [2,4,6,8,10])

parse :: forall n. KnownNat n => Text -> Either String [Diagnostic n]
parse = parseOnly parser

parser :: Parser [Diagnostic n]
parser = sepBy1 diagnosticParser endOfLine

-- >>> parse "00000\n10000\n00001"
-- Right [Diagnostic 0,Diagnostic 16,Diagnostic 1]

newtype Analysis (n :: Nat) = Analysis (Int, Sized.Vector n Int)
  deriving (Show)

instance Semigroup (Analysis n) where
  (<>) (Analysis (a1, a2)) (Analysis (b1, b2)) = Analysis (a1 + b1, Sized.zipWith (+) a2 b2)

instance KnownNat n => Monoid (Analysis n) where
  mempty = Analysis (0, Sized.replicate 0)

analyse :: forall n. KnownNat n => Diagnostic n -> Analysis n
analyse d = Analysis $ (1,) $ mapBitsToSized go d
  where
    go :: Finite n -> Bool -> Int
    go fn True = 1
    go fn _ = 0

analyseAll :: forall n f. (KnownNat n, Foldable f) => f (Diagnostic n) -> Analysis n
analyseAll = foldMap analyse

-- >>> analyse @5 <$> [Diagnostic (0), Diagnostic (16), Diagnostic (31)]
-- [Analysis (1,Vector [0,0,0,0,0]),Analysis (1,Vector [1,0,0,0,0]),Analysis (1,Vector [1,1,1,1,1])]

-- >>> gamma <$> mconcat <$> fmap (analyse @5 . Diagnostic) <$> [[31,0,0],[31,0],[7,0,1]]
-- [0,31,1]

-- >>> gamma @5 <$> [Analysis (1, Sized.fromTuple (0, 0, 0, 0, 0)), Analysis (1, Sized.fromTuple (1,1,1,1,1)), Analysis (1,Sized.fromTuple (0,0,0,0,1))]
-- [0,31,1]
gamma :: Analysis n -> Int
gamma (Analysis (n, counts)) = shiftR1 (Sized.foldr go 0 $ Sized.reverse counts)
  where
    go :: Int -> Int -> Int
    go a b | 2 * a >= n = shiftL1 (setBit b 0)
    go a b = shiftL1 b

epsilon :: Analysis n -> Int
epsilon (Analysis (n, counts)) = shiftR1 (Sized.foldr go 0 $ Sized.reverse counts)
  where
    go :: Int -> Int -> Int
    go a b | 2 * a < n = shiftL1 (setBit b 0)
    go a b = shiftL1 b

bitDepth :: forall n. KnownNat n => Analysis n -> Int
bitDepth (Analysis (_, counts)) = Sized.length @n counts

-- >>> shiftR1 $ shiftL1 (setBit (0::Word) 0)
-- 1
solution :: forall n. KnownNat n => Analysis n -> Int
solution gr = g * (epsilonC - g)
  where
    g = gamma gr
    epsilonC = 2 ^ bitDepth gr -1

-- >>> solution @5 <$> [Analysis (1, Sized.fromTuple (0,0,0,0,1))]
-- [30]

solve :: forall n. KnownNat n => [Diagnostic n] -> Int
solve = solution . analyseAll

solve2 :: forall n. KnownNat n => [Diagnostic n] -> Maybe Int
solve2 = solution2

solution2 :: forall n. KnownNat n => [Diagnostic n] -> Maybe Int
solution2 ds =
  calc <$> go (summarise oxygenScore)
    <*> go (summarise carbonScore)
  where
    calc :: Diagnostic oxy -> Diagnostic carbon -> Int
    calc (Diagnostic ox) (Diagnostic carbon) = ox * carbon

    summarise f = f . analyseAll
    go :: ([Diagnostic n] -> Score n) -> Maybe (Diagnostic n)
    go f = sieve f ds

sieve :: forall n. KnownNat n => ([Diagnostic n] -> Score n) -> [Diagnostic n] -> Maybe (Diagnostic n)
sieve summarise ds = go maxFinite (Right ds)
  where
    go fn (Right rs) = go (fn - finite 1) $ extract $ summariseAndFilter summarise (bitEq fn) rs
    go fn (Left r) = r

-- >>> sieve (oxygenScore . analyseAll) exampleDiagnostics
-- Just (Diagnostic 23)

-- >>> sieve (carbonScore . analyseAll) exampleDiagnostics
-- Just (Diagnostic 10)

extract :: [a] -> Either (Maybe a) [a]
extract [] = Left Nothing
extract [x] = Left $ Just x
extract xs = Right xs

bitEq :: forall n. KnownNat n => Finite n -> Score n -> Diagnostic n -> Bool
bitEq fn (Score ref) (Diagnostic d) = equating ref d
  where
    equating :: Int -> Int -> Bool
    equating = on (==) (`testBit` minBit)
    minBit = fromIntegral fn

oxygenScore :: Analysis n -> Score n
oxygenScore = Score . gamma

carbonScore :: Analysis n -> Score n
carbonScore = Score . epsilon

-- >>> oxygenScore <$> [(Analysis @5 (1, Sized.fromTuple (1::Int,1,1,1,1))), (Analysis @5 (5, Sized.fromTuple(2::Int, 4, 6, 8, 10)))]
-- [Score 31,Score 15]
-- >>> carbonScore <$> [(Analysis @5 (1, Sized.fromTuple (1::Int,1,1,1,1))), (Analysis @5 (5, Sized.fromTuple(2::Int, 4, 6, 8, 10)))]
-- [Score 0,Score 16]

summariseAndFilter :: ([a] -> b) -> (b -> a -> Bool) -> [a] -> [a]
summariseAndFilter f p as = filter (p $ f as) as

-- >>> summariseAndFilter (\as -> div (sum as) (length as)) (\a b -> a < b) $ [1,2,3,4,5]
-- [4,5]
-- >>> summariseAndFilter (\as -> div (sum as) (length as)) (\a b -> a >= b) $ [1,2,3,4,5]
-- [1,2,3]

-- >>> solve <$> parse @12 puzzleData
-- Right 4103154

-- >>> take 5 <$> parse @12 puzzleData
-- Right [Diagnostic 1137,Diagnostic 3329,Diagnostic 3659,Diagnostic 3848,Diagnostic 489]
-- >>> solve @5 <$> testData
-- [30,240,198]

-- >>> fmap (analyse @5 . Diagnostic) example
-- [Analysis (1,Vector [0,0,1,0,0]),Analysis (1,Vector [1,1,1,1,0]),Analysis (1,Vector [1,0,1,1,0]),Analysis (1,Vector [1,0,1,1,1]),Analysis (1,Vector [1,0,1,0,1]),Analysis (1,Vector [0,1,1,1,1]),Analysis (1,Vector [0,0,1,1,1]),Analysis (1,Vector [1,1,1,0,0]),Analysis (1,Vector [1,0,0,0,0]),Analysis (1,Vector [1,1,0,0,1]),Analysis (1,Vector [0,0,0,1,0]),Analysis (1,Vector [0,1,0,1,0])]

-- >>> analyseAll @5 $ diagnostic <$> example
-- Analysis (12,Vector [7,5,8,7,5])

-- >>> solve2 <$> parse @12 puzzleData
-- Right (Just 4245351)

-- >>> solve2 <$> testData
-- [Nothing,Just 0,Just 230]

testData :: [[Diagnostic 5]]
testData =
  fmap
    (fmap $ Diagnostic @5)
    [ [0b00001 :: Int],
      [0b00000, 0b10000, 0b11111],
      example
    ]

example =
  [ 0b00100,
    0b11110,
    0b10110,
    0b10111,
    0b10101,
    0b01111,
    0b00111,
    0b11100,
    0b10000,
    0b11001,
    0b00010,
    0b01010
  ]

exampleDiagnostics :: [Diagnostic 5]
exampleDiagnostics = diagnostic <$> example
