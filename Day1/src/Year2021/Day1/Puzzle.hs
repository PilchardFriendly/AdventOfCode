{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Year2021.Day1.Puzzle where

import Data.Attoparsec.Text (Parser, decimal, endOfLine, many', parseOnly, sepBy, sepBy1, signed)
import Data.List (group)
import Data.Text (Text)
import Year2021.Day1.Input (puzzleData)

data Day1 = Day1

newtype Depth = Depth Int
  deriving (Show, Eq, Ord, Num)

run :: Text -> Either String Int
run t = solve <$> parse t

parser :: Parser [Depth]
parser = sepBy1 (Depth <$> signed decimal) endOfLine

parse :: Text -> Either String [Depth]
parse = parseOnly parser

-- >>> parse "123\n456\n"
-- Right [Depth 123,Depth 456]
solve :: [Depth] -> Int
solve ds = go $ zip ds $ tail ds
  where
    go :: [(Depth, Depth)] -> Int
    go = countIncreasing

solve2 :: [Depth] -> Int
solve2 ds = go $ zip windows $ tail windows
  where
    windows = zipWith3 sum' ds (tail ds) $ (tail . tail) ds
    go :: [(Depth, Depth)] -> Int
    go ds = countIncreasing ds
    sum' :: Num a => a -> a -> a -> a
    sum' x y z = x + y + z

countIncreasing :: Ord a => [(a, a)] -> Int
countIncreasing [] = 0
countIncreasing xs = sum $ isInc <$> xs
  where
    isInc :: Ord a => (a, a) -> Int
    isInc (a, b) | a < b = 1
    isInc _ = 0

-- >>> solve <$> parse puzzleData
-- Right 1616
-- >>> solve <$> testData
-- [0,1,3,3]

-- >>> solve2 <$> parse puzzleData
-- Right 1645

-- >>> solve2 <$> testData
-- [0,0,1,2]

testData =
  fmap Depth
    <$> [ [0],
          [1, 2],
          [1, 2, 3, 4],
          [1, 2, 4, 3, 5]
        ]
