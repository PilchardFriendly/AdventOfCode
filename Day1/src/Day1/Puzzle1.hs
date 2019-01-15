{-# LANGUAGE QuasiQuotes, OverloadedStrings, ExtendedDefaultRules #-}
{- HLINT ignore "Unused LANGUAGE pragma" -}
module Day1.Puzzle1
  ( parseInput
  , puzzleData
  , solution
  , solve
  , solve2
  , solution2
  , scanRepeating
  )
where

import           Data.Attoparsec.Text
import           Data.List                      (
                                                 scanl
                                                )
import qualified Data.Set                      as S
import           Data.Text                      ( Text )

import           Day1.Input


parseInput :: Text -> Either String [Int]
parseInput = parseOnly inputP

lineP = signed decimal
inputP = sepBy lineP endOfLine


solution :: Text -> Either String Int
solution = solving solve

solution2 :: Text -> Either String (Maybe Int)
solution2 = solving solve2

solving :: ([Int] -> a) -> Text -> Either String a
solving f input = fmap f (parseInput input)

solve :: [Int] -> Int
solve = sum

solve2 :: [Int] -> Maybe Int
solve2 xs = scanRepeating $ freq $ cycle xs
  where freq = scanl (+) 0

scanRepeating :: [Int] -> Maybe Int
scanRepeating = go S.empty
 where
  go s [] = Nothing
  go s (a : as) | S.member a s = Just a
                | otherwise    = go (S.insert a s) as
