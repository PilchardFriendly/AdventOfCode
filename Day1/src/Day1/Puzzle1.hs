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
import           Data.List                      ( find
                                                , scanl
                                                )
import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import qualified Data.Text                     as T
import           Data.Text                      ( Text )

import           Data.Maybe                     ( listToMaybe
                                                , mapMaybe
                                                )
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
solve2 xs = listToMaybe $ scanRepeating $ scanFrequencies $ cycle xs
  where scanFrequencies = scanl (+) 0

scanRepeating :: [Int] -> [Int]
scanRepeating xs = mapMaybe fst (scanl merge (Nothing, S.empty) xs)
 where
  merge :: (Maybe Int, Set Int) -> Int -> (Maybe Int, Set Int)
  merge (_, s) a | S.member a s = (Just a, s)
                 | otherwise    = (Nothing, S.insert a s)

-- [+1 -2 +3]  => 1 -1 2 3 1