{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Day5.Day5Spec
  ( spec
  )
where
-- import qualified Data.Text as T
-- import           Data.Text (Text)
import           Data.Char                      ( toUpper
                                                , toLower
                                                )
import           SpecHelper

alchemy :: String -> String
alchemy = reverse . foldl react ""

react :: String -> Char -> String
react rest ch = go (ch:rest)
  where
    go :: String -> String
    go [c] = [c]
    go s@(c1 : (c2 : rest)) 
      | willReact c1 c2 = rest
      | otherwise       = s

willReact :: Char -> Char -> Bool
willReact c1 c2 = go c1 c2 || go c2 c1
  where go c1 c2 = (toUpper c1 == c2) && (toLower c2 == c1)

spec :: Spec
spec = describe "Day 5" $ do
  context "alchemy" $ allSamplesShouldBe
    alchemy
    [Raw "aA" "", Raw "abBA" "", Raw "abAB" "abAB", Raw "aabAAB" "aabAAB"]
  context "react" $ allSamplesShouldBe
    (uncurry react)
    [Raw ("a", 'A') ""
    ,Raw ("a", 'B') "Ba"]
  context "willReact" $ allSamplesShouldBe
    (uncurry willReact)
    [Raw ('a', 'A') True 
    ,Raw ('A', 'a') True 
    ,Raw ('a', 'a') False
    ,Raw ('a', 'B') False]

