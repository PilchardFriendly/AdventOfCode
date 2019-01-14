{-# LANGUAGE OverloadedStrings #-}
module Day1.Day1Spec
  ( spec
  )
where

import           SpecHelper
import qualified Data.Text                     as T
import           Day1.Puzzle1
import           Data.String.Combinators        ( doubleQuotes )
spec :: Spec
spec = do
  describe "parseInput" $ allSamplesShouldBe
    parseInput
    [ Parsed (doubleQuotes "+1\n+2") $ Right [1, 2]
    , Parsed (doubleQuotes "-1\n+2") $ Right [-1, 2]
    ]

  describe "solve" $ allSamplesShouldBe solve [Raw [1, 2, 3] 6, Raw [1, 2] 3]

  describe "scanRepeating"
    $ allSamplesShouldBe scanRepeating [Raw [1, 2, 3, 1] [1]]

  describe "solving 2" $ allSamplesShouldBe
    solve2
    [ Raw [1, -1] $ Just 0
    , Raw [3, 3, 4, -2, -4] $ Just 10
    , Raw [-6, 3, 8, 5, -6] $ Just 5
    , Raw [7, 7, -2, -7, -4] $ Just 14
    ]


  describe "puzzleData" $ do
    it "should have solution" $ solution puzzleData `shouldBe` Right 576

    it "should have solution 2" $ solution2 puzzleData `shouldBe` Right
      (Just 77674)

    it "should begin +13\\n+12" $ T.take 8 puzzleData `shouldBe` T.pack
      "+13\n-12\n"

    let expected = Right [13, -12, -14, 19, -13]
        act      = parseInput puzzleData
        select   = (take 5 <$>)

    it "should parse Day1 data" $ select act `shouldBe` expected






