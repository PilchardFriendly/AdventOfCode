{-# LANGUAGE OverloadedStrings #-}

module Day2Spec
  ( spec
  ) where

import Data.Char (isAlpha)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

import Control.Arrow
import Data.Attoparsec.Text as P
import qualified Data.Attoparsec.Text (Parser)
import Foreign.Marshal.Utils as FMU
import Data.Monoid
import Data.Coerce
import Data.Foldable as Fold
import Day2Input

import SpecHelper


type BoxId = Text

type HasCount2 = Bool

type HasCount3 = Bool

type BoxIdSet = [BoxId]

type BoxIdProjection = (HasCount2, HasCount3)

type ChecksumFunc = BoxIdSet -> Checksum

type Checksum = Product Int

boxProject :: BoxId -> BoxIdProjection
boxProject box = count $ countLetters box
  where
    countLetters :: BoxId -> MultiSet Char
    countLetters = T.foldr MS.insert MS.empty
    count :: MultiSet Char -> BoxIdProjection
    count = MS.foldOccur count' (False, False)
    count' :: Char -> Int -> BoxIdProjection -> BoxIdProjection
    count' _ 2  = first $ const True
    count' _ 3 = second $ const True
    count' _ _ = id

checksum :: BoxIdSet -> Checksum
checksum = checksum' . map boxProject

checksum' :: (Foldable f, Functor f) => f BoxIdProjection -> Checksum
checksum' = coerce . uncurry (*) . fold . fmap cnv
  where
    cnv :: BoxIdProjection -> (Sum Int, Sum Int)
    cnv = FMU.fromBool *** FMU.fromBool


solve :: BoxIdSet -> Checksum
solve = checksum

inputParser :: Parser Text
inputParser = do
  ws <- P.skipWhile $ not . isAlpha
  P.takeWhile isAlpha

parser :: Parser [Text]
parser = sepBy inputParser endOfLine

parsedInput :: Either String [Text]
parsedInput = parseOnly parser puzzle2Data

spec :: Spec
spec =
  describe "Day2" $ do
    context "an empty set of BoxId" $ do
      it "should have a zero checsum" $ solve [] `shouldBe` 0
    context "the example set" $ do
      let example =
            map
              T.pack
              [ "abcde"
              , "bababc"
              , "abbcde"
              , "abcccd"
              , "aabcdd"
              , "abcdee"
              , "ababab"
              ]
      it "should have a checksum of 12" $ solve example `shouldBe` 12
    context "checksum calculations" $ do
      it "calulates [(True,True),(True,True)]" $
        checksum' [(True, True), (True, True)] `shouldBe` 4
      it "calulates [(True,True),(False,False)]" $
        checksum' [(True, True), (False, False)] `shouldBe` 1
      it "calulates [(True,True),(False,False), (True, False)]" $
        checksum' [(True, True), (False, False), (True, False)] `shouldBe` 2
    context "/projection" $ do
      it "abcdef should be (False, False)" $
        boxProject (T.pack "abcdef") `shouldBe` (False, False)
      it "bababc should be (True, True)" $
        boxProject (T.pack "bababc") `shouldBe` (True, True)
      it "aabcdd should be (True, False)" $
        boxProject (T.pack "aabcdd") `shouldBe` (True, False)
      it "abcccd should be (False, True)" $
        boxProject (T.pack "abcccd") `shouldBe` (False, True)
      it "aabcde should be (True, False)" $
        boxProject (T.pack "aabcde") `shouldBe` (True, False)
      it "abcdee should be (True, False)" $
        boxProject (T.pack "abcdee") `shouldBe` (True, False)
      it "ababab should be (False, True)" $
        boxProject (T.pack "ababab") `shouldBe` (False, True)
    context "the input data" $ do
      it "should start with" $
        fmap (Prelude.take expectedLength) parsedInput `shouldBe` Right expected
      it "should end with" $
        fmap (reverse . Prelude.take expectedLength . reverse) parsedInput `shouldBe`
        Right expectedTail
      it "should have 250 lines" $ fmap length parsedInput `shouldBe` Right 250
      it "should have a solution 4 with" $
        fmap solve parsedInput `shouldBe` Right 7105
  where
    expectedLength = length expected
    expected :: [Text]
    expected =
      [ "omlvgpokxfnctqyersabjwzizp"
      , "omlvtdhxxflctqyersabjwziup"
      , "omlvgdakxfnctqyersabzmziup"
      , "omlvgdhkxfnchqyersarjwsiup"
      , "omlvgdnkxfnctqyersabhwziuq"
      ]
    expectedTail :: [Text]
    expectedTail =
      [ "omlvgdhkxfncaqyersabwwzoup"
      , "omlvgdhkxfncjqyersanjwfiup"
      , "omlvgdhkwfnctqyersqbjwziux"
      , "omrvgdhjxfnctqyeksabjwziup"
      , "omlvgdhkxfnctpyersaftwziup"
      ]
