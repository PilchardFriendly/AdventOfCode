{-# LANGUAGE OverloadedStrings #-}

module Day2.Day2Spec
  ( spec
  ) where

import Data.Maybe (catMaybes)
import Data.Char (isAlpha, ord)
import Data.Int (Int32, Int8)
import qualified Data.Bits as B
import Data.Bits (Bits, zeroBits, setBit)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import qualified Data.Set as S
import Data.Set (Set)
import Data.Text (Text,pack)
import qualified Data.Text as T

import Control.Arrow
import Data.Attoparsec.Text as P hiding (take)
import qualified Data.Attoparsec.Text (Parser)
import Foreign.Marshal.Utils as FMU
import Data.Monoid
import Data.Coerce
import Data.Foldable as Fold
import Day2.Input

import SpecHelper
import Data.String.Combinators (doubleQuotes)


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

newtype Discriminator a = MkDiscriminator (Set a)
instance Ord a => Eq ( Discriminator a) where
  (==) (MkDiscriminator a) (MkDiscriminator b) = not $ S.disjoint a b

-- discriminate :: Text -> Maybe (Discriminator Int32)
-- discriminate s = catMaybes mapM_ maybeAscii
--   where
--     maybeAscii :: Char -> Maybe Int32
indices :: Text -> Set Int8
indices s = S.fromList $ catMaybes $ maybeAscii . ord <$> T.unpack s
  where
    maybeAscii :: Int -> Maybe Int8
    maybeAscii c | c >= 97 && c <= 122 = Just $ fromIntegral $ c - 97
    maybeAscii _ = Nothing

bitIndex :: Foldable f => f Int8 -> Int32
bitIndex = foldl step zeroBits 
    where 
      step :: Int32 -> Int8 -> Int32
      step bs n = setBit bs $ bitNum n
      bitNum :: Int8 -> Int
      bitNum = fromIntegral

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
              T.words $ pack "abcde bababc abbcde abcccd aabcdd abcdee ababab"
      it "should have a checksum of 12" $ solve example `shouldBe` 12
    context "checksum calculations" $
      allSamplesShouldBe (getProduct . checksum')
        [Raw [(True, True), (True, True)] $ 4
        ,Raw [(True, True), (False, False)] $ 1
        ,Raw [(True, True), (False, False), (True, False)] $ 2]

    context "/projection" $
      allSamplesShouldBe boxProject
        [Raw (pack "abcdef") $ (False, False)
        ,Raw (pack "bababc") $ (True, True)
        ,Raw (pack "aabcdd") $ (True, False)
        ,Raw (pack "abcccd") $ (False, True)
        ,Raw (pack "aabcde") $ (True, False)
        ,Raw (pack "abcdee") $ (True, False)
        ,Raw (pack "ababab") $ (False, True)]

    context "puzzle 2" $
      context "indices" $
        allSamplesShouldBe indices
         [ Raw (pack "abc") $ S.fromList [0,1,2]]

    context "the input data" $ do
      let 
        puzzle2Example :: [Text]
        puzzle2Example = [ "abcde", "abcfe"]
    
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
      it "should start with" $
        (take expectedLength) <$> parsedInput `shouldBe` Right expected
      it "should end with" $
        (reverse . take expectedLength . reverse) <$> parsedInput `shouldBe` Right expectedTail
      it "should have 250 lines" $ fmap length parsedInput `shouldBe` Right 250
      it "should have a solution 4 with" $ solve <$> parsedInput `shouldBe` Right 7105

