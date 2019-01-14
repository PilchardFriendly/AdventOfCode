{-# LANGUAGE GADTs #-}
module SpecHelper 
    ( module Test.Hspec,
      shouldAllBe,
      Sample(Raw, Annotated, Parsed, Summarised),
      allSamplesShouldBe
    ) where

import Test.Hspec
import Data.String (IsString)
import Prelude hiding(read)
import qualified Text.Read as TR
import Data.String.Combinators
  
shouldAllBe :: (IsString a, Show a, Eq b, Show b)
          => (a -> b)
          -> [(a, b)]
          -> Spec
shouldAllBe f = mapM_ (\(input, v) -> it (show input ++ " -> " ++ show v) $ f input `shouldBe` v)

data Sample a b c where
  Raw :: (Show b, Show c, Eq c) => b -> c -> Sample String b c
  Annotated :: (IsString a, Show a, Show b, Show c, Eq c) => a -> b -> c -> Sample a b c
  Parsed :: (Show b, TR.Read b, Show c, Eq c) => String -> c -> Sample String b c 
  Summarised :: (IsString a, Show a, Show c, Eq c) => a -> b -> c -> Sample a b c

toSpec ::  (b -> c) -> Sample a b c -> Spec
toSpec f (Raw b c) = mkSpec f (show b) b c
toSpec f (Annotated a b c) = mkSpec f (doubleQuotes (show a) ++ show b) b c
toSpec f (Parsed a c) = mkSpec f (show b) b c
  where b = TR.read a
toSpec f (Summarised a b c) = mkSpec f (show a ++ " ..." ) b c

mkSpec :: (Show c, Eq c) => (b -> c) -> String -> b -> c -> Spec
mkSpec f prefix b c = it ( prefix ++ " -> " ++ show c) $ f b `shouldBe` c

allSamplesShouldBe :: (b -> c) -> [Sample a b c] -> Spec
allSamplesShouldBe f = mapM_ (toSpec f)
