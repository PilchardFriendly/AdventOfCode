{-# LANGUAGE OverloadedStrings #-}
module Day4.Day4Spec (
    spec
    ) 
where

import SpecHelper

spec :: Spec
spec = describe "Something" $ 
  context "yolo" $ 
    it "basics" $ 1 `shouldBe` 1