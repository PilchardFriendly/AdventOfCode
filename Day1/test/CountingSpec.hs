module CountingSpec (spec) where
import Data.Map
import Counting
import SpecHelper

spec :: Spec
spec = describe "Counting" $ do
    context "counts" $ do
        it "should 5..6 -> [(5,1)]" $ counts [5] `shouldBe` fromList [(5,1)]
        it "should [5,6,5] -> [(5,2), (6,1)]" $ counts [5, 6, 5] `shouldBe` fromList [(5,2), (6, 1)]
  