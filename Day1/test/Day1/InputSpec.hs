
module Day1.InputSpec (spec) where
    
import SpecHelper
import qualified Data.Text as T
import Data.Text (Text)
import Text.InterpolatedString.Perl6
import Day1.Puzzle1
import qualified Data.Set as S
import Data.Set (Set)

spec :: Spec
spec  = do
  describe "parsing" $ do 
    it "should parse '+1\\n+2'" $ parseInput (T.pack "+1\n+2") `shouldBe` Right [1, 2]

    it "should parse '-1\\n+2" $ parseInput (T.pack "-1\n+2") `shouldBe` Right [-1, 2]

  describe "solving" $ do
    it "should solve 1,2,3 = 6" $ solve [1,2,3] `shouldBe` 6

  describe "scanRepeating" $ 
    it "should detect [1,2,3,1] -> 1" $ scanRepeating [1,2,3,1] `shouldBe` [1]
  describe "solving 2" $ do
    it "should solve 1,-1 -> 0" $ solve2 [1, -1] `shouldBe` Just 0
    it "should solve +3, +3, +4, -2, -4 -> 10" $ solve2 [3,3,4,-2,-4] `shouldBe` Just 10
    it "should solve -6, +3, +8, +5, -6-> 5" $ solve2 [-6,3,8,5,-6] `shouldBe` Just 5
    it "should solve +7, +7, -2, -7, -4 -> 14" $ solve2 [7,7,-2,-7,-4] `shouldBe` Just 14

  describe "puzzleData" $ do
    it "should have solution" $ solution puzzleData `shouldBe` Right 576

    it "should have solution 2" $ solution2 puzzleData `shouldBe` Right (Just 77674)

    it "should begin +13\\n+12" $ T.take 8 puzzleData `shouldBe` T.pack "+13\n-12\n"

    it "should parse Day1 data" $
        assert $ select $ parseInput puzzleData
        where 
            select = fmap $ take 5
            assert a = a `shouldBe` Right [13,-12,-14, 19, -13]

            

        
    
  