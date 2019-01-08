{-# LANGUAGE OverloadedStrings #-}
module Day3.Day3Spec
    ( spec
    )
where
import           Data.Maybe                     ( catMaybes, listToMaybe )
import           Data.Foldable                     ( find)
import qualified Data.Foldable                     as Foldable
import           Data.Attoparsec.Text          as P
                                         hiding ( take )
import qualified Data.Attoparsec.Text           ( Parser )
import qualified Data.Map                      as Map
import qualified Data.Map.Lazy                 as LazyMap
import           Data.Map                      (Map)
import           Data.Monoid                   (Sum(..))          
import           SpecHelper

import qualified Data.Set                      as S
import           Data.Set                       ( Set )
import           Day3.Input                     ( puzzleData3 )

data Rect a = Rect { x :: Int, y :: Int, w :: Int, h :: Int, ident :: a}
  deriving (Eq, Show, Ord)

type Coord = (Int, Int)

type Grid a = Map Coord a

rectP :: Parser a -> Parser (Rect a)
rectP pa = do
    a <- pa
    x <- string " @ " *> P.decimal
    y <- char ',' *> P.decimal
    w <- string ": " *> P.decimal
    h <- char 'x' *> P.decimal
    return $ Rect x y w h a

parser :: Parser a -> Parser [Rect a]
parser pa = sepBy (rectP pa) endOfLine

identListP :: Parser [Int]
identListP = do
    a <- char '#' *> P.decimal
    return [a]

overlay :: Monoid a => Set (Rect a) -> Set (Rect a)
overlay = const S.empty

toGrid :: Rect a -> Grid a
toGrid r@(Rect x y w h a) =  Map.fromList [((xG,yG),a) | xG <- [x .. (x+w)-1], yG <- [y .. (y+h)-1]]

sumGrid :: Monoid a => Grid a -> Grid a -> Grid a
sumGrid = Map.unionWith mappend

solution :: (a -> Sum Int ) -> Grid a -> Int
solution l g = getSum $ foldMap l g 

class FiniteLength a where
    finiteLength :: a -> Int
instance FiniteLength (Set a) where
    finiteLength = S.size
instance FiniteLength [a] where
    finiteLength = length 
            
inSolution :: FiniteLength a => a -> Sum Int
inSolution as | finiteLength as >= 2 = Sum 1
inSolution _ = Sum 0

solve :: (Monoid a, FiniteLength a) => [Rect a] -> Int
solve rs = solution inSolution $ merge $ toGrid <$> rs
    where 
        merge = foldr sumGrid Map.empty

-- puzzle 2
solution2 :: (Foldable t) => (t a -> Bool) -> Grid (t a)-> Maybe a
solution2 f g = do
    result <- find f g
    listToMaybe $ Foldable.toList result

inSolution2 :: (FiniteLength a) => a -> Bool
inSolution2 as | finiteLength as == 1 = True
inSolution2 _ = False

inSolution2b :: Eq a => Grid a -> Grid a -> Bool
inSolution2b total candidate = candidate `LazyMap.isSubmapOf` total

maybeA :: Foldable t => Map k (t a)-> Maybe a
maybeA g = do
    ts <- listToMaybe $ Map.elems g
    listToMaybe $ Foldable.toList ts

solve2 :: (Eq (t a), Monoid (t a), Foldable t, FiniteLength (t a)) => [Rect (t a)] -> Maybe a
solve2 rs = do
    bestM <- find (inSolution2b total) grids
    maybeA bestM

  where 
    grids = toGrid <$> rs
    total = merge grids
    merge = foldr sumGrid Map.empty

spec :: Spec
spec = describe "Soemthing" $ 
  context "Puzze1" $ do
    let setOne    = S.singleton 1
        setTwo    = S.singleton 2
        setOneTwo = setOne <> setTwo    
    context "solve" $
        allSamplesShouldBe
            solve
            [ Summarised "overlapping (5x5)"
                [Rect 0 0 10 10 setOne, Rect 5 5 15 15 setTwo]
                25
            , Summarised
                "disjoint"
                [Rect 0 0 10 10 setOne, Rect 50 50 10 10 setTwo]
                0
            , Summarised
                "nested (8x8) in (10x10)"
                [Rect 0 0 10 10 setOne, Rect 1 1 8 8 setTwo]
                64
            ]
    context "grids" $ do
        let rect1 = Rect 0 0 1 1 setOne
            rect2 = Rect 0 0 1 1 setTwo
            grid1 = toGrid rect1
        context "submaps of each other" $
          allSamplesShouldBe (`LazyMap.isSubmapOf` grid1)
              [ Raw grid1 True ]
        context "equality" $ 
            allSamplesShouldBe (== grid1)
            [ Raw (toGrid rect2) False 
            , Raw grid1 True]             

    context "puzzleData" $ do
      let parsedInput = parseOnly (parser identListP) puzzleData3
      it "should parse" $ take 5 <$> parsedInput `shouldBe` Right
        [ Rect 861 330 20 10 [1]
        , Rect 491 428 28 23 [2]
        , Rect 64  746 20 27 [3]
        , Rect 406 769 25 28 [4]
        , Rect 853 621 17 26 [5]
        ]

      it "should be solved" 
        $ solve
        <$> parsedInput
        `shouldBe` Right 104439

      it "should be solved2" 
        $ solve2
        <$> parsedInput
        `shouldBe` Right (Just 701)



