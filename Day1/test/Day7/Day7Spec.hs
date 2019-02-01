{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Day7.Day7Spec (spec) where
import Data.Attoparsec.Text
import Control.Applicative
import Control.Arrow ((***))
import Control.Monad (mfilter)
import SpecHelper
import Data.List (sort)
import qualified Data.Set as S
import Data.Set (Set)
import Data.Char
import qualified Data.Map as M
import Data.Map (Map)
import Data.Graph

import Day7.Input
import ActivityQueue
import RFunctor
import Distinct


type Part = Char
type Instruction = (Part, Part)
data Dependencies = Part :-> [Part]
  deriving (Show, Eq)
mkRootDependencies :: Part -> Dependencies
mkRootDependencies p = p :-> []

instructionP :: Parser Instruction
instructionP = go <$> (string "Step " *> letter )
                  <*> (string " must be finished before step " *> letter <* string " can begin.")
    where
      go a b = (b,a)

      
puzzleP :: Parser [Instruction]
puzzleP = instructionP  `sepBy` (endOfLine <|> endOfInput) 

directDeps :: [Instruction] -> [Dependencies]
directDeps = fmap  make . M.toList . foldr go M.empty
    where
      make (p, ps) =p :-> S.toList ps
      go :: Instruction -> Map Part (Set Part) -> Map Part (Set Part)
      go (f, s) = M.insertWith (<>) f (S.singleton s)


toGraph :: [Dependencies] -> [SCC Dependencies]
toGraph = stronglyConnComp . map go
  where
    go :: Dependencies -> (Dependencies, Int, [Int])
    go i@ (from :-> to) = (i, toKey from, toKey <$> to)
    -- toKey n = (ord n)
    toKey n = - (ord n)

solution :: [Instruction] -> [[Dependencies]]
solution = map flattenSCC . toGraph . directDeps  -- OMG! topological sort....

findRoots :: [Dependencies] -> [Part]
findRoots = sort . S.toList . uncurry (flip S.difference) . (S.fromList *** S.fromList) . foldl go ([], [])
  where
    go :: ([Part],[Part]) -> Dependencies -> ([Part],[Part])
    go (d, ds) (d' :-> ds') = (d':d, ds'++ds)
          

solve :: [Instruction] -> String
solve = tmp  . solution
  where
    -- tmp = go . concat
 
    tmp = _distincts.  go3 . concat
    go3 :: [Dependencies] -> Distinct Part
    go3 ds = finish $ foldl (flip  go3') (emptyAQ, mempty) ((mkRootDependencies <$> findRoots ds) ++ ds )
    -- Append the remaining active elements
    finish :: (ActivityQueue Part, Distinct Part) -> Distinct Part
    finish (activity, s) = foldlAscAQ (<>) s $ fmapR mkDistinct activity

    go3' :: Dependencies -> (ActivityQueue Part, Distinct Part) -> (ActivityQueue Part, Distinct Part)
    go3' (d :-> []) (q, s) = (queueAQ d q, s)
    go3' dep@(d :-> (next:ds)) st@(q, s) 
      | memberAQ next q = go3' dep (q', s')
      | otherwise = go3' (d :-> ds) (q, s <> mkDistinct next)
      where 
        (minp', q') = dequeueAQ q -- minp' could be less than next
        s' = (s <>) $ (maybe mempty mkDistinct) $ mfilter (next >=)  minp'


spec :: Spec
spec = describe "Sleigh" $ do
  let exampleText = [here|
Step C must be finished before step A can begin.
Step C must be finished before step F can begin.
Step A must be finished before step B can begin.
Step A must be finished before step D can begin.
Step B must be finished before step E can begin.
Step D must be finished before step E can begin.
Step F must be finished before step E can begin.    
|]
  context "parsing" $ do
    context "example" $ do 
      it "should parse to C <- A, C<- F, A <- B, A<-D, B <- E, D <-E, F <- E" $
         (parseOnly puzzleP) exampleText `shouldBe` Right [('A','C'),('F','C'),('B','A'),('D','A'),('E','B'),('E','D'),('E','F')]
  context "solving" $ do
    context "multiroot" $ do
      let cadb = [('C','A'), ('D','B')]
      it "should map A->C, B->D to ABCD" $ solve cadb `shouldBe` "ABCD"
      it "should find roots for A->C, B->D" $ (findRoots . concat . solution ) cadb `shouldBe` "AB"
    context "example" $ do
      let subject = (parseOnly puzzleP exampleText)
      it "should solve to CABDFE" $ (solve <$> subject) `shouldBe` Right "CABDFE"
    context "real thing" $ do
      let subject = (parseOnly puzzleP puzzleData)
      it "should solve to ACHOQRXSEKUGMYIWDZLNBFTJVP" $ (solve <$> subject) `shouldBe` Right "ACHOQRXSEKUGMYIWDZLNBFTJVP"
      it "should not solve to ACHOQRXSKEUGMYWIDZLNTBFJVP" $ (solve <$> subject) `shouldNotBe` Right "ACHOQRXSKEUGMYWIDZLNTBFJVP"
      it "should find roots" $ (findRoots . concat . solution) <$> subject `shouldBe` Right "AHQX"
  -- context  "solving 2" $ do
  context "directDeps" $ do
    it "should world for a single one" $ (directDeps) [('A', 'C')] `shouldBe` ['A' :-> ['C']]