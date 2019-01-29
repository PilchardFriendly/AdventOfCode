{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Day7.Day7Spec (spec) where
import Data.Attoparsec.Text
import Control.Applicative
import Control.Arrow ((***))
import SpecHelper
import Data.List (sort)
import Data.Maybe ( mapMaybe )
import qualified Data.Set as S
import Data.Set (Set)
import Data.Char
import Data.Ord
import qualified Data.Map as M
import Data.Semigroup (Max(..))
import Data.Map (Map)
import Data.Graph
import GHC.Exts (groupWith, the)
import Data.PQueue.Min (MinQueue(..))
import qualified Data.PQueue.Min as PQMin
import Day7.Input

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
      go (f, s) m = M.insertWith (<>) f (S.singleton s) m


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
          
-- partial ordering
newtype Distinct a = MkDistinct (a -> Bool, [a])
instance Eq a => Eq (Distinct a ) where
  a == b = _distincts a == _distincts b
instance Ord a => Ord (Distinct a) where
  compare = comparing _distincts
_distinctF :: Distinct a -> (a -> Bool)
_distinctF (MkDistinct d) = fst d
_distincts :: Distinct a -> [a] 
_distincts (MkDistinct d) = snd d

instance Semigroup (Distinct a) where
  (MkDistinct (fa, as)) <> (MkDistinct (fb, bs)) = 
    MkDistinct ((\a -> (fa a || fb a)), as ++ (filter (not.fa) bs))

instance Monoid (Distinct a) where
  mempty = MkDistinct (const False,[])

mkDistinct :: Eq a => a -> Distinct a
mkDistinct a = MkDistinct ((a ==),[a])

thing:: Dependencies -> Map Part Int -> Int
thing (p :-> ps) m = 1+ (getMax . (Max 0 <>) . foldMap Max . (mapMaybe $ flip M.lookup m) ) ps

things ::forall a k v. (Ord k ) => (a -> Map k v -> (k,v)) -> [a] -> Map k v -> Map k v
things f as m0 = foldl go m0 as
  where
    go :: Map k v -> a -> Map k v
    go m a = M.insert k v m
      where
        (k,v) = f a m      


popQ :: (Ord p, Eq p) => p -> (MinQueue p,Set p,Distinct p)  -> (MinQueue p,Set p,Distinct p)
popQ p st@(q,existing,s) = case PQMin.minView q of
 Just (p', q') | p >= p' -> (q', S.delete p' existing, s <> mkDistinct p')
 _ -> st

solve :: [Instruction] -> String
solve = tmp  . solution
  where
    -- tmp = go . concat
 
    tmp = _distincts.  go3 . concat
    go3 :: [Dependencies] -> Distinct Part
    go3 ds = finish $ foldl (flip  go3') (PQMin.empty, mempty, mempty) ((mkRootDependencies <$> findRoots ds) ++ ds )
    -- Append the remaining active elements
    finish :: (MinQueue Part, Set Part, Distinct Part) -> Distinct Part
    finish (active, _, s) = PQMin.foldlAsc (<>) s $ PQMin.map mkDistinct active

    go3' :: Dependencies -> (MinQueue Part, Set Part, Distinct Part) -> (MinQueue Part, Set Part, Distinct Part)
    go3' (d :-> []) (active, activeElems, s) = (PQMin.insert d active, S.insert d activeElems, s)
    go3' dep@(d :-> (next:ds)) st@(q, qElems, s) 
      | S.member next qElems = go3' dep (popQ next st)
      | otherwise = go3' (d :-> ds) (q, qElems, s <> mkDistinct next)

    --   tmp = go2 . concat
    -- Aborted Attempt2
    go2 :: [Dependencies] -> [Part]
    go2 ds = concat . reverse . fmap snd $ [(the key, value) | (value, key) <- allScores,
                                                then group by key using groupWith]
      where
        allScores = M.toList $ things doobris ds ( rootScores ds)
        rootScores = M.fromList . fmap (,1) . findRoots
    doobris :: Dependencies -> Map Part Int -> (Part, Int)
    doobris d@(p :-> ps) m = (p, thing d m)


    --Abort attempt 1
    go ds = _distincts.snd $ foldl (flip step) ((findRoots ds),mempty) ds


    step :: Dependencies -> (String,Distinct Part) -> (String,Distinct Part)
    step (d :-> ds) st@(roots, sofar) = stepPart (ds ++ [d]) st
    
    stepPart :: [Part] -> (String,Distinct Part) -> (String, Distinct Part)
    stepPart [] st = st
    stepPart pts ([], sofar) = ([], foldl (<>) sofar (mkDistinct <$> pts))
    stepPart parts@(pt:pts) st@(rts@(r:rs), sofar) | pt == r  = stepPart pts (rs,  sofar <> mkDistinct pt) -- short circuit
                                             | pt > r         = stepPart parts (rs, sofar <> mkDistinct r)
                                             | otherwise      = stepPart pts (rts, sofar <> mkDistinct pt)


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

  context "directDeps" $ do
    it "should world for a single one" $ (directDeps) [('A', 'C')] `shouldBe` ['A' :-> ['C']]