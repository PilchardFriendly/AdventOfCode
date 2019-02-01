{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module ActivityQueue where

import           Control.Arrow                  ( (^>>) )
import           Data.Ord
import           Data.PQueue.Min                ( MinQueue )
import           Data.Set
import           Data.Semigroup
import           RFunctor

import qualified Data.Set                      as S
import qualified Data.PQueue.Min               as PQMin

data ActivityQueue a where
    ActivityQueue ::Ord a => MinQueue a -> Set a -> ActivityQueue a
deriving instance Eq (ActivityQueue a)
deriving instance Show a => Show (ActivityQueue a)


instance RFunctor ActivityQueue where
    type RFunctorConstraint ActivityQueue = Ord
    fmapR = mapAQ

emptyAQ :: Ord a => ActivityQueue a
emptyAQ = ActivityQueue PQMin.empty mempty

dequeueAQ :: ActivityQueue a -> (Maybe a, ActivityQueue a)
dequeueAQ st@(ActivityQueue q existing) = case PQMin.minView q of
    Just (p', q') -> (Just p', ActivityQueue q' (S.delete p' existing))
    Nothing       -> (Nothing, st)

queueAQ :: a -> ActivityQueue a -> ActivityQueue a
queueAQ p (ActivityQueue q existing) =
    ActivityQueue (PQMin.insert p q) (S.insert p existing)

foldlAscAQ :: Ord b => (a -> b -> a) -> a -> ActivityQueue b -> a
foldlAscAQ f z (ActivityQueue q _) = PQMin.foldlAsc f z q

foldrAscAQ :: Ord b => (b -> a -> a) -> a -> ActivityQueue b -> a
foldrAscAQ f z (ActivityQueue q _) = PQMin.foldrAsc f z q

mapAQ :: (Ord a, Ord b) => (a -> b) -> ActivityQueue a -> ActivityQueue b
mapAQ f = foldlAscAQ (flip (f ^>> queueAQ)) emptyAQ

memberAQ :: a -> ActivityQueue a -> Bool
memberAQ a (ActivityQueue _ existing) = S.member a existing
