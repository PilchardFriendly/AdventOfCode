{-# LANGUAGE TypeFamilies #-}
module Distinct where
import           Data.Ord                       ( comparing )
import           RFunctor

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
        MkDistinct (\a -> fa a || fb a, as ++ filter (not . fa) bs)


instance Monoid (Distinct a) where
    mempty = MkDistinct (const False, [])


instance RFunctor Distinct where
    type RFunctorConstraint Distinct = Ord
    fmapR f (MkDistinct (af, as)) = foldMap mkDistinct $ f <$> as

-- instance Applicative Distinct where
--   pure a = mkDistinct a

mkDistinct :: Eq a => a -> Distinct a
mkDistinct a = MkDistinct ((a ==), [a])
