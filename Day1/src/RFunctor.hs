{-# LANGUAGE ConstraintKinds, TypeFamilies #-}
module RFunctor where
   
import GHC.Exts (Constraint)

{- Borrowed from RMonad (which doesn't compile on LTS 19.1. 
   We define an associated type family (RFunctorConstraint) which maps
   an a -> Constraint for the f.  So for set-like things we'd
   go 
   ``` instance RFunctor Setlike where
          type RFunctorConstraint Setlike = Ord
          fmapR = Setlike.map ```
   which works because Ord is injective over a.  

   Why do we want this?  Well it turns out that Functor doesn't let us preserve
   constraints across the fmap - it's literally f a -> f b, not (Ord a,Ord b) => f a -> f b.
   
   Technically Functor is a type-level function from Hask -> Hask (e.g. at the type level Functor f :: (a::Hask) -> (f a ::Hask), 
   but things in a setlike are a subtype of Hask (Ord o :: (a::Hask)  -> (o a :: OrdHask)), 
   so Functor can't be total, and the compiler rejects it.

   So if we want to map over Setlike things, we have to find a way of constraining both a and b.

    We could write class 
        ```class OrdFunctor f where
              ordMap :: (Ord a, Ord b) => (a -> b) -> f a -> f b```
    
    but this seems busy work when we can factor out the constraint.

    In order to factor out the constraint, we need "ConstraintKinds" language extension, and to allow
    instances to parameterise the constraint, we need "TypeFamilies".  Put the two together
    and we end up with this.
    
-}
class RFunctor f where
    type RFunctorConstraint f :: * -> Constraint
    fmapR :: (RFunctorConstraint f a, RFunctorConstraint f b) => (a -> b) -> f a -> f b
  