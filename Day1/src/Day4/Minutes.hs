{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Day4.Minutes where

import           Control.Monad
import           Control.Monad.ST
import           Data.STRef
import           Control.Monad.Primitive

import           Data.Vector.Sized              ( )
import qualified Data.Vector.Sized             as V

import           Data.Vector.Mutable.Sized      ( )
import qualified Data.Vector.Mutable.Sized     as MV

import           Data.Vector.Unboxed.Sized      ( )
import qualified Data.Vector.Unboxed.Sized     as UV

import           Data.Finite                    ( finite
                                                , Finite
                                                )


-- import Debug.Trace

import           Data.Typeable                  ( Proxy(..) )

import           GHC.TypeLits            hiding ( Mod )
import           GHC.TypeLits                   ( type (<=) )
import           Data.Modular
import           Day4.SleepRange
type MinuteCount a = V.Vector 60 a


-- Golfing

-- traceMonad :: (Show a, Monad m) => a -> m a
-- traceMonad x = trace ("test: " ++ show x) (return x)
data SizedFun n a =
    SizedFun {dim::Integer, func :: forall s . (PrimMonad s) => V.MVector n (PrimState s) a -> s ()}

modify' :: forall n a . SizedFun n a -> V.Vector n a -> V.Vector n a
modify' (SizedFun dim fn) v = runST $ do
    vm <- V.thaw v
    fn vm
    V.freeze vm

countFinite
    :: forall v n a
     . (KnownNat n, Num a, 1<=n)
    => V.Vector n a
    -> [Finite n]
    -> V.Vector n a
countFinite init xs = (modify' $ SizedFun (natVal (Proxy @n)) go) init
  where
    go :: forall s . (PrimMonad s) => V.MVector n (PrimState s) a -> s ()
    go mv = foldM_ step mv xs
    step a b = do
        i <- MV.read a b
        MV.write a b (i + 1)
        return a




  -- where
  --   go :: (Vector v a, Dim v ~ 60, a~Int) => ST s (v Int)
  --   go = 
  -- where
  --   go :: (DimM v ~ 60, MVector v a, PrimMonad m, a ~ Int) => v (PrimState m) Int -> X1 -> m ()
  --   go v x = do 
  --     a <- MV.read v (unMod x)
  --     MV.write v (succ a)

minuteCount :: (Num a, Show a) => [SleepRange] -> V.Vector 60 a
minuteCount sleep =
    countFinite (V.replicate 0) $ finite . unMod <$> foldMap asleepMinutes sleep
