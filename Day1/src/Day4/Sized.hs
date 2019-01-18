{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
module Day4.Sized (countFinite) where

import           Control.Monad 
import           Control.Monad.ST
import           Control.Monad.Primitive

import           Prelude hiding (read, replicate)
import           Data.Vector.Sized   hiding (foldM_)          
import           Data.Vector.Mutable.Sized  hiding ( replicate)   

import           Data.Finite   (Finite)                 

import           Data.Typeable                  ( Proxy(..) )

import           GHC.TypeLits            hiding ( Mod )
 

type SizedM n a r = forall s . (PrimMonad s) => MVector n (PrimState s) a -> s r

data SizedFun n a r = SizedFun {dim::Integer, func :: SizedM n a r}

modifySized :: forall n a . SizedFun n a () -> Vector n a -> Vector n a
modifySized (SizedFun dim fn) v = runST $ do
    vm <- thaw v
    fn vm
    freeze vm


countFinite
    :: forall n a
     . (KnownNat n, Num a)
    => [Finite n]
    -> Vector n a
countFinite xs = modifySized (SizedFun (natVal (Proxy @n)) go) (replicate 0)
  where
    go :: SizedM n a ()
    go mv = foldM_ step mv xs
    step a b = do
        i <- read a b
        write a b (i + 1)
        return a