{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Day6.Partitionable where

class Partitionable a b
    where
      intoPartition :: a -> b    