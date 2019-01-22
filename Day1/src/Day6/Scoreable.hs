module Day6.Scoreable where

class Scoreable a where
    score :: a -> Integer
    