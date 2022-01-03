{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Year2021.Day2.Puzzle where

import Control.Lens (Getter, Iso', iso, over, review, view, (^.))
import Control.Lens.TH (makeFieldsNoPrefix, makeLenses)
import Data.Attoparsec.Text (Parser, choice, decimal, endOfLine, many', parseOnly, sepBy, sepBy1, signed, string)
import Data.List (group)
import Data.Text (Text)
import Year2021.Day2.Input (puzzleData)

data Day2 = Day2

data Command
  = Forward Int
  | Down Int
  | Up Int
  deriving (Show)

forward :: Int -> Command
forward = Forward

down :: Int -> Command
down = Down

up :: Int -> Command
up = Up

newtype Depth = Depth Int
  deriving (Show, Num)

newtype X = X Int
  deriving (Show, Num)

newtype Aim = Aim Int
  deriving (Show, Num)

data Position = Position
  { -- |
    _depth :: Depth,
    -- |
    _x :: X
  }
  deriving (Show)

startingPosition = Position (Depth 0) (X 0)

makeFieldsNoPrefix ''Position

data Position2 = Position2
  { _depth :: Depth,
    _x :: X,
    _aim :: Aim
  }
  deriving (Show)

makeFieldsNoPrefix ''Position2

startingPosition2 = Position2 (Depth 0) (X 0) (Aim 0)

run :: Text -> Either String Int
run t = solve <$> parse t

parser :: Parser [Command]
parser = sepBy1 commandParser endOfLine

commandParser :: Parser Command
commandParser =
  choice
    [ string "forward " *> (Forward <$> signed decimal),
      string "up " *> (Up <$> signed decimal),
      string "down " *> (Down <$> signed decimal)
    ]

-- >>> pa
parse :: Text -> Either String [Command]
parse = parseOnly parser

-- >>> parse "forward 1\ndown 2\nup 3"
-- Right [Forward 1,Down 2,Up 3]
solve :: [Command] -> Int
solve = solution . moves startingPosition

solution :: (HasDepth p Depth, HasX p X) => p -> Int
solution p = solution' (p ^. depth) (p ^. x)

solution' :: Depth -> X -> Int
solution' (Depth d) (X x) = d * x

moves :: Position -> [Command] -> Position
moves = foldl (flip commandStep)

commandStep :: Command -> Position -> Position
commandStep (Forward fwd) = over x (+ X fwd)
commandStep (Down d) = over depth (+ Depth d)
commandStep (Up u) = over depth (\d -> d - Depth u)

solve2 :: [Command] -> Int
solve2 = solution . moves2 startingPosition2

moves2 :: Position2 -> [Command] -> Position2
moves2 = foldl (flip commandStep2)

-- (Position2) -> (Aim, X, Depth) -> (Aim, X, Depth)
commandStep2 :: Command -> Position2 -> Position2
commandStep2 (Forward fwd) = goForward fwd
commandStep2 (Down d) = over aim (+ Aim d)
commandStep2 (Up u) = over aim (\a -> a - Aim u)

goForward :: Int -> Position2 -> Position2
goForward fwd p = p {_x = newX, _depth = newDepth $ view aim p}
  where
    newX = X fwd + view x p
    newDepth (Aim a) = Depth (a * fwd) + view depth p

-- >>> solve <$> parse puzzleData
-- Right 2027977
-- >>> solve <$> testData
-- [0,6]

-- >>> solve2 <$> parse puzzleData
-- Right 1903644897

-- >>> moves2 startingPosition2 <$> parse puzzleData
-- Right (Position2 {_depth = Depth 967791, _x = X 1967, _aim = Aim 1031})

-- >>> solve2 <$> testData
-- [0,18]

-- >>> moves2 startingPosition2 <$> testData
-- [Position2 {_depth = Depth 0, _x = X 0, _aim = Aim (-1)},Position2 {_depth = Depth 6, _x = X 3, _aim = Aim 2}]

testData =
  [ [up 1],
    [down 2, forward 3]
  ]
