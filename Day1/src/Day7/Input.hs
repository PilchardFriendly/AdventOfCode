{- HLINT ignore "Unused LANGUAGE pragma" -} 
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Day7.Input (puzzleData) where

import Data.Text (Text)   
import Data.String.Here

puzzleData ::Text
puzzleData = [here|
Step X must be finished before step M can begin.
Step A must be finished before step R can begin.
Step C must be finished before step K can begin.
Step H must be finished before step G can begin.
Step R must be finished before step Z can begin.
Step S must be finished before step K can begin.
Step K must be finished before step G can begin.
Step O must be finished before step Z can begin.
Step Q must be finished before step G can begin.
Step E must be finished before step Y can begin.
Step U must be finished before step I can begin.
Step G must be finished before step N can begin.
Step M must be finished before step P can begin.
Step Y must be finished before step I can begin.
Step I must be finished before step V can begin.
Step Z must be finished before step B can begin.
Step W must be finished before step V can begin.
Step D must be finished before step P can begin.
Step L must be finished before step J can begin.
Step N must be finished before step T can begin.
Step T must be finished before step P can begin.
Step B must be finished before step F can begin.
Step F must be finished before step P can begin.
Step J must be finished before step V can begin.
Step V must be finished before step P can begin.
Step Z must be finished before step F can begin.
Step B must be finished before step J can begin.
Step B must be finished before step P can begin.
Step X must be finished before step F can begin.
Step Y must be finished before step N can begin.
Step W must be finished before step D can begin.
Step G must be finished before step B can begin.
Step L must be finished before step V can begin.
Step K must be finished before step L can begin.
Step W must be finished before step P can begin.
Step E must be finished before step F can begin.
Step Y must be finished before step J can begin.
Step J must be finished before step P can begin.
Step A must be finished before step O can begin.
Step O must be finished before step E can begin.
Step T must be finished before step V can begin.
Step S must be finished before step E can begin.
Step I must be finished before step L can begin.
Step E must be finished before step B can begin.
Step G must be finished before step J can begin.
Step Z must be finished before step J can begin.
Step K must be finished before step T can begin.
Step L must be finished before step F can begin.
Step X must be finished before step S can begin.
Step U must be finished before step G can begin.
Step K must be finished before step N can begin.
Step Q must be finished before step W can begin.
Step H must be finished before step F can begin.
Step O must be finished before step P can begin.
Step M must be finished before step D can begin.
Step T must be finished before step J can begin.
Step G must be finished before step T can begin.
Step N must be finished before step P can begin.
Step O must be finished before step V can begin.
Step Q must be finished before step I can begin.
Step Z must be finished before step T can begin.
Step C must be finished before step J can begin.
Step D must be finished before step J can begin.
Step G must be finished before step W can begin.
Step U must be finished before step L can begin.
Step R must be finished before step B can begin.
Step H must be finished before step K can begin.
Step X must be finished before step I can begin.
Step X must be finished before step B can begin.
Step I must be finished before step P can begin.
Step L must be finished before step N can begin.
Step O must be finished before step Y can begin.
Step F must be finished before step J can begin.
Step E must be finished before step I can begin.
Step G must be finished before step M can begin.
Step Q must be finished before step E can begin.
Step D must be finished before step F can begin.
Step A must be finished before step Z can begin.
Step I must be finished before step D can begin.
Step B must be finished before step V can begin.
Step U must be finished before step J can begin.
Step Y must be finished before step T can begin.
Step O must be finished before step M can begin.
Step M must be finished before step B can begin.
Step M must be finished before step L can begin.
Step N must be finished before step B can begin.
Step X must be finished before step U can begin.
Step E must be finished before step Z can begin.
Step Z must be finished before step L can begin.
Step R must be finished before step E can begin.
Step M must be finished before step I can begin.
Step H must be finished before step N can begin.
Step X must be finished before step J can begin.
Step C must be finished before step S can begin.
Step R must be finished before step I can begin.
Step E must be finished before step D can begin.
Step Y must be finished before step L can begin.
Step S must be finished before step D can begin.
Step U must be finished before step Z can begin.
Step A must be finished before step C can begin.
Step Y must be finished before step W can begin.
|]