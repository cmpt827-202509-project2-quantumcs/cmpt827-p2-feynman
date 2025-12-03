module Feynman.Synthesis.Reversible.GrAStar where

import Feynman.Core
import Feynman.Algebra.Base
import Feynman.Algebra.Linear
import Feynman.Synthesis.Phase
import Feynman.Synthesis.Reversible

import Data.List hiding (transpose)

import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Ord (comparing)

import Data.Maybe

import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Writer.Lazy

import Data.Bits
import Debug.Trace

{- Gray code synthesis -}
data Pt = Pt {
  candidates :: [Int],
  target     :: Maybe Int,
  pending    :: Maybe Int,
  vectors    :: [Phase]
} deriving Show



-- Optionally adds "may" phases whenever possible
addMay :: LinearTrans -> [Phase] -> [Primitive] -> ([Primitive], [Phase])
addMay st phases = (\(a,b) -> (reverse a,b)) . snd . foldl' go (st,([],phases)) where
  go (st,(circ,may)) gate@(CNOT c t) =
    let tmp = (st!t) + (st!c) in
      case partition (\phase -> fst phase == tmp) may of
        ([], may')      -> (Map.insert t tmp st, (gate:circ, may'))
        ([phase], may') -> (Map.insert t tmp st, (circ', may')) where
          circ' = synthesizePhase t (snd phase) ++ (gate:circ)
  go (st,(circ,may)) gate = (st,(gate:circ,may))

-- input: the functions currently computed on the qubits
-- output: the functions we would like to end with, on the qubits
-- must: the phase functions we must hit during synthesis
-- may: some optional goal phases we can add, if it's convenient (Joe: when would this happen?)
-- Returns a list of gates, and a list of successfully synthesized phase functions.
cnotMinGrAStar :: LinearTrans -> LinearTrans -> [Phase] -> [Phase] -> ([Primitive], [Phase])
cnotMinGrAStar input output must may = undefined
-- Skeleton of A*:
--   while there are still nodes in the queue:
--     pop highest priority node
--     expand:
--       check if node is a goal:
--         if so, we're done! quit with this node's path as the result
--       compute node children and their cost heuristic h(n)'s
--       insert node children (prioritized by f(n) = g(n) + h(n), lowest f is highest priority)
--   otherwise, search failed! there's no solution.
-- Note we can use linearSynth to get ourselves efficiently to some particular goal state -- this should always be the last step, to get to "output".
-- 
