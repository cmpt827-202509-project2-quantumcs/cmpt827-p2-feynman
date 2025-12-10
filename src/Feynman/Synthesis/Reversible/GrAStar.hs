module Feynman.Synthesis.Reversible.GrAStar where

import Feynman.Core
import Feynman.FeatureFlags
import Feynman.Algebra.Base
import Feynman.Algebra.Linear
import Feynman.Synthesis.Phase
import Feynman.Synthesis.Reversible

import Data.List hiding (transpose)

import Data.HashPSQ (HashPSQ)
import qualified Data.HashPSQ as HashPSQ

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
import Control.Exception (assert)


traceA :: (HasFeatureFlags) => String -> a -> a
traceA = traceIf (useFeature fcfTrace_AStar)

traceASearch :: (HasFeatureFlags) => String -> a -> a
traceASearch = traceIf (useFeature fcfTrace_AStarSearch)

traceValA :: (HasFeatureFlags) => (a -> String) -> a -> a
traceValA = traceValIf (useFeature fcfTrace_AStar)


-- Optionally adds "may" phases whenever possible
-- FIXED: Now checks if the initial state satisfies any phases before processing gates.
addMay :: LinearTrans -> [Phase] -> [Primitive] -> ([Primitive], [Phase])
addMay st phases circ = (reverse resCirc, resPhases)
  where
    -- 1. Identify phases satisfied by the initial state immediately
    (satisfied, remaining) = partition (\(v, _) -> v `elem` Map.elems st) phases
    
    -- Helper to map Vector -> Qubit ID (reverse lookup of state)
    vecToQubit = Map.fromList [ (v, k) | (k, v) <- Map.toList st ]
    
    -- Generate gates for the phases satisfied at the input
    initGates = concatMap (\(v, a) -> synthesizePhase (vecToQubit ! v) a) satisfied

    -- 2. Process the rest of the circuit
    -- We initialize the accumulator with 'reverse initGates' so they appear at the
    -- start of the final circuit (since the accumulator builds in reverse).
    initialAccum = (st, (reverse initGates, remaining))

    (_, (resCirc, resPhases)) = foldl' go initialAccum circ
    
    go (st,(circ,may)) gate@(CNOT c t) =
      let tmp = (st!t) + (st!c) in
        case partition (\phase -> fst phase == tmp) may of
          ([], may')      -> (Map.insert t tmp st, (gate:circ, may'))
          ([phase], may') -> (Map.insert t tmp st, (circ', may')) where
            circ' = synthesizePhase t (snd phase) ++ (gate:circ)
    go (st,(circ,may)) gate = (st,(gate:circ,may))


-- Generally in this algorithm, we care about storing 3 elements for each node:
-- 1. The set of phases (each parity an F2Vec) that remain to be computed
-- 2. The current LinearTrans computed by this circuit
-- 3. The circuit so far (in reverse order)
-- We consider circuits computing the same parities and final transform as
-- functionally equivalent, so together those make the PSQ key. The circuit
-- must be stored as the value, and the priority gets g(n) which is the lower-
-- bound cost for the path through this node to the goal (i.e. g(n) + h(n)).
-- As usual g(n) is the cost so far, in our case the circuit CNOT- complexity.
-- f(n) may include an effectively fractional tiebreaker cost, such as any
-- "may"s satisfied, or the circuit depth.

type AStarQ = HashPSQ (Set F2Vec, Set F2Vec, Set F2Vec) Int (F2Mat, [Primitive])


-- Trivial heuristic forces a breadth-first search
trivialHeuristic _ = 0 :: Int

-- Assuming all phases are distinct, we will need at least one CNOT per
phaseCountHeuristic :: (Set F2Vec, Set F2Vec, Set F2Vec) -> Int
phaseCountHeuristic (mustRemain, _, _) = Set.size mustRemain


-- input: the functions currently computed on the qubits
-- output: the functions we would like to end with, on the qubits
-- must: the phase functions we must hit during synthesis
-- may: some optional goal phases we can add, if it's convenient
-- Returns a list of gates, and a list of successfully synthesized phase functions.
cnotMinGrAStar :: (HasFeatureFlags) => LinearTrans -> LinearTrans -> [Phase] -> [Phase] -> ([Primitive], [Phase])
cnotMinGrAStar input output must may =
  traceA ("GrAStar output=" ++ show (Map.toList output) ++ ", must=" ++ show must ++ ", may=" ++ show may) $
    traceA ("  circuit=" ++ show resCirc ++ "\n  phases=" ++ show resPhases)
      (resCirc, resPhases)
  where
    (resCirc, resPhases) = addMay input (must ++ may) (circuit ++ linearSynth lastTransform output)

    inputBasis = Set.fromList (vals inputMat)
    rootKey = (Set.fromList (map fst must) Set.\\ inputBasis, inputBasis, inputBasis)
    -- (lastTransform, circuit) = expandNext (HashPSQ.singleton rootKey (heuristic rootKey) (inputMat, []))
    (lastTransform, circuit) = expandNext (HashPSQ.singleton rootKey rootF (inputMat, []))
    n = Map.size input
    (qids, inVecs) = unzip (Map.toList input)
    inputMat = fromList inVecs

    heuristic = case True of
                  _ | useFeature fcfFeature_GrAStar_Heuristic_Trivial -> trivialHeuristic
                  _ | useFeature fcfFeature_GrAStar_Heuristic_PhaseCount -> phaseCountHeuristic
                  _ -> error "No default heuristic at the moment"

    nodePriority
      :: (Set F2Vec, Set F2Vec, Set F2Vec)  -- PSQ key = (mustRemain, basis, generated)
      -> F2Mat                              -- current matrix
      -> [Primitive]                        -- circuit so far (reversed)
      -> Int
    nodePriority key@(mustRemain, _, _) curMat circRev =
      let g       = length circRev
          hPhase  = heuristic key
          curTrans = Map.fromList (zip qids (vals curMat))
          -- hLin = cost of linearSynth from current transform to output
          hLin    = length (linearSynth curTrans output)
          -- lower bound of remaining work
          h       = max hPhase hLin
      in g + h
    
    rootF = nodePriority rootKey inputMat []

    -- Skeleton of A*:
    --   while there are still nodes in the queue:
    --     pop highest priority node
    --     expand:
    --       check if node is a goal:
    --         if so, we're done! quit with this node's path as the result
    --       compute node children and their cost heuristic h(n)'s
    --       insert node children (prioritized by f(n) = g(n) + h(n), lowest f is highest priority)
    --   otherwise, search failed! there's no solution.

    -- The "key" also contains the full set of generated parities for the circuit now
    expandNext :: (HasFeatureFlags) => AStarQ -> (LinearTrans, [Primitive])
    expandNext psq =
      traceASearch ("Expanding " ++ formatNode (HashPSQ.findMin psq)) $
        generateChildren (HashPSQ.findMin psq)
      where
        generateChildren Nothing = undefined -- shouldn't happen
        generateChildren (Just ((mustRemain, basis, generated), fCost, (curMat, circRev)))
          | null mustRemain = (curTransform, reverse circRev) -- no musts left: goal achieved!
          | otherwise       = expandNext (foldl' (\psq' (k, p, v) -> HashPSQ.insert k p v psq') psqDel childNodes)
          -- Try adding every different CNOT to the PSQ
          where
            curTransform = Map.fromList (zip qids (vals curMat))
            psqDel = HashPSQ.deleteMin psq
            childNodes = catMaybes [makeChild i j | i <- [0..n-1], j <- [0..n-1], i /= j]
            makeChild i j
              | newParity `Set.member` generated = Nothing
              | otherwise                        = assert (childBasis == Set.fromList (vals childMat)) $
                                                     Just (childKey, childF, childVal)
              where
                childKey = (childMustRemain, childBasis, childGenerated)
                childCirc = newGate : circRev
                childF = nodePriority childKey childMat childCirc
                -- childF = length circRev + heuristic childKey
                childVal = (childMat, childCirc)

                childMustRemain = Set.delete newParity mustRemain
                childBasis = Set.insert newParity (Set.delete curParity basis)
                childGenerated = Set.insert newParity generated

                curParity = row curMat j
                newParity = row childMat j
                newGate = CNOT (qids !! i) (qids !! j)

                childMat = addRow i j curMat
        formatNode Nothing = "<SKIP!>"
        formatNode (Just ((mustRemain, basis, generated), fCost, (curMat, circRev))) =
          "Basis=" ++ show (Set.toList basis) ++ ", f=" ++ show fCost ++ ", must=" ++ show (Set.toList mustRemain)

