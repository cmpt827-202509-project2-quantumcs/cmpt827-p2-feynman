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
type CloseQ = Set (Set F2Vec, Set F2Vec, Set F2Vec)

-- Trivial heuristic forces a breadth-first search
trivialHeuristic _ _ = 0 :: Int

-- Assuming all phases are distinct, we will need at least one CNOT per
phaseCountHeuristic :: F2Mat -> (Set F2Vec, Set F2Vec, Set F2Vec) -> Int
phaseCountHeuristic _ (mustRemain, _, _) = Set.size mustRemain

linSynthHeuristic :: F2Mat -> (Set.Set F2Vec, Set.Set F2Vec, Set.Set F2Vec) -> Int
linSynthHeuristic curMat (mustRemain, _, _) =
  let kMust = Set.size mustRemain
      perParityLB =
        case Set.toList mustRemain of
          [] -> 0
          vs -> max 0 (maximum (map cost vs ) - 1)
  in max kMust perParityLB
  where
    tranposeMat = transpose curMat

    -- Faster approach, dont have to calculate minSolution for every node
    -- Solve A^t * x = v
    -- If A^t is full rank, there is a unique solution
    -- Otherwise, find the minimum weight solution
    solve :: F2Vec -> Maybe F2Vec
    solve =
      if fullRank tranposeMat
        then oneSolution tranposeMat
        else minSolution tranposeMat
    
    cost :: F2Vec -> Int
    cost v =
      case solve v of
        Just x -> wt x
        Nothing -> 0 -- if the node is unsolvable, return 0 cost
    


-- input: the functions currently computed on the qubits
-- output: the functions we would like to end with, on the qubits
-- must: the phase functions we must hit during synthesis
-- may: some optional goal phases we can add, if it's convenient
-- Returns a list of gates, and a list of successfully synthesized phase functions.
cnotMinGrAStar :: (HasFeatureFlags) => LinearTrans -> LinearTrans -> [Phase] -> [Phase] -> ([Primitive], [Phase])
cnotMinGrAStar input output origMust origMay =
  traceA ("+ GrAStar output=" ++ show (Map.toList output) ++ ", must=" ++ show must ++ ", may=" ++ show may) $
    traceA ("+ circuit=" ++ show resCirc ++ "\n  phases=" ++ show resPhases) $
      traceA ("+ STATS - " ++ "Generating nodes: " ++ show genNodes ++ ", Expanding nodes: " ++ show expNodes) $
      traceA("###################################################################################") $
        (resCirc, resPhases)
  where
    must = filter (\(_, a) -> a /= 0) origMust
    may = filter (\(_, a) -> a /= 0) origMay

    -- Identify the qubits in "must" parities
    mandatoryMask = foldl (.|.) 0 (map fst must)

    -- Identify the qubits needed in output
    (initialKeptQubits, dependencyMask) = Map.foldlWithKey checkOutput (Set.empty, mandatoryMask) input

    checkOutput (kept, mask) qubit val 
      | output ! qubit /= val = (Set.insert qubit kept, mask .|. output ! qubit)
      | otherwise                = (kept, mask)
      
    -- Identify if there is any additional necessary qubit helping along the way to get the final output parity
    (finalRelatedQubits, _) = iterateClosure initialKeptQubits dependencyMask

    iterateClosure kept mask =
      let (kept', mask', changed) = Map.foldlWithKey extend (kept, mask, False) input
      in if changed then iterateClosure kept' mask' else (kept', mask')

    extend (k, m, c) q val 
      | Set.member q k = (k, m, c) -- already kept
      | val .&. m /= 0 = (Set.insert q k, m .|. val, True) -- overlaps needed vars
      | otherwise      = (k, m, c)
      
    -- Filter the final related qubits from input and output for searching and synthesizing  
    relatedInput  = Map.filterWithKey (\k _ -> Set.member k finalRelatedQubits) input
    relatedOutput = Map.filterWithKey (\k _ -> Set.member k finalRelatedQubits) output

    (resCirc, remMust) = addMay relatedInput must (circuit ++ linearSynth lastTransform relatedOutput)

    resPhases = remMust ++ may

    n = Map.size relatedInput
    (qids, inVecs) = unzip (Map.toList relatedInput)
    inputMat = fromList inVecs

    heuristic = case True of
                  _ | useFeature fcfFeature_GrAStar_Heuristic_Trivial -> trivialHeuristic
                  _ | useFeature fcfFeature_GrAStar_Heuristic_PhaseCount -> phaseCountHeuristic
                  _ | useFeature fcfFeature_GrAStar_Heuristic_LinSynth -> linSynthHeuristic
                  _ -> error "No default heuristic at the moment"

    inputBasis = Set.fromList (vals inputMat)
    rootKey = (Set.fromList (map fst must) Set.\\ inputBasis, inputBasis, inputBasis)
    
    initialTrans = Map.fromList (zip qids (vals inputMat))
    initialHLin = length (linearSynth initialTrans relatedOutput)
    rootF = 0 + max (heuristic inputMat rootKey) initialHLin
    initialMemo = Map.singleton initialTrans initialHLin

    ((lastTransform, circuit), (genNodes, expNodes)) = expandNext (HashPSQ.singleton rootKey rootF (inputMat, [])) Set.empty initialMemo (0, 0)

    -- Threading a Map to memoize `linearSynth` calls speeds up heuristic calculation
    expandNext :: (HasFeatureFlags) => AStarQ -> CloseQ -> Map LinearTrans Int -> (Int, Int) -> ((LinearTrans, [Primitive]), (Int, Int))
    expandNext psq closed memo (genNodes, expNodes) =
      let newExpNodes = expNodes + 1 in
      traceASearch ("Expanding " ++ formatNode (HashPSQ.findMin psq)) $
        generateChildren (HashPSQ.findMin psq) newExpNodes
      where
        generateChildren Nothing _ = undefined 
        generateChildren (Just (key@(mustRemain, basis, generated), fCost, (curMat, circRev))) newExpNodes
          | null mustRemain = ((curTransform, reverse circRev), (genNodes, newExpNodes)) 
          | otherwise       =
            let psqDel = HashPSQ.deleteMin psq
                indices = [(i,j) | i <- [0..n-1], j <- [0..n-1], i /= j]
                curRowsArr = vals curMat
                
                (childNodes, newMemo) = foldl' processChild ([], memo) indices
                
                processChild (acc, m) (i, j) = 
                  -- 1. Compute parity and short-circuit invalid states before mutating any matrices
                  let curParity_i = curRowsArr !! i
                      curParity_j = curRowsArr !! j
                      newParity = curParity_i + curParity_j
                  in if newParity `Set.member` generated then (acc, m)
                     else 
                       let childMustRemain = Set.delete newParity mustRemain
                           childBasis = Set.insert newParity (Set.delete curParity_j basis)
                           childGenerated = Set.insert newParity generated
                           childKey = (childMustRemain, childBasis, childGenerated)
                       in if childKey `Set.member` closed then (acc, m)
                          else 
                            let childMat = addRow i j curMat
                                childTrans = Map.fromList (zip qids (vals childMat))
                                -- 2. Memoized lookup for the expensive linearSynth cost
                                (hLinCost, m') = case Map.lookup childTrans m of
                                                   Just c  -> (c, m)
                                                   Nothing -> let c = length (linearSynth childTrans relatedOutput)
                                                              in (c, Map.insert childTrans c m)
                                hPhase = heuristic childMat childKey
                                childCirc = CNOT (qids !! i) (qids !! j) : circRev
                                g = length childCirc
                                childF = g + max hPhase hLinCost
                                childVal = (childMat, childCirc)
                            in ((childKey, childF, childVal) : acc, m')

                newClosed = Set.insert key closed
                newGenNodes = genNodes + length childNodes 
                newPsq = foldl' (\psq' (k, p, v) -> HashPSQ.insert k p v psq') psqDel childNodes
             in expandNext newPsq newClosed newMemo (newGenNodes, newExpNodes)
          where
            curTransform = Map.fromList (zip qids (vals curMat))

        formatNode Nothing = "<SKIP!>"
        formatNode (Just ((mustRemain, basis, generated), fCost, (curMat, circRev))) =
          "Basis=" ++ show (Set.toList basis) ++ ", f=" ++ show fCost ++ ", must=" ++ show (Set.toList mustRemain)