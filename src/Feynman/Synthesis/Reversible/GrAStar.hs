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

adjust :: Int -> Int -> [Pt] -> [Pt]
adjust t p xs = map f xs
  where f (Pt c t p vecs) = Pt c t p $ map g vecs
        g (bv, i) = (if bv@.t then complementBit bv p else bv, i)

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

-- Pointed
cnotMinGrAstarPointed0 :: Synthesizer
cnotMinGrAstarPointed0 input output [] may = (linearSynth input output, may)
cnotMinGrayPointed0 input output xs may = (linearSynth input output, may)
          
{- Master method -}

-- Fastest
cnotMinGrAstar i o must may = cnotMinGrAstarPointed0 i o (filter (\(_, i) -> order i /= 1) must) may

-- Eagerly applies phases
cnotMinGrAstarEager = \i o mu ma -> cnotMinGrAstarPointed0 i o (mu ++ ma) []

-- Compares between configurations (doubles runtime but best performance)
cnotMinGrAstarPointed input output xs may =
  let result1 = cnotMinGrAstarPointed0 input output xs may
      result2 = cnotMinGrAstarPointed0 input output (filter (\(_, i) -> order i /= 1) xs) may
      isct g = case g of
        CNOT _ _  -> True
        otherwise -> False
      countc = length . filter isct . fst
  in
    minimumBy (comparing countc) [result1, result2]


{- Brute force synthesis -}

maximalSkeleton :: [ID] -> LinearTrans -> [Primitive] -> Set F2Vec
maximalSkeleton ids st gates = snd $ Data.List.foldl f (st, Set.fromList $ Map.elems st) gates
  where f (st, vals) (CNOT c t) =
          let tmp = (st!t) + (st!c) in
            (Map.insert t tmp st, Set.insert tmp vals)
        f (st, vals) _          = (st, vals)

maximalASkeleton :: [ID] -> LinearTrans -> [Primitive] -> (LinearTrans, Set F2Vec)
maximalASkeleton ids st gates = Data.List.foldl f (st, Set.fromList $ Map.elems st) gates
  where f (st, vals) (CNOT c t) =
          let tmp = (st!t) + (st!c) in
            (Map.insert t tmp st, Set.insert tmp vals)
        f (st, vals) _          = (st, vals)

allCNOTs :: [ID] -> [[Primitive]]
allCNOTs ids = concatMap f ids
  where f id = [ [CNOT id id'] | id'<-ids, id /= id']

allSkeletons :: [ID] -> [[Primitive]]
allSkeletons ids = [[]] ++ allCNOTs ids ++ [x++y | y<-allSkeletons ids, x<-allCNOTs ids]

check :: [ID] -> LinearTrans -> Set F2Vec -> [Primitive] -> Bool
check ids st vals = Set.isSubsetOf vals . maximalSkeleton ids st

bruteForceSkeleton :: [ID] -> Set F2Vec -> Maybe [Primitive]
bruteForceSkeleton ids vals = find (Set.isSubsetOf vals . maximalSkeleton ids st) $ allSkeletons ids
  where st = genInitSt ids

bruteForceASkeleton :: [ID] -> Set F2Vec -> LinearTrans -> Maybe [Primitive]
bruteForceASkeleton ids vals out = find (verify . maximalASkeleton ids st) $ allSkeletons ids
  where st               = genInitSt ids
        verify (st, set) = st == out && Set.isSubsetOf vals set

genInitSt :: [ID] -> LinearTrans
genInitSt ids = Map.fromList $ map (\(id, i) -> (id, bitI (length ids) i)) $ zip ids [0..]
