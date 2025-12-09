{-# LANGUAGE ImplicitParams #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Feynman.FeatureFlags where

import Data.Maybe (isJust)
import qualified Debug.Trace

-- These names are super long to avoid namespace clashes; you don't generally
-- interact with them directly so there's not as much cost to verbosity
data FeatureFlags = FeatureFlags
  { -- Log each synthesis step as it occurs
    fcfTrace_Synthesis_Pathsum_Unitary :: Bool,
    fcfTrace_Synthesis_XAG :: Bool,
    fcfTrace_Graph :: Bool,
    fcfTrace_AStar :: Bool,
    fcfTrace_AStarSearch :: Bool,
    fcfFeature_Synthesis_Pathsum_Unitary_AffineSynth :: Bool,
    fcfFeature_Synthesis_Pathsum_Unitary_MCTSynth :: Bool,
    fcfFeature_Synthesis_Pathsum_Unitary_XAGSynth :: Bool,
    fcfFeature_Synthesis_Pathsum_Unitary_MCRzPhase :: Bool,
    fcfFeature_Synthesis_Pathsum_Unitary_MCTRzPhase :: Bool,
    fcfFeature_Synthesis_Pathsum_Unitary_XAGRzPhase :: Bool,
    fcfFeature_Synthesis_Pathsum_Unitary_XAGMBURzPhase :: Bool,
    fcfFeature_Synthesis_XAG_Direct :: Bool,
    fcfFeature_Synthesis_XAG_Strash :: Bool,
    fcfFeature_Synthesis_XAG_MinMultSat :: Bool,
    fcfFeature_GrAStar_Heuristic_Trivial :: Bool,
    fcfFeature_GrAStar_Heuristic_PhaseCount :: Bool
  }

defaultFeatures :: FeatureFlags
defaultFeatures =
  FeatureFlags
    { fcfTrace_Synthesis_Pathsum_Unitary = False,
      fcfTrace_Synthesis_XAG = False,
      fcfTrace_Graph = False,
      fcfTrace_AStar = False,
      fcfTrace_AStarSearch = False,
      fcfFeature_Synthesis_Pathsum_Unitary_AffineSynth = True,
      fcfFeature_Synthesis_Pathsum_Unitary_MCTSynth = False,
      fcfFeature_Synthesis_Pathsum_Unitary_XAGSynth = False,
      fcfFeature_Synthesis_Pathsum_Unitary_MCRzPhase = True,
      fcfFeature_Synthesis_Pathsum_Unitary_MCTRzPhase = False,
      fcfFeature_Synthesis_Pathsum_Unitary_XAGRzPhase = False,
      fcfFeature_Synthesis_Pathsum_Unitary_XAGMBURzPhase = False,
      fcfFeature_Synthesis_XAG_Direct = False,
      fcfFeature_Synthesis_XAG_Strash = False,
      fcfFeature_Synthesis_XAG_MinMultSat = False,
      fcfFeature_GrAStar_Heuristic_Trivial = False,
      fcfFeature_GrAStar_Heuristic_PhaseCount = False
    }

-- This is pretty meh, but when I considered giving making these switches into
-- enumerated types, I realized I'd be writing more lines of code, and then
-- the switch system itself would be nonuniform and more complicated, which
-- kind of defeats the whole purpose.

reset_fcfFeature_Synthesis_Pathsum_Unitary_Synth fc =
  fc
    { fcfFeature_Synthesis_Pathsum_Unitary_AffineSynth = False,
      fcfFeature_Synthesis_Pathsum_Unitary_MCTSynth = False,
      fcfFeature_Synthesis_Pathsum_Unitary_XAGSynth = False
    }

reset_fcfFeature_Synthesis_Pathsum_Unitary_Phase fc =
  fc
    { fcfFeature_Synthesis_Pathsum_Unitary_MCRzPhase = False,
      fcfFeature_Synthesis_Pathsum_Unitary_MCTRzPhase = False,
      fcfFeature_Synthesis_Pathsum_Unitary_XAGRzPhase = False,
      fcfFeature_Synthesis_Pathsum_Unitary_XAGMBURzPhase = False
    }

reset_fcfFeature_Synthesis_XAG fc =
  fc
    { fcfFeature_Synthesis_XAG_Direct = False,
      fcfFeature_Synthesis_XAG_Strash = False,
      fcfFeature_Synthesis_XAG_MinMultSat = False
    }

reset_fcfFeature_GrAStar_Heuristic fc =
  fc
    { fcfFeature_GrAStar_Heuristic_Trivial = False,
      fcfFeature_GrAStar_Heuristic_PhaseCount = False
    }

isFeatureSwitch :: String -> Bool
isFeatureSwitch s = isJust (featureSwitchFunction s)

featureSwitchFunction :: String -> Maybe (FeatureFlags -> FeatureFlags)
featureSwitchFunction "trace-unitary" = Just (\fc -> fc {fcfTrace_Synthesis_Pathsum_Unitary = True})
featureSwitchFunction "trace-xag" = Just (\fc -> fc {fcfTrace_Synthesis_XAG = True})
featureSwitchFunction "trace-graph" = Just (\fc -> fc {fcfTrace_Graph = True})
featureSwitchFunction "trace-astar" = Just (\fc -> fc {fcfTrace_AStar = True})
featureSwitchFunction "trace-astar-search" = Just (\fc -> fc {fcfTrace_AStarSearch = True})
featureSwitchFunction "unitary-ket-original" =
  Just (\fc -> (reset_fcfFeature_Synthesis_Pathsum_Unitary_Synth fc) {fcfFeature_Synthesis_Pathsum_Unitary_AffineSynth = True})
featureSwitchFunction "unitary-ket-mct" =
  Just (\fc -> (reset_fcfFeature_Synthesis_Pathsum_Unitary_Synth fc) {fcfFeature_Synthesis_Pathsum_Unitary_MCTSynth = True})
featureSwitchFunction "unitary-ket-xag" =
  Just (\fc -> (reset_fcfFeature_Synthesis_Pathsum_Unitary_Synth fc) {fcfFeature_Synthesis_Pathsum_Unitary_XAGSynth = True})
featureSwitchFunction "unitary-phase-original" =
  Just (\fc -> (reset_fcfFeature_Synthesis_Pathsum_Unitary_Phase fc) {fcfFeature_Synthesis_Pathsum_Unitary_MCRzPhase = True})
featureSwitchFunction "unitary-phase-mct-rz" =
  Just (\fc -> (reset_fcfFeature_Synthesis_Pathsum_Unitary_Phase fc) {fcfFeature_Synthesis_Pathsum_Unitary_MCTRzPhase = True})
featureSwitchFunction "unitary-phase-xag-rz" =
  Just (\fc -> (reset_fcfFeature_Synthesis_Pathsum_Unitary_Phase fc) {fcfFeature_Synthesis_Pathsum_Unitary_XAGRzPhase = True})
featureSwitchFunction "unitary-phase-xag-mbu-rz" =
  Just (\fc -> (reset_fcfFeature_Synthesis_Pathsum_Unitary_Phase fc) {fcfFeature_Synthesis_Pathsum_Unitary_XAGMBURzPhase = True})
featureSwitchFunction "xag-direct" =
  Just (\fc -> (reset_fcfFeature_Synthesis_XAG fc) {fcfFeature_Synthesis_XAG_Direct = True})
featureSwitchFunction "xag-strash" =
  Just (\fc -> (reset_fcfFeature_Synthesis_XAG fc) {fcfFeature_Synthesis_XAG_Strash = True})
featureSwitchFunction "xag-minmultsat" =
  Just (\fc -> (reset_fcfFeature_Synthesis_XAG fc) {fcfFeature_Synthesis_XAG_MinMultSat = True})
featureSwitchFunction "gas-heuristic-trivial" =
  Just (\fc -> (reset_fcfFeature_GrAStar_Heuristic fc) {fcfFeature_GrAStar_Heuristic_Trivial = True})
featureSwitchFunction "gas-heuristic-phasecount" =
  Just (\fc -> (reset_fcfFeature_GrAStar_Heuristic fc) {fcfFeature_GrAStar_Heuristic_PhaseCount = True})
featureSwitchFunction _ = Nothing

useFeature :: (?featureFlags :: FeatureFlags) => (FeatureFlags -> Bool) -> Bool
useFeature fcf = fcf ?featureFlags

traceIf :: Bool -> String -> a -> a
traceIf True msg x = Debug.Trace.trace msg x
traceIf False _ x = x

traceValIf :: Bool -> (a -> String) -> a -> a
traceValIf True msgF x = Debug.Trace.trace (msgF x) x
traceValIf False _ x = x
