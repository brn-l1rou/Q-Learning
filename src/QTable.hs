module QTable
  ( QTable
  , emptyQ
  , getQ
  , setQ
  , qValues
  , maxQ
  , bestAction
  , visitedPairs
  ) where

import qualified Data.Map.Strict as Map

import Environment (Action, State, allActions)

type QTable = Map.Map (State, Action) Double

emptyQ :: QTable
emptyQ = Map.empty

getQ :: QTable -> State -> Action -> Double
getQ q s a = Map.findWithDefault 0 (s, a) q

setQ :: State -> Action -> Double -> QTable -> QTable
setQ s a v = Map.insert (s, a) v

qValues :: QTable -> State -> [(Action, Double)]
qValues q s = [ (a, getQ q s a) | a <- allActions ]

maxQ :: QTable -> State -> Double
maxQ q s = maximum (map snd (qValues q s))

bestAction :: QTable -> State -> Action
bestAction q s = fst (foldl1 melhor (qValues q s))
  where
    melhor atual candidato
      | snd candidato > snd atual = candidato
      | otherwise                 = atual

visitedPairs :: QTable -> Int
visitedPairs = Map.size
