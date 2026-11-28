module Policy
  ( greedyAction
  , randomAction
  , epsilonGreedy
  ) where

import Environment (Action, State, allActions)
import QTable (QTable, bestAction)
import Rng (Gen, nextDouble, nextInt)

greedyAction :: QTable -> State -> Action
greedyAction = bestAction

randomAction :: Gen -> (Action, Gen)
randomAction g = (allActions !! i, g')
  where
    (i, g') = nextInt (length allActions) g

epsilonGreedy :: Double -> QTable -> State -> Gen -> (Action, Gen)
epsilonGreedy epsilon q s g
  | sorteio < epsilon = randomAction g'
  | otherwise         = (greedyAction q s, g')
  where
    (sorteio, g') = nextDouble g
