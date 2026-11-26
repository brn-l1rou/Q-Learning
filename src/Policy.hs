-- |
-- Módulo Policy (políticas)
--
-- Uma POLÍTICA é a regra que o agente usa para escolher a ação em cada estado.
--
-- O dilema EXPLORAÇÃO x APROVEITAMENTO (exploration vs. exploitation):
--   * aproveitar = escolher a melhor ação conhecida (maior Q);
--   * explorar   = tentar outra ação, para descobrir se ela é melhor.
-- Só aproveitar pode prender o agente em uma solução ruim; só explorar
-- nunca usa o que foi aprendido. A política epsilon-greedy equilibra os dois.
module Policy
  ( greedyAction
  , randomAction
  , epsilonGreedy
  ) where

import Environment (Action, State, allActions)
import QTable (QTable, bestAction)
import Rng (Gen, nextDouble, nextInt)

-- | Política gulosa (greedy): sempre a ação de maior Q. É a política final
-- que o agente usa depois de treinado.
greedyAction :: QTable -> State -> Action
greedyAction = bestAction

-- | Sorteia uma ação qualquer, com a mesma chance para as quatro.
randomAction :: Gen -> (Action, Gen)
randomAction g = (allActions !! i, g')
  where
    (i, g') = nextInt (length allActions) g

-- | Política epsilon-greedy:
--   * com probabilidade epsilon, explora (ação aleatória);
--   * com probabilidade 1 - epsilon, aproveita (melhor ação conhecida).
-- Devolve a ação escolhida e o gerador atualizado.
epsilonGreedy :: Double -> QTable -> State -> Gen -> (Action, Gen)
epsilonGreedy epsilon q s g
  | sorteio < epsilon = randomAction g'
  | otherwise         = (greedyAction q s, g')
  where
    (sorteio, g') = nextDouble g
