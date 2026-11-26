-- |
-- Módulo QTable (a tabela Q)
--
-- A tabela Q guarda, para cada par (estado, ação), uma estimativa de
-- "quanto vale" executar aquela ação naquele estado: a soma das recompensas
-- futuras (descontadas) que o agente espera receber.
--
-- Usamos um Data.Map (dicionário balanceado): pares que ainda não foram
-- visitados não ocupam espaço e valem 0 por padrão.
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

-- | Tabela Q: (estado, ação) -> valor.
type QTable = Map.Map (State, Action) Double

-- | Tabela vazia: todos os valores começam em 0.
emptyQ :: QTable
emptyQ = Map.empty

-- | Lê Q(s, a). Se o par nunca foi atualizado, vale 0.
getQ :: QTable -> State -> Action -> Double
getQ q s a = Map.findWithDefault 0 (s, a) q

-- | Grava Q(s, a) = v, devolvendo uma NOVA tabela (a antiga não muda).
setQ :: State -> Action -> Double -> QTable -> QTable
setQ s a v = Map.insert (s, a) v

-- | Os valores Q de todas as ações em um estado.
qValues :: QTable -> State -> [(Action, Double)]
qValues q s = [ (a, getQ q s a) | a <- allActions ]

-- | max_a Q(s, a): o valor do estado se o agente agir da melhor forma.
-- É a FUNÇÃO DE VALOR V(s) derivada da tabela Q.
maxQ :: QTable -> State -> Double
maxQ q s = maximum (map snd (qValues q s))

-- | argmax_a Q(s, a): a melhor ação conhecida no estado.
-- Em caso de empate, fica a primeira na ordem North, South, West, East.
bestAction :: QTable -> State -> Action
bestAction q s = fst (foldl1 melhor (qValues q s))
  where
    melhor atual candidato
      | snd candidato > snd atual = candidato
      | otherwise                 = atual

-- | Quantos pares (estado, ação) já foram atualizados pelo menos uma vez.
-- Mede o quanto do ambiente o agente chegou a experimentar.
visitedPairs :: QTable -> Int
visitedPairs = Map.size
