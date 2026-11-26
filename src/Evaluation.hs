-- |
-- Módulo Evaluation (avaliação)
--
-- Funções para medir o que o agente aprendeu: seguir a política gulosa,
-- comparar com um agente aleatório e resumir as estatísticas de treino.
module Evaluation
  ( greedyPath
  , reachesGoal
  , randomSuccessRate
  , successRate
  , averageReward
  , averageSteps
  , chunksOf
  ) where

import Environment (Cell(..), GridWorld(..), State, cellAt, isTerminal, step)
import Policy (greedyAction, randomAction)
import QLearning (EpisodeStats(..))
import QTable (QTable)
import Rng (Gen)

-- | Caminho percorrido seguindo sempre a melhor ação (sem exploração),
-- a partir do início, até um estado terminal ou até 'limite' passos.
greedyPath :: GridWorld -> QTable -> Int -> [State]
greedyPath gw q limite = andar (startState gw) limite
  where
    andar s n
      | isTerminal gw s || n <= 0 = [s]
      | otherwise =
          let (s', _, _) = step gw s (greedyAction q s)
          in  s : andar s' (n - 1)

-- | O caminho termina no objetivo?
reachesGoal :: GridWorld -> [State] -> Bool
reachesGoal gw caminho = case reverse caminho of
  (ultimo : _) -> cellAt gw ultimo == Goal
  []           -> False

-- | Taxa de sucesso (0 a 1) de um agente que só escolhe ações ao acaso,
-- em 'episodios' tentativas de até 'limite' passos. Serve de linha de base.
randomSuccessRate :: GridWorld -> Int -> Int -> Gen -> Double
randomSuccessRate gw limite episodios g0 =
  fromIntegral (contar episodios g0 0) / fromIntegral (max 1 episodios)
  where
    contar 0 _ acertos = acertos :: Int
    contar n g acertos =
      let (chegou, g') = tentativa (startState gw) limite g
      in  contar (n - 1) g' (if chegou then acertos + 1 else acertos)

    tentativa s n g
      | n <= 0    = (False, g)
      | otherwise =
          let (a, g')      = randomAction g
              (s', _, fim) = step gw s a
          in  if fim then (cellAt gw s' == Goal, g') else tentativa s' (n - 1) g'

-- | Fração de episódios que chegaram ao objetivo.
successRate :: [EpisodeStats] -> Double
successRate es = media (map (\e -> if epReachedGoal e then 1 else 0) es)

-- | Recompensa média por episódio.
averageReward :: [EpisodeStats] -> Double
averageReward = media . map epReward

-- | Quantidade média de passos por episódio.
averageSteps :: [EpisodeStats] -> Double
averageSteps = media . map (fromIntegral . epSteps)

-- | Média de uma lista (0 para lista vazia).
media :: [Double] -> Double
media [] = 0
media xs = sum xs / fromIntegral (length xs)

-- | Divide uma lista em blocos de tamanho n. Ex.: chunksOf 2 [1,2,3] = [[1,2],[3]].
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = bloco : chunksOf n resto
  where
    (bloco, resto) = splitAt (max 1 n) xs
