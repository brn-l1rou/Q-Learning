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

greedyPath :: GridWorld -> QTable -> Int -> [State]
greedyPath gw q limite = andar (startState gw) limite
  where
    andar s n
      | isTerminal gw s || n <= 0 = [s]
      | otherwise =
          let (s', _, _) = step gw s (greedyAction q s)
          in  s : andar s' (n - 1)

reachesGoal :: GridWorld -> [State] -> Bool
reachesGoal gw caminho = case reverse caminho of
  (ultimo : _) -> cellAt gw ultimo == Goal
  []           -> False

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

successRate :: [EpisodeStats] -> Double
successRate es = media (map (\e -> if epReachedGoal e then 1 else 0) es)

averageReward :: [EpisodeStats] -> Double
averageReward = media . map epReward

averageSteps :: [EpisodeStats] -> Double
averageSteps = media . map (fromIntegral . epSteps)

media :: [Double] -> Double
media [] = 0
media xs = sum xs / fromIntegral (length xs)

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = bloco : chunksOf n resto
  where
    (bloco, resto) = splitAt (max 1 n) xs
