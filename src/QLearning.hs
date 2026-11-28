module QLearning
  ( Params(..)
  , defaultParams
  , epsilonAt
  , qUpdate
  , EpisodeStats(..)
  , runEpisode
  , train
  ) where

import qualified Data.List as List

import Environment (Cell(..), GridWorld(..), cellAt, step)
import Policy (epsilonGreedy)
import QTable (QTable, emptyQ, getQ, maxQ, setQ)
import Rng (Gen)

data Params = Params
  { alpha        :: Double
  , gamma        :: Double
  , epsilonStart :: Double
  , epsilonMin   :: Double
  , epsilonDecay :: Double
  , numEpisodes  :: Int   
  , maxSteps     :: Int   
  } deriving (Show, Eq)

defaultParams :: Params
defaultParams = Params
  { alpha        = 0.1
  , gamma        = 0.9
  , epsilonStart = 1.0
  , epsilonMin   = 0.05
  , epsilonDecay = 0.99
  , numEpisodes  = 500
  , maxSteps     = 100
  }

epsilonAt :: Params -> Int -> Double
epsilonAt p ep = max (epsilonMin p) (epsilonStart p * epsilonDecay p ^ ep)

qUpdate :: Params -> Double -> Double -> Double -> Bool -> Double
qUpdate p qAtual recompensa maxProximo terminal =
  qAtual + alpha p * (alvo - qAtual)
  where
    alvo
      | terminal  = recompensa
      | otherwise = recompensa + gamma p * maxProximo

data EpisodeStats = EpisodeStats
  { epReward      :: Double  
  , epSteps       :: Int     
  , epReachedGoal :: Bool    
  } deriving (Show, Eq)

runEpisode :: Params -> GridWorld -> Double -> QTable -> Gen -> (QTable, Gen, EpisodeStats)
runEpisode p gw epsilon q0 g0 = passo (startState gw) q0 g0 0 0
  where
    passo s q g passos total
      | passos >= maxSteps p = (q, g, EpisodeStats total passos False)
      | otherwise =
          let (a, g')      = epsilonGreedy epsilon q s g        
              (s', r, fim) = step gw s a                       
              novoValor    = qUpdate p (getQ q s a) r (maxQ q s') fim
              q'           = setQ s a novoValor q               
              total'       = total + r
          in if fim
               then (q', g', EpisodeStats total' (passos + 1) (cellAt gw s' == Goal))
               else passo s' q' g' (passos + 1) total'          

train :: Params -> GridWorld -> Gen -> (QTable, [EpisodeStats])
train p gw g0 = (qFinal, reverse historico)
  where
    (qFinal, _, historico) = List.foldl' episodio (emptyQ, g0, []) [0 .. numEpisodes p - 1]

    episodio (q, g, hs) ep =
      let (q', g', stats) = runEpisode p gw (epsilonAt p ep) q g
      in  q' `seq` (q', g', stats : hs)
