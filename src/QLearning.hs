-- |
-- Módulo QLearning (o algoritmo)
--
-- A cada passo o agente está no estado s, escolhe a ação a, recebe a
-- recompensa r, vai para s' e atualiza a tabela com a regra de Bellman:
--
--   Q(s,a) <- Q(s,a) + alpha * [ r + gamma * max_a' Q(s',a') - Q(s,a) ]
--
--   alpha (taxa de aprendizado): quanto da nova informação é absorvida;
--   gamma (fator de desconto)  : quanto o futuro importa (0 = só o agora);
--   r + gamma * max Q(s',.)    : o ALVO (estimativa nova);
--   alvo - Q(s,a)              : o ERRO de diferença temporal (TD).
--
-- O Q-Learning é "off-policy": aprende o valor da política ótima (o max no
-- alvo) mesmo enquanto age com uma política que explora (epsilon-greedy).
module QLearning
  ( Params(..)
  , defaultParams
  , epsilonAt
  , qUpdate
  ) where

-- | Hiperparâmetros do treinamento.
data Params = Params
  { alpha        :: Double  -- ^ taxa de aprendizado (0 a 1)
  , gamma        :: Double  -- ^ fator de desconto (0 a 1)
  , epsilonStart :: Double  -- ^ epsilon no primeiro episódio
  , epsilonMin   :: Double  -- ^ epsilon mínimo
  , epsilonDecay :: Double  -- ^ fator de decaimento por episódio
  , numEpisodes  :: Int     -- ^ quantidade de episódios de treino
  , maxSteps     :: Int     -- ^ limite de passos por episódio
  } deriving (Show, Eq)

-- | Valores usados na demonstração.
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

-- | Epsilon no episódio 'ep' (começando em 0): decai exponencialmente,
-- sem ficar abaixo do mínimo. No início o agente explora muito; no fim,
-- quase só aproveita.
epsilonAt :: Params -> Int -> Double
epsilonAt p ep = max (epsilonMin p) (epsilonStart p * epsilonDecay p ^ ep)

-- | A regra de atualização do Q-Learning, isolada para ficar fácil de testar.
-- Parâmetros: Q(s,a) atual, recompensa r, max Q(s',.) e se s' é terminal.
-- Em estado terminal não existe futuro, então o alvo é só r.
qUpdate :: Params -> Double -> Double -> Double -> Bool -> Double
qUpdate p qAtual recompensa maxProximo terminal =
  qAtual + alpha p * (alvo - qAtual)
  where
    alvo
      | terminal  = recompensa
      | otherwise = recompensa + gamma p * maxProximo

-- | Resumo de um episódio.
data EpisodeStats = EpisodeStats
  { epReward      :: Double  -- ^ soma das recompensas do episódio
  , epSteps       :: Int     -- ^ passos dados
  , epReachedGoal :: Bool    -- ^ terminou no objetivo?
  } deriving (Show, Eq)

-- | Executa um episódio completo: do início até um estado terminal ou até
-- o limite de passos, atualizando a tabela Q a cada passo.
runEpisode :: Params -> GridWorld -> Double -> QTable -> Gen -> (QTable, Gen, EpisodeStats)
runEpisode p gw epsilon q0 g0 = passo (startState gw) q0 g0 0 0
  where
    passo s q g passos total
      | passos >= maxSteps p = (q, g, EpisodeStats total passos False)
      | otherwise =
          let (a, g')      = epsilonGreedy epsilon q s g        -- 1. escolhe a ação
              (s', r, fim) = step gw s a                       -- 2. age no ambiente
              novoValor    = qUpdate p (getQ q s a) r (maxQ q s') fim
              q'           = setQ s a novoValor q               -- 3. atualiza Q(s,a)
              total'       = total + r
          in if fim
               then (q', g', EpisodeStats total' (passos + 1) (cellAt gw s' == Goal))
               else passo s' q' g' (passos + 1) total'          -- 4. continua de s'

-- | Treina por 'numEpisodes' episódios, começando com a tabela vazia.
-- Devolve a tabela final e as estatísticas de cada episódio, em ordem.
train :: Params -> GridWorld -> Gen -> (QTable, [EpisodeStats])
train p gw g0 = (qFinal, reverse historico)
  where
    (qFinal, _, historico) = List.foldl' episodio (emptyQ, g0, []) [0 .. numEpisodes p - 1]

    episodio (q, g, hs) ep =
      let (q', g', stats) = runEpisode p gw (epsilonAt p ep) q g
      in  q' `seq` (q', g', stats : hs)

