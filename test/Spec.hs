-- |
-- Testes automatizados do Q-Learning.
--
-- Rodar com:  cabal test
--
-- Usamos um mini "framework" próprio (a função 'teste') para não depender
-- de bibliotecas externas: cada teste é um nome + uma condição Bool.
module Main (main) where

import Control.Monad (unless)
import System.Exit (exitFailure)

import Environment
import Policy
import QLearning
import QTable
import Rng

-- | Compara números reais com tolerância (contas com Double têm arredondamento).
aprox :: Double -> Double -> Bool
aprox a b = abs (a - b) < 1e-9

-- | Executa um teste e imprime [PASSOU] ou [FALHOU].
teste :: String -> Bool -> IO Bool
teste nome ok = do
  putStrLn ((if ok then "[PASSOU] " else "[FALHOU] ") ++ nome)
  return ok

-- | Sorteia n números em [0,1) a partir de uma semente.
sorteios :: Int -> Int -> [Double]
sorteios n semente = take n (go (mkGen semente))
  where
    go g = let (d, g') = nextDouble g in d : go g'

-- | Sorteia n ações com uma política, sempre no estado inicial.
acoesSorteadas :: Int -> (Gen -> (Action, Gen)) -> [Action]
acoesSorteadas n politica = take n (go (mkGen 1))
  where
    go g = let (a, g') = politica g in a : go g'

-- | Tabela de exemplo em que a melhor ação do início é East.
tabelaEast :: QTable
tabelaEast = setQ (0, 0) East 5 (setQ (0, 0) South 2 emptyQ)

-- | Testes rápidos: gerador, ambiente, tabela Q, regra de atualização e políticas.
testes :: [IO Bool]
testes =
  -- Gerador aleatório -------------------------------------------------------
  [ teste "Mesma semente gera a mesma sequencia (reprodutivel)" $
      sorteios 100 7 == sorteios 100 7
  , teste "Sementes diferentes geram sequencias diferentes" $
      sorteios 100 7 /= sorteios 100 8
  , teste "nextDouble fica sempre em [0, 1)" $
      all (\d -> d >= 0 && d < 1) (sorteios 10000 3)
  , teste "nextInt 4 gera os quatro valores 0..3" $
      let vals = take 1000 (go (mkGen 5))
          go g = let (i, g') = nextInt 4 g in i : go g'
      in  all (`elem` vals) [0 .. 3] && all (\i -> i >= 0 && i <= 3) vals

  -- Ambiente ----------------------------------------------------------------
  , teste "Labirinto de exemplo tem 5x5, inicio em (0,0) e 20 estados validos" $
      gridRows exampleGrid == 5 && gridCols exampleGrid == 5
      && startState exampleGrid == (0, 0) && length (validStates exampleGrid) == 20
  , teste "Mover para fora do mapa mantem o agente no lugar e custa um passo" $
      step exampleGrid (0, 0) North == ((0, 0), rewardStep, False)
  , teste "Bater na parede mantem o agente no lugar" $
      step exampleGrid (0, 1) South == ((0, 1), rewardStep, False)
  , teste "Passo comum move o agente" $
      step exampleGrid (0, 0) East == ((0, 1), rewardStep, False)
  , teste "Chegar ao objetivo da +10 e termina o episodio" $
      step exampleGrid (3, 4) South == ((4, 4), rewardGoal, True)
  , teste "Cair na armadilha da -10 e termina o episodio" $
      step exampleGrid (0, 4) South == ((1, 4), rewardTrap, True)

  -- Tabela Q ----------------------------------------------------------------
  , teste "Tabela vazia vale 0 para qualquer par" $
      getQ emptyQ (2, 2) West == 0
  , teste "setQ grava e getQ le o valor" $
      getQ tabelaEast (0, 0) East == 5
  , teste "maxQ e bestAction acham o maior valor" $
      maxQ tabelaEast (0, 0) == 5 && bestAction tabelaEast (0, 0) == East

  -- Regra de atualização ----------------------------------------------------
  , teste "qUpdate: 0 + 0.1 * (-1 + 0.9*0 - 0) = -0.1" $
      aprox (qUpdate defaultParams 0 (-1) 0 False) (-0.1)
  , teste "qUpdate: estado terminal ignora o futuro (alvo = r)" $
      aprox (qUpdate defaultParams 0 10 999 True) 1.0
  , teste "qUpdate: 2 + 0.1 * (-1 + 0.9*5 - 2) = 2.15" $
      aprox (qUpdate defaultParams 2 (-1) 5 False) 2.15
  , teste "qUpdate com alpha = 1 substitui o valor pelo alvo" $
      aprox (qUpdate defaultParams { alpha = 1 } 3 (-1) 5 False) 3.5

  -- Políticas ---------------------------------------------------------------
  , teste "epsilon = 0 sempre escolhe a melhor acao (so aproveita)" $
      all (== East) (acoesSorteadas 200 (epsilonGreedy 0 tabelaEast (0, 0)))
  , teste "epsilon = 1 escolhe as quatro acoes (so explora)" $
      let as = acoesSorteadas 400 (epsilonGreedy 1 tabelaEast (0, 0))
      in  all (`elem` as) allActions
  , teste "epsilonAt decai e respeita o minimo" $
      epsilonAt defaultParams 0 == 1.0
      && epsilonAt defaultParams 10 < epsilonAt defaultParams 5
      && epsilonAt defaultParams 10000 == epsilonMin defaultParams
  ]

-- | Roda todos os testes e encerra com erro se algum falhar.
main :: IO ()
main = do
  resultados <- sequence testes
  let total  = length resultados
      falhas = length (filter not resultados)
  putStrLn ""
  putStrLn (show (total - falhas) ++ "/" ++ show total ++ " testes passaram.")
  unless (falhas == 0) exitFailure
