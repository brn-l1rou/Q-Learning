-- |
-- Programa de demonstração do Q-Learning.
--
-- Uso:  cabal run qlearning-demo            (semente padrão = 42)
--       cabal run qlearning-demo -- 7       (outra semente aleatória)
--
-- O programa:
--   1. apresenta o ambiente (labirinto, recompensas e hiperparâmetros);
--   2. mostra, passo a passo, contas reais da atualização da tabela Q;
--   3. treina o agente e mostra a evolução por blocos de episódios;
--   4. exibe a política aprendida, a função de valor e o caminho final;
--   5. compara estratégias de exploração x aproveitamento.
module Main (main) where

import Data.Char (isDigit)
import System.Environment (getArgs)
import System.IO (BufferMode(..), hSetBuffering, stdout)
import Text.Printf (printf)

import Environment
import Evaluation
import QLearning
import QTable
import Render
import Rng (mkGen)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  semente <- lerSemente
  let gw = exampleGrid
      p  = defaultParams
  titulo "Q-LEARNING EM HASKELL - DEMONSTRACAO"
  putStrLn ("Semente aleatoria: " ++ show semente)
  secaoAmbiente gw p
  secaoPassoAPasso gw p
  q <- secaoTreinamento gw p semente
  secaoPolitica gw q
  secaoExploracao gw p semente
  titulo "Fim da demonstracao"

-- | Lê a semente do primeiro argumento (padrão 42).
lerSemente :: IO Int
lerSemente = do
  args <- getArgs
  return $ case args of
    (d : _) | not (null d), all isDigit d -> read d
    _                                     -> 42

-- | 1. O ambiente e os parâmetros.
secaoAmbiente :: GridWorld -> Params -> IO ()
secaoAmbiente gw p = do
  titulo "1. O ambiente (estados, acoes e recompensas)"
  putStr (renderGrid gw)
  putStrLn ""
  putStrLn "Legenda: S inicio | G objetivo | X armadilha | # parede | . livre"
  putStrLn ("Estados validos : " ++ show (length (validStates gw)) ++ " posicoes (linha, coluna)")
  putStrLn ("Acoes           : " ++ show allActions)
  putStrLn ("Recompensas     : objetivo " ++ fmt rewardGoal ++ " | armadilha " ++ fmt rewardTrap
            ++ " | cada passo " ++ fmt rewardStep)
  printf "Hiperparametros : alpha = %.2f | gamma = %.2f | epsilon %.2f -> %.2f (x%.2f por episodio)\n"
         (alpha p) (gamma p) (epsilonStart p) (epsilonMin p) (epsilonDecay p)
  putStrLn ("Treino          : " ++ show (numEpisodes p) ++ " episodios de ate "
            ++ show (maxSteps p) ++ " passos")

-- | 2. Três atualizações calculadas à mão, com os números reais.
secaoPassoAPasso :: GridWorld -> Params -> IO ()
secaoPassoAPasso gw p = do
  titulo "2. Passo a passo da atualizacao da tabela Q"
  putStrLn "Regra: Q(s,a) <- Q(s,a) + alpha * [ r + gamma * max Q(s',.) - Q(s,a) ]"
  q1 <- mostrarAtualizacao gw p emptyQ (0, 0) East
  q2 <- mostrarAtualizacao gw p q1 (3, 4) South
  _  <- mostrarAtualizacao gw p q2 (2, 4) South
  putStrLn ""
  putStrLn "Repare no ultimo exemplo: o valor positivo do objetivo comecou a"
  putStrLn "'voltar' para os estados anteriores. Com muitos episodios ele se"
  putStrLn "propaga ate o inicio e forma o caminho."

-- | Executa um único passo (s, a), imprime a conta e devolve a tabela atualizada.
mostrarAtualizacao :: GridWorld -> Params -> QTable -> State -> Action -> IO QTable
mostrarAtualizacao gw p q s a = do
  let (s', r, fim) = step gw s a
      qAntes       = getQ q s a
      maxProx      = if fim then 0 else maxQ q s'
      qDepois      = qUpdate p qAntes r (maxQ q s') fim
  putStrLn ""
  putStrLn ("Estado s = " ++ show s ++ ", acao a = " ++ show a
            ++ "  ->  s' = " ++ show s' ++ ", r = " ++ fmt r
            ++ (if fim then " (terminal)" else ""))
  printf "  Q(s,a) = %.2f + %.2f * [ %.2f + %.2f * %.2f - %.2f ] = %.4f\n"
         qAntes (alpha p) r (if fim then 0 else gamma p) maxProx qAntes qDepois
  return (setQ s a qDepois q)

-- | 3. Treinamento, mostrando a evolução a cada 50 episódios.
secaoTreinamento :: GridWorld -> Params -> Int -> IO QTable
secaoTreinamento gw p semente = do
  titulo "3. Treinamento"
  let (q, historico) = train p gw (mkGen semente)
      tamanho        = 50
  putStrLn "Episodios  | epsilon | recompensa media | passos medios | sucesso"
  mapM_ (\(i, bloco) -> do
          let ini = i * tamanho
          printf "%4d-%-4d  |  %.2f   |      %7.2f     |     %5.1f     |  %3.0f%%\n"
                 (ini + 1) (ini + length bloco) (epsilonAt p ini)
                 (averageReward bloco) (averageSteps bloco) (100 * successRate bloco))
        (zip [0 :: Int ..] (chunksOf tamanho historico))
  putStrLn ""
  putStrLn "No inicio o agente explora quase ao acaso (epsilon alto) e cai em"
  putStrLn "armadilhas; conforme epsilon diminui ele passa a usar o que aprendeu."
  return q

-- | 4. Política aprendida, função de valor e caminho final.
secaoPolitica :: GridWorld -> QTable -> IO ()
secaoPolitica gw q = do
  titulo "4. Politica aprendida (melhor acao em cada estado)"
  putStr (renderPolicy gw q)
  titulo "5. Funcao de valor V(s) = max_a Q(s,a)"
  putStr (renderValues gw q)
  putStrLn ""
  putStrLn "Valores Q do estado inicial:"
  putStr (renderQValues q (startState gw))
  titulo "6. Caminho seguindo a politica gulosa (sem exploracao)"
  let caminho = greedyPath gw q 50
  putStr (renderPath gw caminho)
  putStrLn ""
  putStrLn ("Passos: " ++ show (length caminho - 1)
            ++ (if reachesGoal gw caminho then " | chegou ao objetivo!" else " | NAO chegou ao objetivo"))

-- | 5. Comparação de estratégias de exploração.
secaoExploracao :: GridWorld -> Params -> Int -> IO ()
secaoExploracao gw p semente = do
  titulo "7. Exploracao x aproveitamento"
  let configuracoes =
        [ ("Sem exploracao (epsilon = 0)", p { epsilonStart = 0,   epsilonMin = 0 })
        , ("epsilon fixo = 0.1",           p { epsilonStart = 0.1, epsilonMin = 0.1 })
        , ("epsilon fixo = 0.5",           p { epsilonStart = 0.5, epsilonMin = 0.5 })
        , ("epsilon decrescente 1 -> 0.05", p)
        ]
      naoTerminais = filter (not . isTerminal gw) (validStates gw)
      totalPares   = length naoTerminais * length allActions
  putStrLn "Estrategia                     | sucesso (100 ult.) | recomp. media | pares (s,a) testados | politica final"
  mapM_ (\(nome, cfg) -> do
          let (q, historico) = train cfg gw (mkGen semente)
              ultimos        = drop (length historico - 100) historico
              caminho        = greedyPath gw q 50
              resultado
                | reachesGoal gw caminho = "objetivo em " ++ show (length caminho - 1) ++ " passos"
                | otherwise              = "nao chega"
          printf "%-30s |        %3.0f%%        |    %6.2f     |       %2d de %2d       | %s\n"
                 nome (100 * successRate ultimos) (averageReward ultimos)
                 (visitedPairs q) totalPares resultado)
        configuracoes
  let aleatorio = randomSuccessRate gw (maxSteps p) 1000 (mkGen semente)
  printf "%-30s |        %3.0f%%        |       -       |          -           | -\n"
         "Agente aleatorio (nao aprende)" (100 * aleatorio)
  putStrLn ""
  putStrLn "* Muita exploracao (epsilon = 0.5) custa caro durante o treino."
  putStrLn "* Sem exploracao o agente testa menos pares (s,a): conhece menos o"
  putStrLn "  ambiente. Aqui ele ainda acha o objetivo porque Q comeca em 0 e cada"
  putStrLn "  passo custa -1: acoes nunca testadas parecem melhores que as ja"
  putStrLn "  testadas (exploracao implicita, ou 'inicializacao otimista')."
  putStrLn "* O epsilon decrescente explora bastante no comeco e aproveita no final."

-- | Imprime um título de seção.
titulo :: String -> IO ()
titulo t = do
  putStrLn ""
  putStrLn (replicate 72 '=')
  putStrLn t
  putStrLn (replicate 72 '=')
