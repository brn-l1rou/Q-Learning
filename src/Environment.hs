-- |
-- Módulo Environment (o ambiente)
--
-- Define o mundo em que o agente vive: um labirinto em grade (GridWorld).
--   * ESTADO  = a posição (linha, coluna) do agente;
--   * AÇÃO    = mover para Norte, Sul, Oeste ou Leste;
--   * RECOMPENSA = número devolvido pelo ambiente após cada ação.
--
-- O agente NÃO conhece estas regras: ele só observa o estado seguinte e a
-- recompensa. É isso que torna o Q-Learning um método "livre de modelo".
module Environment
  ( Action(..)
  , allActions
  , Cell(..)
  , State
  , GridWorld(..)
  , parseGrid
  , exampleGrid
  , cellAt
  , validStates
  , rewardGoal
  , rewardTrap
  , rewardStep
  , isTerminal
  , move
  , step
  ) where

-- | As quatro ações possíveis.
-- 'Enum' e 'Bounded' permitem listar todas com [minBound .. maxBound].
data Action = North | South | West | East
  deriving (Show, Eq, Ord, Enum, Bounded)

-- | Lista com todas as ações: [North, South, West, East].
allActions :: [Action]
allActions = [minBound .. maxBound]

-- | O conteúdo de uma célula do labirinto.
data Cell = Empty | Wall | Trap | Goal
  deriving (Show, Eq)

-- | Um estado é a posição (linha, coluna). Linha 0 é a de cima.
type State = (Int, Int)

-- | O labirinto: dimensões, células e a posição inicial do agente.
data GridWorld = GridWorld
  { gridRows   :: Int
  , gridCols   :: Int
  , gridCells  :: [[Cell]]
  , startState :: State
  } deriving (Show, Eq)

-- | Monta um labirinto a partir de texto:
--   'S' = início, '#' = parede, 'X' = armadilha, 'G' = objetivo, '.' = livre.
parseGrid :: [String] -> GridWorld
parseGrid linhas = GridWorld
  { gridRows   = length linhas
  , gridCols   = maximum (0 : map length linhas)
  , gridCells  = map (map paraCelula) linhas
  , startState = inicio
  }
  where
    paraCelula ch = case ch of
      '#' -> Wall
      'X' -> Trap
      'G' -> Goal
      _   -> Empty
    inicio = case [ (r, c) | (r, linha) <- zip [0 ..] linhas
                           , (c, ch)    <- zip [0 ..] linha
                           , ch == 'S' ] of
      (s : _) -> s
      []      -> (0, 0)

-- | O labirinto usado na demonstração (5 x 5).
-- O caminho mais curto do S até o G tem 8 passos.
exampleGrid :: GridWorld
exampleGrid = parseGrid
  [ "S...."
  , ".##.X"
  , "...#."
  , "X#..."
  , "...#G"
  ]

-- | Conteúdo de uma posição. Fora do mapa é tratado como parede.
cellAt :: GridWorld -> State -> Cell
cellAt gw (r, c)
  | r < 0 || c < 0 || r >= gridRows gw || c >= gridCols gw = Wall
  | otherwise = (gridCells gw !! r) !! c

-- | Todos os estados em que o agente pode estar (tudo que não é parede).
validStates :: GridWorld -> [State]
validStates gw =
  [ (r, c) | r <- [0 .. gridRows gw - 1]
           , c <- [0 .. gridCols gw - 1]
           , cellAt gw (r, c) /= Wall ]

-- | Recompensa ao chegar no objetivo.
rewardGoal :: Double
rewardGoal = 10

-- | Recompensa (punição) ao cair na armadilha.
rewardTrap :: Double
rewardTrap = -10

-- | Custo de cada passo comum. Ser negativo incentiva o caminho mais curto.
rewardStep :: Double
rewardStep = -1

-- | Estados terminais encerram o episódio: objetivo e armadilhas.
isTerminal :: GridWorld -> State -> Bool
isTerminal gw s = cellAt gw s `elem` [Goal, Trap]

-- | Posição de destino de uma ação (sem checar paredes).
move :: Action -> State -> State
move North (r, c) = (r - 1, c)
move South (r, c) = (r + 1, c)
move West  (r, c) = (r, c - 1)
move East  (r, c) = (r, c + 1)

-- | A dinâmica do ambiente: dado o estado e a ação, devolve
-- (próximo estado, recompensa, episódio terminou?).
-- Bater em parede ou na borda mantém o agente no lugar e custa um passo.
step :: GridWorld -> State -> Action -> (State, Double, Bool)
step gw s a = case cellAt gw destino of
  Wall  -> (s, rewardStep, False)
  Goal  -> (destino, rewardGoal, True)
  Trap  -> (destino, rewardTrap, True)
  Empty -> (destino, rewardStep, False)
  where
    destino = move a s
