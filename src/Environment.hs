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

data Action = North | South | West | East
  deriving (Show, Eq, Ord, Enum, Bounded)

allActions :: [Action]
allActions = [minBound .. maxBound]

data Cell = Empty | Wall | Trap | Goal
  deriving (Show, Eq)

type State = (Int, Int)

data GridWorld = GridWorld
  { gridRows   :: Int
  , gridCols   :: Int
  , gridCells  :: [[Cell]]
  , startState :: State
  } deriving (Show, Eq)

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

exampleGrid :: GridWorld
exampleGrid = parseGrid
  [ "S...."
  , ".##.X"
  , "...#."
  , "X#..."
  , "...#G"
  ]

cellAt :: GridWorld -> State -> Cell
cellAt gw (r, c)
  | r < 0 || c < 0 || r >= gridRows gw || c >= gridCols gw = Wall
  | otherwise = (gridCells gw !! r) !! c

validStates :: GridWorld -> [State]
validStates gw =
  [ (r, c) | r <- [0 .. gridRows gw - 1]
           , c <- [0 .. gridCols gw - 1]
           , cellAt gw (r, c) /= Wall ]

rewardGoal :: Double
rewardGoal = 10

rewardTrap :: Double
rewardTrap = -10

rewardStep :: Double
rewardStep = -1

isTerminal :: GridWorld -> State -> Bool
isTerminal gw s = cellAt gw s `elem` [Goal, Trap]

move :: Action -> State -> State
move North (r, c) = (r - 1, c)
move South (r, c) = (r + 1, c)
move West  (r, c) = (r, c - 1)
move East  (r, c) = (r, c + 1)

step :: GridWorld -> State -> Action -> (State, Double, Bool)
step gw s a = case cellAt gw destino of
  Wall  -> (s, rewardStep, False)
  Goal  -> (destino, rewardGoal, True)
  Trap  -> (destino, rewardTrap, True)
  Empty -> (destino, rewardStep, False)
  where
    destino = move a s
