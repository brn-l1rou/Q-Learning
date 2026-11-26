-- |
-- Módulo Render (exibição)
--
-- Transforma o labirinto, a política e a função de valor em texto para o
-- terminal. Fica separado da lógica, que continua pura e testável.
module Render
  ( renderGrid
  , renderPolicy
  , actionArrow
  ) where

import Environment (Action(..), Cell(..), GridWorld(..), State, cellAt)
import QTable (QTable, bestAction)

-- | Desenha a grade aplicando uma função a cada posição.
desenhar :: GridWorld -> (State -> String) -> String
desenhar gw f = unlines
  [ concat [ f (r, c) | c <- [0 .. gridCols gw - 1] ]
  | r <- [0 .. gridRows gw - 1] ]

-- | Símbolo fixo de uma célula especial, se houver.
simbolo :: GridWorld -> State -> Maybe String
simbolo gw s = case cellAt gw s of
  Wall -> Just " # "
  Trap -> Just " X "
  Goal -> Just " G "
  Empty
    | s == startState gw -> Just " S "
    | otherwise          -> Nothing

-- | O labirinto: S início, # parede, X armadilha, G objetivo, . livre.
renderGrid :: GridWorld -> String
renderGrid gw = desenhar gw (\s -> maybe " . " id (simbolo gw s))

-- | Seta que representa cada ação.
actionArrow :: Action -> String
actionArrow North = "^"
actionArrow South = "v"
actionArrow West  = "<"
actionArrow East  = ">"

-- | A política gulosa: em cada célula livre, a seta da melhor ação.
renderPolicy :: GridWorld -> QTable -> String
renderPolicy gw q = desenhar gw celula
  where
    celula s = case cellAt gw s of
      Empty -> " " ++ actionArrow (bestAction q s) ++ " "
      _     -> maybe " . " id (simbolo gw s)
