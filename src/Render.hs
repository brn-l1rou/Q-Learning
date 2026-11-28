module Render
  ( renderGrid
  , renderPolicy
  , actionArrow
  , renderValues
  , renderPath
  , renderQValues
  , fmt
  ) where

import Text.Printf (printf)

import Environment (Action(..), Cell(..), GridWorld(..), State, cellAt)
import QTable (QTable, bestAction, maxQ, qValues)

desenhar :: GridWorld -> (State -> String) -> String
desenhar gw f = unlines
  [ concat [ f (r, c) | c <- [0 .. gridCols gw - 1] ]
  | r <- [0 .. gridRows gw - 1] ]

simbolo :: GridWorld -> State -> Maybe String
simbolo gw s = case cellAt gw s of
  Wall -> Just " # "
  Trap -> Just " X "
  Goal -> Just " G "
  Empty
    | s == startState gw -> Just " S "
    | otherwise          -> Nothing

renderGrid :: GridWorld -> String
renderGrid gw = desenhar gw (\s -> maybe " . " id (simbolo gw s))

actionArrow :: Action -> String
actionArrow North = "^"
actionArrow South = "v"
actionArrow West  = "<"
actionArrow East  = ">"

renderPolicy :: GridWorld -> QTable -> String
renderPolicy gw q = desenhar gw celula
  where
    celula s = case cellAt gw s of
      Empty -> " " ++ actionArrow (bestAction q s) ++ " "
      _     -> maybe " . " id (simbolo gw s)

renderValues :: GridWorld -> QTable -> String
renderValues gw q = desenhar gw celula
  where
    celula s = case cellAt gw s of
      Wall  -> "  #####"
      Trap  -> "  TRAP "
      Goal  -> "  GOAL "
      Empty -> printf "%7.2f" (maxQ q s)

renderPath :: GridWorld -> [State] -> String
renderPath gw caminho = desenhar gw celula
  where
    celula s
      | cellAt gw s == Empty && s /= startState gw && s `elem` caminho = " * "
      | otherwise = maybe " . " id (simbolo gw s)

renderQValues :: QTable -> State -> String
renderQValues q s = unlines
  [ "  Q(" ++ show s ++ ", " ++ pad (show a) ++ ") = " ++ fmt v
  | (a, v) <- qValues q s ]
  where
    pad t = t ++ replicate (5 - length t) ' '

fmt :: Double -> String
fmt = printf "%.2f"
