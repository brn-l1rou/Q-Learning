-- | Programa de demonstração do Q-Learning (versão parcial).
-- Por enquanto apenas mostra o gerador de números aleatórios funcionando.
module Main (main) where

import Rng (Gen, mkGen, nextDouble)

main :: IO ()
main = do
  putStrLn "Q-Learning em Haskell"
  putStrLn ""
  putStrLn "5 sorteios com a semente 42:"
  mapM_ (putStrLn . ("  " ++) . show) (sorteios 5 (mkGen 42))
  putStrLn "5 sorteios com a semente 42 de novo (mesmos valores):"
  mapM_ (putStrLn . ("  " ++) . show) (sorteios 5 (mkGen 42))

-- | Gera n números em [0, 1), passando o gerador de um sorteio para o outro.
sorteios :: Int -> Gen -> [Double]
sorteios 0 _ = []
sorteios n g = d : sorteios (n - 1) g'
  where
    (d, g') = nextDouble g
