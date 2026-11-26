-- |
-- Módulo Rng (gerador de números pseudoaleatórios)
--
-- O Q-Learning precisa de aleatoriedade para EXPLORAR (escolher ações ao
-- acaso de vez em quando). Em Haskell as funções são puras, então não existe
-- um "random()" escondido: o estado do gerador é um valor comum, que entra na
-- função e sai atualizado junto com o número sorteado.
--
-- Usamos um gerador congruencial linear (LCG) de 64 bits:
--     s' = a * s + c   (mod 2^64)
-- com as constantes do MMIX (Donald Knuth). É simples, rápido e, com a mesma
-- semente, sempre produz a mesma sequência: os experimentos são reproduzíveis.
module Rng
  ( Gen
  , mkGen
  , nextDouble
  , nextInt
  ) where

import Data.Bits (shiftR)
import Data.Word (Word64)

-- | O estado do gerador. 'newtype' cria um tipo novo sem custo em tempo de
-- execução: por dentro é só um Word64 (inteiro sem sinal de 64 bits).
newtype Gen = Gen Word64
  deriving (Show, Eq)

-- | Cria um gerador a partir de uma semente. Avançamos uma vez para
-- "misturar" sementes pequenas como 1, 2, 3.
mkGen :: Int -> Gen
mkGen semente = snd (nextWord (Gen (fromIntegral semente)))

-- | Um passo do LCG. A aritmética de Word64 já é módulo 2^64 (dá a volta
-- sozinha quando estoura).
nextWord :: Gen -> (Word64, Gen)
nextWord (Gen s) = (s', Gen s')
  where
    s' = 6364136223846793005 * s + 1442695040888963407

-- | Sorteia um Double em [0, 1).
-- Usamos os 53 bits mais altos (os de melhor qualidade em um LCG) e
-- dividimos por 2^53.
nextDouble :: Gen -> (Double, Gen)
nextDouble g = (fromIntegral (w `shiftR` 11) / 9007199254740992, g')
  where
    (w, g') = nextWord g

-- | Sorteia um inteiro em [0, n - 1].
nextInt :: Int -> Gen -> (Int, Gen)
nextInt n g = (min (n - 1) (floor (d * fromIntegral n)), g')
  where
    (d, g') = nextDouble g
