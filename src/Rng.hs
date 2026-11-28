module Rng
  ( Gen
  , mkGen
  , nextDouble
  , nextInt
  ) where

import Data.Bits (shiftR)
import Data.Word (Word64)

newtype Gen = Gen Word64
  deriving (Show, Eq)

mkGen :: Int -> Gen
mkGen semente = snd (nextWord (Gen (fromIntegral semente)))

nextWord :: Gen -> (Word64, Gen)
nextWord (Gen s) = (s', Gen s')
  where
    s' = 6364136223846793005 * s + 1442695040888963407

nextDouble :: Gen -> (Double, Gen)
nextDouble g = (fromIntegral (w `shiftR` 11) / 9007199254740992, g')
  where
    (w, g') = nextWord g

nextInt :: Int -> Gen -> (Int, Gen)
nextInt n g = (min (n - 1) (floor (d * fromIntegral n)), g')
  where
    (d, g') = nextDouble g
