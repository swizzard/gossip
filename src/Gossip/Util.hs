module Gossip.Util
  ( lk
  , ch
  , swrRun
  ) where

import Control.Monad.RWS.Strict (RWS, runRWS)
import System.Random (RandomGen, randomR)


ch :: (RandomGen g) => g -> [a] -> (a, g)
ch g xs = (xs !! n, g')
  where (n, g') = randomR (0, length xs - 1) g

lk :: (Eq a) => [(a, b)] -> a -> [b]
lk xs k = foldl f [] xs where
  f acc (a, b) | a == k = b:acc
               | otherwise = acc

swrRun :: r -> s -> RWS r w s a -> (a, s, w)
swrRun r s m = runRWS m r s
