module Gossip.Util
  ( ch
  , lk
  ) where

import Control.Monad.Random
import Control.Monad.RWS.Strict (RWS, runRWS)
import System.Random (RandomGen, randomR)


ch :: (MonadRandom m) => [a] -> m a
ch xs = do
  n <- getRandomR (0, length xs - 1)
  return $ xs !! n

lk :: (Eq a) => [(a, b)] -> a -> [b]
lk xs k = foldl f [] xs where
  f acc (a, b) | a == k = b:acc
               | otherwise = acc
