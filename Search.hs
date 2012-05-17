module Search where

import Control.Monad

newtype BFS a = BFS {runBFS :: [a] -> [a]}

instance Monad BFS where
  return x = BFS (x:)
  BFS s >>= f = 
