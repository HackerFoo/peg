{- Copyright 2012 Dustin DeWeese
   This file is part of peg.

    peg is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    peg is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with peg.  If not, see <http://www.gnu.org/licenses/>.
-}

{-# LANGUAGE FlexibleInstances, TupleSections, GeneralizedNewtypeDeriving, RankNTypes, ImpredicativeTypes #-}
module Search where

import Control.Monad
import Control.Monad.Trans
import System.IO
import Control.Monad.Identity
import Control.Monad.Cont

import System.IO.Unsafe

{-
data Tree a = Node (Tree a) (Tree a) | Leaf a | Empty deriving (Show)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x y) = Node (fmap f x) (fmap f y)

instance Monad Tree where
  return = Leaf
  Node x y >>= f = Node (x >>= f) (y >>= f)
  Leaf x >>= f = f x
  Empty >>= _ = Empty

instance MonadPlus Tree where
  mzero = Empty
  mplus = Node
-}

type Tree = TreeT Identity

newtype TreeT m a = TreeT { runTreeT :: m (TreeT' m a) }
data TreeT' m a = NodeT (TreeT m a) (TreeT m a) | LeafT a | EmptyT

instance (Functor m) => Functor (TreeT m) where
  fmap f mt = TreeT . fmap f' $ runTreeT mt
    where f' EmptyT = EmptyT
          f' (LeafT x) = LeafT (f x)
          f' (NodeT mx my) = NodeT (fmap f mx) (fmap f my)

instance (Monad m) => Monad (TreeT m) where
  return = TreeT . return . LeafT
  mt >>= f = TreeT $ do
               t <- runTreeT mt
               case t of
                 EmptyT -> return EmptyT
                 LeafT x -> runTreeT (f x)
                 NodeT mx my -> return $ NodeT (mx >>= f) (my >>= f)

instance (Monad m) => MonadPlus (TreeT m) where
  mzero = TreeT . return $ EmptyT
  mx `mplus` my = TreeT . return $ NodeT mx my

instance MonadIO (TreeT IO) where
  liftIO = lift
               
instance MonadTrans TreeT where
  lift = TreeT . liftM LeafT

data Queue a = Queue [a] ([a] -> [a])

emptyQueue = Queue [] id

pushQueue x (Queue l f) = Queue l (f . (x:))

popQueue :: Queue a -> (a, Queue a)
popQueue = popQ' . forceQueue
  where popQ' (Queue (x:l) f) = (x, Queue l f)
        popQ' (Queue [] f) = error "popQ: empty queue"

forceQueue (Queue [] f) = Queue (f []) id
forceQueue q = q

listQueue (Queue l f) = l ++ f []

nullQueue :: Queue a -> Bool
nullQueue = nullQ' . forceQueue
  where nullQ' (Queue l _) = null l

data QueueStateT q r m a = QueueStateT {
  runQueueStateT :: Queue q ->
               ((a, Queue q) -> m (Maybe r, Queue q)) ->
               m (Maybe r, Queue q)
}

instance Functor (QueueStateT q r m) where
  fmap f (QueueStateT qf) = QueueStateT $ \q k -> qf q (k . (\(x, q) -> (f x, q)))

instance Monad (QueueStateT q r m) where
  return x = QueueStateT $ \q k -> k (x, q)
  QueueStateT qf >>= f = QueueStateT $ \q k ->
                           qf q $ \(x, q') ->
                             runQueueStateT (f x) q' k

instance MonadIO (QueueStateT q r IO) where
  liftIO = lift

instance MonadTrans (QueueStateT q r) where
  lift m = QueueStateT $ \q k -> k . (, q) =<< m
               
pushQ :: q -> QueueStateT q r m ()
pushQ x = QueueStateT $ \q k -> k ((), pushQueue x q)

popQ :: (Monad m) => QueueStateT q r m q
popQ = QueueStateT $ \q k -> if nullQueue q
                               then return (Nothing, q)
                               else k $ popQueue q

runQ qm = runQueueStateT (fmap Just qm) emptyQueue return

runWithQ q qm = runQueueStateT (fmap Just qm) q return

runBFS c = runQ $ pushQ c >> bfs

bfs :: (Monad m) => QueueStateT (TreeT m a) r m a
bfs = do mc <- popQ
         c <- lift $ runTreeT mc
         case c of
           LeafT x -> return x
           EmptyT -> bfs
           NodeT mx my -> pushQ mx >> pushQ my >> bfs

runBFSn n c = runBFSn' n $ pushQueue c emptyQueue

runBFSn' n q | n <= 0 = return []
             | otherwise = do (mx, q') <- runWithQ q bfs
                              case mx of
                                Nothing -> return []
                                Just x -> fmap (x :) $ runBFSn' (n-1) q'

runBFSAll c = runBFSAll' $ pushQueue c emptyQueue
runBFSAll' q = do (mx, q') <- runWithQ q bfs
                  case mx of
                    Nothing -> return []
                    Just x -> fmap (x :) $ runBFSAll' q'

runBFSAllI c = runBFSAllI' $ pushQueue c emptyQueue
runBFSAllI' q = do (mx, q') <- runWithQ q bfs
                   case mx of
                     Nothing -> return []
                     Just x -> fmap (x :) . unsafeInterleaveIO $ runBFSAllI' q'

choose :: (MonadPlus m) => [a] -> m a
choose = foldr (mplus . return) mzero

test :: Int -> Maybe (Int, Int)
test c = runIdentity $ do
  (x, q) <- runBFS $ do
    x <- choose [1..]
    y <- choose [x..]
    guard $ x * y == c
    return (x, y)
  return x

testTreeT :: IO [(Int, Int)]
testTreeT = do
  x <- runBFSAllI $ do
    liftIO $ putStr "enter a number: "
    c <- liftIO readLn
    x <- choose [1..]
    y <- choose [x..]
    guard $ x * y == c
    liftIO . putStrLn $ "a solution is " ++ show (x, y)
    return (x, y)
  return (take 4 x)
