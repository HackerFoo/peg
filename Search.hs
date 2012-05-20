{-# LANGUAGE FlexibleInstances #-}
module Search where

import Control.Monad
import Control.Monad.Trans
import System.IO

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

newtype TreeT m a = TreeT { runTreeT :: m (Tree a) }

instance (Functor m) => Functor (TreeT m) where
  fmap f mt = TreeT . (fmap.fmap) f $ runTreeT mt

instance (Monad m) => Monad (TreeT m) where
  return = TreeT . return . Leaf
  mt >>= f = TreeT $ do
               t <- runTreeT mt
               loop t
    where loop Empty = return Empty
          loop (Leaf x) = runTreeT (f x)
          loop (Node x y) = do x' <- loop x
                               y' <- loop y
                               return $ Node x' y'

instance (Monad m) => MonadPlus (TreeT m) where
  mzero = TreeT . return $ Empty
  mx `mplus` my = TreeT $ do x <- runTreeT mx
                             y <- runTreeT my
                             return $ Node x y

instance MonadIO (TreeT IO) where
  liftIO m = TreeT $ do x <- m
                        return (Leaf x)
               
data Queue a = Queue [a] ([a] -> [a])

emptyQ = Queue [] id

pushQ x (Queue l f) = Queue l (f . (x:))

popQ = popQ' . forceQ
  where popQ' (Queue (x:l) f) = (x, Queue l f)
        popQ' (Queue [] f) = error "popQ: empty queue"

forceQ (Queue [] f) = Queue (f []) id
forceQ q = q

listQ (Queue l f) = l ++ f []

nullQ = nullQ' . forceQ
  where nullQ' (Queue l _) = null l

data QueueM q r a = QueueM { runQueueM :: Queue q ->
                                          ((a, Queue q) -> (Maybe r, Queue q)) ->
                                          (Maybe r, Queue q) }

instance Functor (QueueM q r) where
  fmap f (QueueM qf) = QueueM $ \q k -> qf q (k . (\(x, q) -> (f x, q)))

instance Monad (QueueM q r) where
  return x = QueueM $ \q k -> k (x, q)
  QueueM qf >>= f = QueueM $ \q k ->
                      qf q $ \(x, q') ->
                        runQueueM (f x) q' k

pushQM :: q -> QueueM q r ()
pushQM x = QueueM $ \q k -> k ((), pushQ x q)

popQM :: QueueM q r q
popQM = QueueM $ \q k -> if nullQ q
                           then (Nothing, q)
                           else k $ popQ q


runQM qm = runQueueM (fmap Just qm) emptyQ id

runTree c = runQM $ pushQM c >> loop
  where loop = do c <- popQM
                  case c of
                    Leaf x -> return x
                    Empty -> loop
                    Node x y -> pushQM x >> pushQM y >> loop

choose :: (MonadPlus m) => [a] -> m a
choose = foldr (mplus . return) mzero

--test :: Int -> Tree (Int, Int)
test c = fst . runTree $ do
  x <- choose [1..]
  y <- choose [x..]
  guard $ x * y == c
  return (x, y)

testTreeT :: TreeT IO (Int, Int)
testTreeT = do
  liftIO $ putStr "enter a number: "
  c <- liftIO readLn
  x <- choose [1..40]
  y <- choose [1..40]
  guard $ x * y == c
  return (x, y)

testTreeT2 = do
  t <- runTreeT testTreeT
  return . fst . runTree $ t
