{-# LANGUAGE FlexibleInstances #-}
module Search where

import Control.Monad
import Control.Monad.Trans
import System.IO
import Control.Monad.Identity

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
  liftIO m = TreeT $ do x <- m
                        return (LeafT x)
               
instance MonadTrans TreeT where
  lift m = TreeT $ do x <- m
                      return (LeafT x)
               
data Queue a = Queue [a] ([a] -> [a])

emptyQueue = Queue [] id

pushQueue x (Queue l f) = Queue l (f . (x:))

popQueue = popQ' . forceQueue
  where popQ' (Queue (x:l) f) = (x, Queue l f)
        popQ' (Queue [] f) = error "popQ: empty queue"

forceQueue (Queue [] f) = Queue (f []) id
forceQueue q = q

listQueue (Queue l f) = l ++ f []

nullQueue = nullQ' . forceQueue
  where nullQ' (Queue l _) = null l

data QueueT q r m a = QueueT {
  runQueueT :: Queue q ->
               ((a, Queue q) -> m (Maybe r, Queue q)) ->
               m (Maybe r, Queue q)
}

instance Functor (QueueT q r m) where
  fmap f (QueueT qf) = QueueT $ \q k -> qf q (k . (\(x, q) -> (f x, q)))

instance Monad (QueueT q r m) where
  return x = QueueT $ \q k -> k (x, q)
  QueueT qf >>= f = QueueT $ \q k ->
                      qf q $ \(x, q') ->
                        runQueueT (f x) q' k

instance MonadIO (QueueT q r IO) where
  liftIO m = QueueT $ \q k -> do x <- m
                                 k (x, q)

instance MonadTrans (QueueT q r) where
  lift m = QueueT $ \q k -> do x <- m
                               k (x, q)
               
pushQ :: q -> QueueT q r m ()
pushQ x = QueueT $ \q k -> k ((), pushQueue x q)

popQ :: (Monad m) => QueueT q r m q
popQ = QueueT $ \q k -> if nullQueue q
                           then return (Nothing, q)
                           else k $ popQueue q

runQ qm = runQueueT (fmap Just qm) emptyQueue return

runBFS c = runQ $ pushQ c >> loop
  where loop = do mc <- popQ
                  c <- lift (runTreeT mc)
                  case c of
                    LeafT x -> return x
                    EmptyT -> loop
                    NodeT mx my -> do pushQ mx
                                      pushQ my
                                      loop

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

testTreeT :: IO (Maybe (Int, Int))
testTreeT = do
  (x, q) <- runBFS $ do
    liftIO $ putStr "enter a number: "
    c <- liftIO readLn
    x <- choose [1..]
    y <- choose [x..]
    guard $ x * y == c
    liftIO . putStrLn $ "a solution is " ++ show (x, y)
    return (x, y)
  return x
