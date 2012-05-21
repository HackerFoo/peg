
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

{-# LANGUAGE CPP #-}
module Peg.Monad where

import Peg.Types
import Peg.Parse (traceStack)

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Control.Exception
{-
import Data.IORef
import System.IO.Unsafe

dirtyIORef :: IORef Bool
dirtyIORef = unsafePerformIO $ newIORef False

resetMore :: IO ()
resetMore = writeIORef dirtyIORef False

isThereMore :: IO Bool
isThereMore = readIORef dirtyIORef


theresMore :: Peg ()
theresMore = liftIO (writeIORef dirtyIORef True) >> mzero
-}
-- | pop an argument from the stack, push onto argument stack
getArg check = do
  force
  guard . not =<< emptyStack
  x <- popStack
  guard $ check x
  pushArg x

a `dig` b = dig' a b []
  where dig' a b c | a == 0 = c
                   | otherwise = dig' d b (m:c)
          where (d, m) = a `divMod` b

a `ldig` b = dig' a b []
  where dig' a b c | a == 0 = c
                   | otherwise = dig' d b (m:c)
          where (d, m) = (a-1) `divMod` b

letNum :: Int -> String
letNum x | x <= 0 = "a"
         | otherwise = map (toEnum . (+a)) . (`ldig` 26) $ x + 1
  where a = fromEnum 'a'

newVar :: Peg Value
newVar = do PegState s a m n c <- get
#ifdef DEBUG
            when (n > 25) mzero
#endif
            put $ PegState s a m (n+1) c
            return . V $ '_': letNum n

pushStack x = modify (\(PegState s a m n c) -> PegState (x:s) a m n c)
appendStack x = modify (\(PegState s a m n c) -> PegState (x++s) a m n c)

popStack :: Peg Value
popStack = do PegState (x:s) a m n c <- get
              put $ PegState s a m n c
              return x
emptyStack = null . psStack <$> get

setStack s = modify (\(PegState _ a m n c) -> PegState s a m n c)

getStack :: Peg Stack
getStack = psStack <$> get

pushArg x = modify (\(PegState s a m n c) -> PegState s (x:a) m n c)

popArg :: Peg Value
popArg = do PegState s (x:a) m n c <- get
            put $ PegState s a m n c
            return x

peekArg :: Peg Value
peekArg = do PegState s (x:a) m n c <- get
             return x

doWord w = do
  m <- psWords <$> get
  pushArg (W w)
  case w `M.lookup` m of
    Nothing -> pushStack (W w)
    Just [x] -> x 
    Just x -> msum x
  popArg
  return ()

force = do
  st <- get
  case psStack st of
    (W w : _) -> popStack >> doWord w
#ifdef DEBUG
      >> traceStack
#endif
    _ -> return ()

(f ||. g) x = f x || g x
(f &&. g) x = f x && g x

minsert k x = M.insertWith (++) k [x]
mlookup k = maybe [] id . M.lookup k

--addConstraint v f = modify (\(PegState s a m n c) -> PegState s a m n (minsert v f c))
addConstraint x = modify $ \(PegState s a m n c) -> PegState s a m n (x:c)
