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

{-# LANGUAGE CPP, ImplicitParams #-}
module Peg.Monad (module Peg.Monad, newVar, newSVar) where

import Peg.Types
import Peg.Parse
import qualified Peg.Constraints as C
import Peg.Constraints (newVar, newSVar)
import Peg.Utils

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Control.Exception
import Data.List
import Data.Char (toUpper)

import Debug.Trace

-- | pop an argument from the stack, push onto argument stack
getArg check = do
  force
  guard . not =<< emptyStack
  x <- popStack
  guard $ check x
  pushArg x

pushStack x = modify (\(PegState s a w n c p) -> PegState (x:s) a w n c p)
appendStack x = modify (\(PegState s a w n c p) -> PegState (x++s) a w n c p)

popStack :: Peg Value
popStack = do PegState (x:s) a w n c p <- get
              put $ PegState s a w n c p
              return x
emptyStack = null . psStack <$> get

setStack s = modify (\(PegState _ a w n c p) -> PegState s a w n c p)

getStack :: Peg Stack
getStack = psStack <$> get

pushArg x = modify (\(PegState s a w n c p) -> PegState s (x:a) w n c p)

popArg :: Peg Value
popArg = do PegState s (x:a) w n c p <- get
            put $ PegState s a w n c p
            return x

peekArg :: Peg Value
peekArg = do PegState s (x:a) w n c p <- get
             return x

pushAnc x = modify $ \(PegState s a w n c p) -> PegState s a w n c (x:p)
popAnc = do PegState s a w n c (x:p) <- get
            put $ PegState s a w n c p
            return x

doWord w = checkUnify $ do
  popStack
  m <- psWords <$> get
  pushArg (W w)
  case w `M.lookup` m of
    Nothing -> mzero
    Just [x] -> x 
    Just x -> msum x
  popArg
  return ()

checkUnify :: Peg () -> Peg ()
checkUnify m = do
  PegState s _ _ _ _ p <- get
  if topIs (isList ||. (== W "]")) s
    then m
    else case maybeAny (\x -> ((,) x) <$> unify s (x ++ [S "x"]) []) p of
           Nothing -> --trace (show $ map showStack p) $
                      pushAnc s >> m >> popAnc >> return ()
           Just (s', b) -> do sv <- newSVar
                              trace (showStack s ++ " == " ++
                                     showStack s') $ return ()
                              trace (show b) $ return ()
                              setStack [sv]
                              addConstraint ([sv], s)

addConstraint = let ?eval = eval in C.addConstraint

force = do
  st <- get
  case psStack st of
    (W w : _) -> doWord w
#ifdef DEBUG
      >> traceStack
#endif
    (S s : _) -> (popStack >> addConstraint ([S s], [])) `mplus` do
                   v <- newVar
                   s' <- newSVar
                   popStack
                   pushStack s'
                   pushStack v
                   addConstraint ([v, s'], [S s])
    _ -> return ()

bind nm l = modify $ \(PegState s a w n c p) ->
              PegState s a (minsert nm (f l) w) n c p
  where f l = do w <- popArg
                 appendStack l
                 force
                 pushArg w

unbind nm = modify $ \(PegState s a w n c p) -> PegState s a (M.delete nm w) n c p

eval s' = do
  st@(PegState s a w n c p) <- get
  put $ PegState s' [] w n c p
  force
  s'' <- getStack
  put st
  return s''
