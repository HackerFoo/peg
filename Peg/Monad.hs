
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

module Peg.Monad where

import Peg.Types

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Control.Exception hiding (try)

-- | pop an argument from the stack, push onto argument stack
getArg' check st = do
  force
  em <- emptyStack
  if em
    then done
    else do
      x <- popStack
      if check x
        then return ()
        else if st x
               then pushStack x >> done
               else mzero
      pushArg x

getArg ch = getArg' ch ((== W "[") ||. (== W "]"))
getArgNS ch = getArg' ch (== W "[")

pushStack x = modify (\(PegState s a m xx) -> PegState (x:s) a m xx)
appendStack x = modify (\(PegState s a m xx) -> PegState (x++s) a m xx)

popStack :: Peg Value
popStack = do PegState (x:s) a m xx <- get
              put $ PegState s a m xx
              return x
emptyStack = null . psStack <$> get

-- | can't go any further, bail
done = do
  st <- get
  liftIO . throwIO $ PegException (psStack st) (psArgStack st)

pushArg x = modify (\(PegState s a m xx) -> PegState s (x:a) m xx)
popArg = do PegState s (x:a) m xx <- get
            put $ PegState s a m xx
            return x

doWord w = do
  m <- psWords <$> get
  case w `M.lookup` m of
    Nothing -> pushStack (W w)
    Just f -> pushArg (W w) >> f >> popArg >> return ()

force = do
  st <- get
  case psStack st of
    (W w : _) -> popStack >> doWord w -- >> traceStack
    _ -> return ()

(f ||. g) x = f x || g x
(f &&. g) x = f x && g x

