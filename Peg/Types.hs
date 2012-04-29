

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
{-# LANGUAGE DeriveDataTypeable #-}
module Peg.Types where

import Control.Applicative
import Data.Maybe
import Control.Monad.Logic
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Exception hiding (try)
import Data.Typeable

type Stack = [Value]
type Env = Map String (Peg ())
data Constraint = IsEqualTo Value deriving (Show)
data PegState = PegState { psStack :: Stack,
                           psArgStack :: Stack,
                           psWords :: Env,
                           psUniqueVarCounter :: Int,
                           psConstraints :: Map String [Constraint] }
type Peg = StateT PegState (LogicT IO)
data PegException = PegException Stack Stack deriving (Show, Typeable)
instance Exception PegException
data Rule = Rule { getRule :: Stack -> Peg Stack }
data Value = F Double
           | I Integer
           | C Char
           | L Stack
           | W String
           | V String
           | Io
  deriving (Show, Eq, Ord)

isWord (W _) = True
isWord _ = False

isWordEq s (W s') = s == s'
isWordEq _ _ = False

isInt (I _) = True
isInt _ = False

isChar (C _) = True
isChar _ = False

isList (L _) = True
isList _  = False

isFloat (F _) = True
isFloat _ = False

toString :: Value -> Maybe String
toString (L l) = loop l
  where loop [] = return ""
        loop (C c : r) = (c:) <$> loop r
        loop _ = mzero
toString _ = mzero

isString = isJust . toString

isIo Io = True
isIo _ = False

isVar (V _) = True
isVar _ = False
