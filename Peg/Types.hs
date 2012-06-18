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

module Peg.Types where

import Control.Applicative
import Data.Maybe
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Search

type Stack = [Value]
type Env = Map String [Peg ()]
data PegState = PegState { psStack :: Stack,
                           psArgStack :: Stack,
                           psWords :: Env,
                           psUniqueVarCounter :: Int,
                           psConstraints :: ([(Stack, Stack)], [(Stack, Stack)]),
                           psBindings :: Map String Value,
                           psAncestors :: [Stack] }
type Peg = StateT PegState Tree
data Rule = Rule { getRule :: Stack -> Peg Stack }
data Value = F Double  -- float
           | I Integer -- integer
           | C Char    -- character
           | L Stack   -- stack
           | W String  -- word
           | A String  -- atom
           | V String  -- variable
           | S String  -- stack variable
           | Io        -- I/O token
  deriving (Show, Eq, Ord)

isWord (W _) = True
isWord _ = False

isAtom (A _) = True
isAtom _ = False

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

isStackVar (S _) = True
isStackVar _ = False

has p (L l) = any (has p) l
has p x = p x

class ValueT a where
  toValue :: a -> Value
  fromValue :: Value -> Maybe a

instance ValueT Double where
  toValue = F
  fromValue (F x) = Just x
  fromValue _ = Nothing

instance ValueT Integer where
  toValue = I
  fromValue (I x) = Just x
  fromValue _ = Nothing

instance ValueT Char where
  toValue = C
  fromValue (C x) = Just x
  fromValue _ = Nothing

instance ValueT Bool where
  toValue = A . show
  fromValue (A "True") = Just True
  fromValue (A "False") = Just False
  fromValue _ = Nothing

instance (ValueT a) => ValueT [a] where
  toValue = L . map toValue
  fromValue (L x) = mapM fromValue x
  fromValue _ = Nothing

ofType :: (ValueT a) => Value -> a -> Bool
ofType v x = isJust $ fromValue v `asTypeOf` Just x

val :: (ValueT a) => Value -> a
val = fromJust . fromValue

