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

{-# LANGUAGE ImplicitParams, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module Peg.Constraints where

import Peg.Types
import Peg.Utils
import Peg.Parse (parseStack)

import Control.Applicative
import Control.Monad.State
import Data.List
import Data.Maybe
import qualified Data.Map as M
import Data.Map (Map)

import Debug.Trace

substVar :: Value -> [Value] -> Peg Bool
substVar x ys = do
  PegState s a w n (cc, cd) b p <- get
  let (cp, cc') = partition (\(l, r) -> x `elem` l || x `elem` r) cc
  let cd' = map (\(l,r) -> (substs x ys l, substs x ys r)) $ cp ++ cd
  put $ PegState (substs x ys s) (substs x ys a) w n (cc', cd') b p
  processConstraints
  return True

processConstraints :: Peg ()
processConstraints = do
  PegState s a w n (cc, cd) b p <- get
  if null cd
    then return ()
    else do put $ PegState s a w n (cc, tail cd) b p
            addConstraint $ head cd
            processConstraints

getConstraints :: Peg [(Stack, Stack)]
getConstraints = fst . psConstraints <$> get
setConstraints c = do PegState s a w n _ b p <- get
                      put $ PegState s a w n c b p

isIoPrimitive (W "getChar#") = True
isIoPrimitive (W "putChar#") = True
isIoPrimitive _ = False

ensure x y
  | isVar x = substVar x [toValue y]
  | x `ofType` y = guard (val x == y) >> return True

anti :: (ValueT a, Eq a) => String -> (a -> a -> a) -> a -> [Value] -> [Value] -> Peg Bool
anti w' op i [y, x] [z]
  | isVar x && isVal y && isVal z = addConstraint ([x], [W w', y, z])
  | isVal x && isVar y && isVal z = substVar y [toValue $ val x `op` val z]
  | isVal x && isVal y && isVar z = substVar z [toValue $ val x `op` val y]
  | x == y = ensure z i
  | x == z = ensure y i
  | y == z = addConstraint ([x], [W w', y, z])
  | otherwise = return False
  where isVal = flip ofType i

sym :: (ValueT a, Eq a) => String -> (a -> a -> a) -> a -> [Value] -> [Value] -> Peg Bool
sym w' op i [y, x] [z]
  | isVar x && isVal y && isVal z = addConstraint ([x], [W w', y, z])
  | isVal x && isVar y && isVal z = addConstraint ([x], [W w', y, z])
  | isVal x && isVal y && isVar z = substVar z [toValue $ val x `op` val y]
  | x == y = return False -- later
  | x == z = ensure y i
  | y == z = ensure x i
  | otherwise = return False
  where isVal = flip ofType i

op21 :: (ValueT a, ValueT b, ValueT c, Eq c) => (a -> b -> c) -> Value -> Value -> Value -> Peg Bool
op21 op x y z
  | Just x' <- fromValue x, Just y' <- fromValue y = ensure z $ x' `op` y'
  | otherwise = return False --addConstraint' ([z], [W w, y, x])

op22 :: (ValueT a, ValueT b, ValueT c, ValueT d, Eq c, Eq d) => (a -> b -> (c, d)) -> Value -> Value -> Value -> Value -> Peg Bool
op22 op x y z i
  | Just x' <- fromValue x, Just y' <- fromValue y =
      let (z', i') = x' `op` y' in ensure z z' >> ensure i i'
  | otherwise = return False --addConstraint' ([i, z], [W w, y, x])

op11 :: (ValueT a, ValueT b, Eq b) => (a -> b) -> Value -> Value -> Peg Bool
op11 op x y
  | Just x' <- fromValue x = ensure y (op x')
  | otherwise = return False --addConstraint' ([y], [W w, x])

type IntF21 = Integer -> Integer -> Integer
type IntF11 = Integer -> Integer
type IntF22 = Integer -> Integer -> (Integer, Integer)

class Op f where
  op :: f -> [Value] -> [Value] -> Peg Bool

instance (ValueT a, ValueT b, Eq b) => Op (a -> b) where
  op f [x] [y] = op11 f x y
instance (ValueT a, ValueT b, ValueT c, Eq c) => Op (a -> b -> c) where
  op f [y, x] [z] = op21 f x y z
instance (ValueT a, ValueT b, ValueT c, ValueT d, Eq c, Eq d) => Op (a -> b -> (c, d)) where
  op f [y, x] [i, z] = op22 f x y z i

eqC [x, y] [A "True"]
  | isVar x = substVar x [y]
  | isVar y = substVar y [x]
  | isList x && isList y = mzero
  | x == y = return True
  | x /= y = mzero
eqC [x, y] [A "False"]
  | not (isVar x) && not (isVar y) =
      if x /= y
        then return True
        else mzero
  | otherwise = return False
eqC [x, y] [V v]
  | not (isVar x) && not (isVar y) = substVar (V v) [A . show $ x == y]
  | otherwise = return False
eqC _ _ = mzero

--predC p [V v] [x, y] | V v /= y = substVar (V v) [y] >> predC p [y] [x, y]
--predC p [y] [x, V v] | V v /= y = substVar (V v) [y] >> predC p [y] [x, y]
predC _ [V _] [_, _] = return False
predC p [x] [A "False", _] = guard (not $ p x) >> return True
predC p [x] [A "True", _] = guard (p x) >> return True
predC p [x] [V v, _]
  | not (isVar x) = substVar (V v) [A . show $ p x]
  | otherwise = return False
predC _ _ _ = mzero

poprC [h, L t] [x@(V _)] = substVar x [L $ h:t]
poprC [h, t@(V _)] [x@(V _)] = substVar x [L $ [h, W "$#", t]]
poprC _ _ = return False

floatInt :: (Double -> Integer -> a) -> Double -> Integer -> a
floatInt = id

float :: (Double -> a) -> Double -> a
float = id

char :: (Char -> a) -> Char -> a
char = id

float2 :: (Double -> Double -> a) -> Double -> Double -> a
float2 = id

int2 :: (Integer -> Integer -> a) -> Integer -> Integer -> a
int2 = id

int_float :: (Integer -> Double) -> Integer -> Double
int_float = id

float_int :: (Double -> Integer) -> Double -> Integer
float_int = id

wordConstraints = M.fromList [
  ("eq?", eqC),
  --("popr", poprC),
  ("add_int#", sym "sub_int#" (+) (0 :: Integer)),
  ("sub_int#", anti "add_int#" (-) (0 :: Integer)),
  ("mul_int#", sym "div_int#" (*) (1 :: Integer)),
  ("div_int#", anti "mul_int#" div (1 :: Integer)),
  ("add_float#", sym "sub_float#" (+) (0 :: Double)),
  ("sub_float#", anti "add_float#" (-) (0 :: Double)),
  ("mul_float#", sym "divide_float#" (*) (1 :: Double)),
  ("divide_float#", anti "mul_float#" (/) (1 :: Double)),
  ("mod_int#", op $ int mod),
  ("divMod_int#", op $ int divMod),
  ("quot_int#", op $ int quot),
  ("rem_int#", op $ int rem),
  ("quotRem_int#", op $ int quotRem),
  ("pos_power_int#", op $ int2 (^)),
  ("pos_power_float#", op $ floatInt (^)),
  ("int_power_float#", op $ floatInt (^^)),
  ("power_float#", op $ float2 (**)),
  ("exp#", op $ float exp),
  ("sqrt#", op $ float sqrt),
  ("log#", op $ float log),
  ("logBase#", op $ float logBase),
  ("sin#", op $ float sin),
  ("tan#", op $ float tan),
  ("cos#", op $ float cos),
  ("asin#", op $ float asin),
  ("atan#", op $ float atan),
  ("acos#", op $ float acos),
  ("sinh#", op $ float sinh),
  ("tanh#", op $ float tanh),
  ("cosh#", op $ float cosh),
  ("asinh#", op $ float asinh),
  ("atanh#", op $ float atanh),
  ("acosh#", op $ float acosh),
  ("lt_int#", op $ int (<)),
  ("lte_int#", op $ int (<=)),
  ("gt_int#", op $ int (>)),
  ("gte_int#", op $ int (>=)),
  ("lt_float#", op $ float (<)),
  ("lte_float#", op $ float (<=)),
  ("gt_float#", op $ float (>)),
  ("gte_float#", op $ float (>=)),
  ("lt_char#", op $ char (<)),
  ("lte_char#", op $ char (<=)),
  ("gt_char#", op $ char (>)),
  ("gte_char#", op $ char (>=)),
  ("intToFloat#", op $ int_float realToFrac),
  ("round#", op $ float_int round),
  ("floor#", op $ float_int floor),
  ("ceiling#", op $ float_int ceiling),
  ("int?", predC isInt),
  ("float?", predC isFloat),
  ("word?", predC isWord),
  ("list?", predC isList),
  ("char?", predC isChar),
  ("io?", predC isIo),
  ("hasIO?", predC $ has isIo)]

int :: (Integer -> a) -> Integer -> a
int = id

addConstraint :: ([Value], [Value]) -> Peg Bool
addConstraint (y, W w:x) | Just f <- w `M.lookup` wordConstraints =
  do b <- f x y
     when (not b) $ addConstraint' (y, W w:x)
     return True
addConstraint ([sv@(S _)], x)
  = addConstraint' ([sv], x) >> return True --substSVar sv x
addConstraint (l, [v@(V _)]) = substVar v l
addConstraint x = addConstraint' x >> return True

addConstraint' :: ([Value], [Value]) -> Peg ()
addConstraint' x = modify (\(PegState s a w n (cc,cd) b p) ->
                             PegState s a w n (x:cc,cd) b p)

unify' x y = do [sx, sy] <- replicateM 2 newSVar
                b <- unify (x ++ [sx]) (y ++ [sy]) []
                mapM_ substBinding b

substBinding (v@(V _), x) = substVar v [x]
substBinding (s@(S _), L x) = substVar s x
substBinding (L x, L y) = addConstraint (x, y)
substBinding _ = error "substBinding: invalid bindings"

incVarCounter = do PegState s a w n c b p <- get
                   put $ PegState s a w (n+1) c b p
                   return n

newVar :: Peg Value
newVar = V . ('_' :) . letNum <$> incVarCounter

newSVar :: Peg Value
newSVar = S . ('_' :) . letNum <$> incVarCounter

unifyTest x y = unify x' y' []
  where Right x' = parseStack x
        Right y' = parseStack y
