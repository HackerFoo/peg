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

{-# LANGUAGE ImplicitParams #-}
module Peg.Constraints where

import Peg.Types
import Peg.Utils
import Peg.Parse

import Control.Applicative
import Control.Monad.State
import Data.List

substVar x y = do
  PegState s a w n c p <- get
  let (cp, cn) = partition (\(l, r) -> x `elem` l || x `elem` r) c
  put $ PegState (subst x y s) (subst x y a) w n cn p
  mapM_ (\(l, r) -> addConstraint (subst x y l, subst x y r)) cp

substSVar x ys = do
  PegState s a w n c p <- get
  let (cp, cn) = partition (\(l, r) -> x `elem` l || x `elem` r) c
  put $ PegState (substs x ys s) (substs x ys a) w n cn p
  mapM_ (\(l, r) -> addConstraint (substs x ys l, substs x ys r)) cp

getConstraints :: Peg [(Stack, Stack)]
getConstraints = psConstraints <$> get
setConstraints c = do PegState s a w n _ p <- get
                      put $ PegState s a w n c p

isIoPrimitive (W "getChar#") = True
isIoPrimitive (W "putChar#") = True
isIoPrimitive _ = False

addConstraint ([A "True"], [W "eq?", v@(V _), x]) = substVar v x
addConstraint ([A "True"], [W "eq?", x, v@(V _)]) = substVar v x
addConstraint ([I x], [W "add_int#", v@(V _), I y]) = substVar v (I $ x - y)
addConstraint ([I x], [W "add_int#", I y, v@(V _)]) = substVar v (I $ x - y)
addConstraint ([I x], [W "add_int#", v'@(V _), v@(V _)]) 
  | v == v' = guard (x `mod` 2 == 0) >> substVar v (I $ x `div` 2)
addConstraint ([I x], [W "sub_int#", v@(V _), I y]) = substVar v (I $ y - x)
addConstraint ([I x], [W "sub_int#", I y, v@(V _)]) = substVar v (I $ y + x)
addConstraint ([I x], [W "sub_int#", v'@(V _), v@(V _)])
  | v == v' = guard (x == 0)
addConstraint ([I x], [W "mul_int#", v@(V _), I y]) =
  guard (x `mod` y == 0) >> substVar v (I $ x `div` y)
addConstraint ([I x], [W "mul_int#", I y, v@(V _)]) =
  guard (x `mod` y == 0) >> substVar v (I $ x `div` y)
addConstraint ([I x], [W "mul_int#", v'@(V _), v@(V _)])
  | v == v' = guard (x >= 0 && r*r == x) >> substVar v (I r)
  where r = round . sqrt . realToFrac $ x
addConstraint ([I x], [W "div_int#", v@(V _), I y]) = 
  guard (y `mod` x == 0) >> substVar v (I $ y `div` x)
addConstraint ([I x], [W "div_int#", I y, v@(V _)]) =
  msum $ map (substVar v . I) [y * x .. y*(x+1)-1]
addConstraint ([I x], [W "div_int#", v'@(V _), v@(V _)])
  | v == v' = guard $ x == 1
addConstraint ([F x], [W "add_float#", v@(V _), F y]) =
  substVar v (F $ x - y)
addConstraint ([F x], [W "add_float#", F y, v@(V _)]) =
  substVar v (F $ x - y)
addConstraint ([F x], [W "add_float#", v@(V _), v'@(V _)])
  | v == v' = substVar v (F $ x / 2)
addConstraint ([F x], [W "sub_float#", v@(V _), F y]) =
  substVar v (F $ y - x)
addConstraint ([F x], [W "sub_float#", F y, v@(V _)]) =
  substVar v (F $ y + x)
addConstraint ([F x], [W "sub_float#", v@(V _), v'@(V _)])
  | v == v' = guard $ x == 0
addConstraint ([F x], [W "mul_float#", v@(V _), F y]) =
  substVar v (F $ x / y)
addConstraint ([F x], [W "mul_float#", F y, v@(V _)]) =
  substVar v (F $ x / y)
addConstraint ([F x], [W "mul_float#", v@(V _), v'@(V _)])
  | v == v' = guard (x >= 0) >> substVar v (F $ sqrt x)
addConstraint ([F x], [W "divide_float#", v@(V _), F y])
  = substVar v (F $ y / x)
addConstraint ([F x], [W "divide_float#", F y, v@(V _)])
  = substVar v (F $ y * x)
addConstraint ([F x], [W "divide_float#", v@(V _), v'@(V _)])
  | v == v' = guard $ x == 1
addConstraint ([h, L t], [W "popr", x@(V _)])
  = substVar x . L $ h:t
addConstraint ([h, t@(V _)], [W "popr", x@(V _)])
  = substVar x . L $ [h, W "$#", t]
addConstraint ([sv@(S _)], x)
  = addConstraint' ([sv], x) --substSVar sv x
addConstraint (l, r)
  | not (any (has $ isVar ||. isStackVar ||. isIoPrimitive) r) = unify' l =<< ?eval r
addConstraint x = addConstraint' x

addConstraint' x = modify $ \(PegState s a w n c p) -> PegState s a w n (x:c) p

unify' x y = do [sx, sy] <- replicateM 2 newSVar
                b <- unify (x ++ [sx]) (y ++ [sy]) []
                mapM_ (\(v, x) -> substVar v x) b

incVarCounter = do PegState s a w n c p <- get
                   put $ PegState s a w (n+1) c p
                   return n

newVar :: Peg Value
newVar = V . ('_' :) . letNum <$> incVarCounter

newSVar :: Peg Value
newSVar = S . ('_' :) . letNum <$> incVarCounter

unifyTest x y = unify x' y' []
  where Right x' = parseStack x
        Right y' = parseStack y
