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

module Peg.Utils where

import Peg.Types

import Control.Monad
import Data.Map (Map)
import qualified Data.Map as M

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

topIs _ [] = False
topIs f (x:_) = f x

(f ||. g) x = f x || g x
(f &&. g) x = f x && g x

minsert k x = M.insertWith (++) k [x]
mlookup k = maybe [] id . M.lookup k


maybeAny _ [] = Nothing
maybeAny f (x:xs) = case f x of
                      Nothing -> maybeAny f xs
                      r -> r

unify [] [] b = return b
--unify [s@(S _)] ys b = updateBindings s (L ys) b
unify xs [s@(S _)] b = updateBindings s (L xs) b
unify [] _ b = mzero
unify _ [] b = mzero
unify (V x:xs) (y:ys) b | not (isWord y) =
  unify (subst (V x) y xs) ys =<< updateBindings (V x) y b
unify (x:xs) (V y:ys) b | not (isWord x) =
  unify xs (subst (V y) x ys) =<< updateBindings (V y) x b
unify (L x:xs) (L y:ys) b = unify xs ys =<< unify x y b
unify (L x:xs) (W "]":ys) b =
  case gatherList 0 [] ys of
    Left _ -> mzero
    Right (y', ys') -> unify (L x:xs) (L (reverse y'):ys') b
unify (W "]":xs) (L y:ys) b =
  case gatherList 0 [] xs of
    Left _ -> mzero
    Right (x', xs') -> unify (L (reverse x'):xs') (L y:ys) b
unify (x:xs) (y:ys) b
  | x == y = unify xs ys b
  | otherwise = mzero

subst1 a b (L xs) = L (subst a b xs)
subst1 a b x | a == x = b
             | otherwise = x

occurs a (L bs) = any (occurs a) bs
occurs a b = a == b

updateBindings v x b = do
  guard . not $ occurs v x
  b' <- mapM (\(a, z) -> let z' = subst1 v x z in
                           guard (not $ occurs v z') >> return (a, z')) b
  return $ (v,x) : b

gatherList n l (w@(W "]") : s) = gatherList (n+1) (w:l) s
gatherList n l (w@(A "[") : s)
  | n <= 0 = Right (l,s)
  | otherwise = gatherList (n-1) (w:l) s
gatherList n l (w:s) = gatherList n (w:l) s
gatherList n l [] = Left l

subst a b xs = map f xs
  where f (L xs) = L $ subst a b xs
        f x | x == a = b
            | otherwise = x

substs a bs xs = concatMap f xs
  where f (L xs) = [L $ substs a bs xs]
        f x | x == a = bs
            | otherwise = [x]
