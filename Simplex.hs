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

{-# LANGUAGE FlexibleContexts, ParallelListComp #-}
{-# OPTIONS_GHC -fglasgow-exts -fenable-rewrite-rules #-}
module Simplex {-(simplex, values, solve, Constraint(..))-} where

import Control.Monad
import Control.Monad.ST
import Data.Maybe
import Data.List
import Data.Ord

--import Utils hiding (trace)
import QuadMatrix
import Debug.Trace -- as Trace

sign x = if x < 0 then -1 else 1

class Reducible a where
  reduce :: a -> a

instance Reducible (Int, Int) where
  reduce (x,y) = (abs x `div` d, signum x * y `div` d)
    where d = gcd x y

instance Reducible (Double, Double) where
  reduce (x,y) = (scaleFloat s x', scaleFloat s y')
    --where s = 1 - min (exponent x') (exponent y')
    where s = 1 - ((exponent x' + exponent y') `div` 2)
          x' = abs x
          y' = if x < 0 then -y else y

{-# SPECIALIZE selectColS :: QuadMatrix Double -> Maybe Int #-}
selectColS a | Just ((_, j), x) <- minimumI ExcludeZero a [ (0, j) | j <- [1..n-1] ] = if x < 0 then Just j else Nothing
             | otherwise = Nothing
  where (_, n) = qDim a

{-# SPECIALIZE selectColP :: QuadMatrix Double -> Int -> Maybe Int #-}
selectColP a r | Just ((_, j), x) <- maximumI ExcludeZero a [ (r, j) | j <- [1..n-1] ] = if x > 0 then Just j else Nothing
               | otherwise = Nothing
  where (_, n) = qDim a

{-# SPECIALIZE selectPivot :: QuadMatrix Double -> Int -> Maybe Int #-}
selectPivot a c | Just (_, _, i) <- mn = Just i
                | otherwise = Nothing
  where (m, _) = qDim a
        p = foldrI ExcludeZero (\x@(_, e) xs -> if e > 0 then x:xs else xs) [] a [ (i,c) | i <- [1..m-1] ]
        (_, mn) = foldlI IncludeZero f (p, Nothing) a [ (i,0) | ((i, _), _) <- p ]
        f ((_, d):ds, Nothing) ((i, _), n) = (ds, Just (n, d, i))
        f ((_, d):ds, m@(Just (mn, md, mi))) ((i, _), n) | n*md < mn*d = (ds, Just (n, d, i))
                                                         | otherwise = (ds, m)

{-# SPECIALIZE pivot :: Int -> Int -> QuadMatrix Double -> QuadMatrix Double #-}
pivot r c a = a'''
  where (m, n) = qDim a
        p = a @> (r,c)
        col = a .@ [ (i,c) | i <- [0..m-1], i /= r ]
        row = a .@ [ (r,j) | j <- [0..n-1], j /= c ]
        rat = map (\((i,_), x) -> (i, reduce (p, x))) col
        a' = a \@ [ ((i,c), 0) | i <- [0..m-1], i /= r ]
        a'' = qAccum ExcludeZero (*) a' [ ((i,j), x) | (i, (x, _)) <- rat, j <- [0..n-1], j /= c, x /= 1]
        a''' = qAccum IncludeZero (+) a'' [ ((i, j), -x*y) | (i, (_, y)) <- rat, ((_, j), x) <- row ]

{-# SPECIALIZE simplexS :: QuadMatrix Double -> Maybe (QuadMatrix Double) #-}
simplexS a = case selectColS a of
              Nothing -> Just a
              Just c -> case selectPivot a c of
                Nothing -> Nothing
                Just r -> simplexS $ pivot r c a

{-# SPECIALIZE infeasible :: QuadMatrix Double -> Maybe Int #-}
infeasible a | t > m = Nothing
             | otherwise = Just t 
  where t = foldl f (m+1) [0..n-1]
        f t j | [((i,_), x)] <- a .@ [ (i, j) | i <- [0..t-1]] =
                if x < 0 && null (a .@ [ (i, j) | i <- [t..m-1] ]) then i else t
              | otherwise = t
        (m, n) = qDim a

-- what to do in loop? e.g. solve [0,1] [[5,-1,-1::Double]]

{-# SPECIALIZE simplex :: QuadMatrix Double -> Maybe (QuadMatrix Double) #-}
simplex a = case infeasible a of
              Nothing -> simplexS a
              Just r -> case do c <- selectColP a r 
                                r <- selectPivot a c
                                return (r,c) of
                          Nothing -> Nothing
                          Just (r,c) -> simplex $ pivot r c a

{-# SPECIALIZE values :: QuadMatrix Double -> [Double] #-}
values a = map ans [1..n-1]
  where (m, n) = qDim a
        ans j | [((i, _), x)] <- a .@ [ (i, j) | i <- [0..m-1] ] = realToFrac (a @> (i, 0)) / realToFrac x
              | otherwise = 0

--solve :: (Real e, Fractional e, Reducible (e,e), IArray UArray e) => [e] -> [Constraint e] -> Maybe [e]

--{-# SPECIALIZE solve :: [Double] -> [[Double]] -> Maybe [Double] #-}
solve c ba = liftM (take (length c) . values) . simplex $ augmentedMatrix c ba

{-# SPECIALIZE augmentedMatrix :: [Double] -> [[Double]] -> QuadMatrix Double #-}
augmentedMatrix c ba = qmatrix (1 + lba, 1 + lc + lba) /@ es
  where lc = length c
        lba = length ba
        es = [ ((0, j), x) | x <- c | j <- [1..] ] ++
             concat [ [ ((i, j), x) | j <- [0..] | x <- (abs b : row)] | i <- [1..] | (b : row) <- ba ] ++
             [ ((i, i + lc), sign (head x)) | i <- [1..] | x <- ba ]
