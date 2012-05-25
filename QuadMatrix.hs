{-# LANGUAGE PatternGuards, FlexibleContexts #-}
module QuadMatrix (QuadMatrix(..), qDim, qmatrix, (@>), (!@), (.@), foldrI, foldlI, minimumI, maximumI, (/@), (\@), qAccum, ZeroRule(..), printQMat, showQMat, tileSort, log2, qSeq) where

import Data.Bits
import Control.Monad
import Data.List
import Data.Maybe
import Data.Bits

import qualified Debug.Trace as T


pad n s | l >= n = take n s
        | otherwise = replicate (n-l) ' ' ++ s
  where l = length s

log2 x | x < 1 = error "log2 not defined for x < 1"
       | otherwise = snd . until ((==1) . fst) (\(x, n) -> (x `shiftR` 1, n + 1)) $ (x-1, 1)

data QuadMatrix a = QuadMatrix !(Int, Int) !Int !(QuadMatrix' a) deriving (Show)
data QuadMatrix' a = Quad !(QuadMatrix' a) !(QuadMatrix' a) !(QuadMatrix' a) !(QuadMatrix' a) |
                     Element !a |
                     Zero deriving (Show)

printQMat w a = putStrLn $ showQMat w a

showQMat w a = "\n" ++ replicate w ' ' ++ concatMap (\j -> ' ' : pad w ("["++show j++"]")) [0..n-1] ++ "\n" ++
               concatMap (\i-> pad w ("["++show i++"]") ++
                               concatMap (\j -> ' ' : pad w (show (a@>(i,j)))) [0..n-1] ++ "\n") [0..m-1]
  where (m, n) = qDim a

qSeq (QuadMatrix (m, n) l q) x = m `seq` n `seq` l `seq` q `qSeq'` x

qSeq' Zero x = x
qSeq' (Element e) x = e `seq` x
qSeq' (Quad a b c d) x = a `seq` b `seq` c `seq` d `seq` x

qDim (QuadMatrix d _ _) = d

qmatrix (m,n) = QuadMatrix (m, n) l Zero
  where l = log2 (max m n)

-- NOTE FOR FUNCTIONS TAKING A LIST OF ELEMENTS
-- they will be returned tile sorted (sorted as if the bits of the row and
-- column index were interleaved)

q @> e = snd . head $ q !@ [e]
q !@ es = foldrI IncludeZero (:) [] q es
q .@ es = foldrI ExcludeZero (:) [] q es

data ZeroRule = IncludeZero | ExcludeZero deriving (Show, Eq)

foldrI :: (Num b) => ZeroRule -> (((Int, Int), b) -> a -> a) -> a -> QuadMatrix b -> [(Int, Int)] -> a
foldrI z f s q es = d q' l es s
  where (QuadMatrix (m, n) l q') = q
        d _ _ [] s = s
        d (Element e) l [ij] s = f (ij, e) s
        d Zero l es s | z == ExcludeZero = s
                      | otherwise = foldr f s [ (ij, 0) | ij <- es ]
        d (Quad x00 x01 x10 x11) l es s = d x00 (l-1) es00 .
                                          d x01 (l-1) es01 .
                                          d x10 (l-1) es10 .
                                          d x11 (l-1) es11 $ s
          where (es00, es01, es10, es11) = quadSplit id (l-1) es

foldlI :: (Num b) => ZeroRule -> (a -> ((Int, Int), b) -> a) -> a -> QuadMatrix b -> [(Int, Int)] -> a
foldlI z f s q es = d q' l es s
  where (QuadMatrix (m, n) l q') = q
        d _ _ [] s = s
        d (Element e) l [ij] s = f s (ij, e)
        d Zero l es s | z == ExcludeZero = s
                      | otherwise = foldl f s [ (ij, 0) | ij <- es ]
        d (Quad x00 x01 x10 x11) l es s = d x11 (l-1) es11 .
                                          d x10 (l-1) es10 .
                                          d x01 (l-1) es01 .
                                          d x00 (l-1) es00 $ s
          where (es00, es01, es10, es11) = quadSplit id (l-1) es

minimumI z q es = foldlI z (\m a@(_, ax) -> Just $ maybe a (\b@(_, bx) -> if ax < bx then a else b) m) Nothing q es
maximumI z q es = foldlI z (\m a@(_, ax) -> Just $ maybe a (\b@(_, bx) -> if ax > bx then a else b) m) Nothing q es

q /@ es = QuadMatrix (m,n) l (accumQ IncludeZero (\_ _ x -> x) q' l es)
  where (QuadMatrix (m, n) l q') = q

q \@ es = QuadMatrix (m,n) l (accumQ ExcludeZero (\_ _ x -> x) q' l es)
  where (QuadMatrix (m, n) l q') = q

{-# SPECIALIZE accumQ :: ZeroRule -> ((Int, Int) -> Double -> Double -> Double) -> QuadMatrix' Double -> Int -> [((Int, Int), Double)] -> QuadMatrix' Double #-}
accumQ :: (Num e) => ZeroRule -> ((Int, Int) -> e -> a -> e) -> QuadMatrix' e -> Int -> [((Int, Int), a)] -> QuadMatrix' e
accumQ _ _ q _ [] = q
accumQ z f (Element e) l [(ij, x)] | e' == 0 = Zero
                                   | otherwise = Element e'
  where e' = f ij e x
accumQ z f Zero l es | z == ExcludeZero = Zero
                     | l <= 0 = if e' == 0 then Zero else Element e'
                     | otherwise = collapse $ accumQ z f (Quad Zero Zero Zero Zero) l es
  where e' = f ij 0 x
        [(ij, x)] = es
accumQ z f (Quad x00 x01 x10 x11) l es = collapse $ Quad (accumQ z f x00 (l-1) es00)
                                                            (accumQ z f x01 (l-1) es01)
                                                            (accumQ z f x10 (l-1) es10)
                                                            (accumQ z f x11 (l-1) es11)
  where (es00, es01, es10, es11) = quadSplit fst (l-1) es

--diff xs ys = (filter (not . (`elem` ys)) xs, filter (not . (`elem` xs)) ys)

collapse q | Quad Zero Zero Zero Zero <- q = Zero
           | otherwise = q

qMapI z f q es = QuadMatrix (m,n) l (accumQ z (\_ x _ -> f x) q' l (zip es (repeat ())))
  where (QuadMatrix (m, n) l q') = q

qMap z f q = qMapI z f q [ (i, j) | i <- [0..m-1], j <- [0..n-1] ]
  where (m, n) = qDim q

qAccum :: (Num a) => ZeroRule -> (a -> b -> a) -> QuadMatrix a -> [((Int, Int), b)] -> QuadMatrix a
{-# SPECIALIZE qAccum :: ZeroRule -> (Double -> Double -> Double) -> QuadMatrix Double -> [((Int, Int), Double)] -> QuadMatrix Double #-}
qAccum z f q es = QuadMatrix (m, n) l (accumQ z (\_ x y -> f x y) q' l es)
  where (QuadMatrix (m, n) l q') = q

tileSort f es = tileSort' f l es []
  where (m, n) = foldl (\(m, n) e -> let (i, j) = f e in (max m i, max n j)) (0, 0) es
        l = log2 (max m n + 1)


--tileSort' :: (a -> (Int, Int)) -> Int -> Int -> [a] -> [a] -> [a]
tileSort' _ _ [] r = r
tileSort' f l es r | l <= 0 = es ++ r
                   | otherwise = tileSort' f (l-1) es00 .
                                 tileSort' f (l-1) es01 .
                                 tileSort' f (l-1) es10 .
                                 tileSort' f (l-1) es11 $ r
  where (es00, es01, es10, es11) = quadSplit f l es

--quadSplit :: (a -> (Int, Int)) -> Int -> [a] -> ([a], [a], [a], [a])
quadSplit f l es = foldl' split ([], [], [], []) es
  where split (es00, es01, es10, es11) e = 
            if testBit i l
              then if testBit j l
                     then (es00, es01, es10, e:es11)
                     else (es00, es01, e:es10, es11)
              else if testBit j l
                     then (es00, e:es01, es10, es11)
                     else (e:es00, es01, es10, es11)
          where (i, j) = f e
{-
quadSplit f l es = foldr split ([], [], [], []) es
  where split e (es00, es01, es10, es11) = 
          if i < 0
             then if j < 0
                    then (e:es00, e:es01, e:es10, e:es11)
                    else if testBit j l
                           then (es00, e:es01, es10, e:es11)
                           else (e:es00, es01, e:es10, es11)
             else if j < 0
                    then if testBit i l
                           then (es00, es01, e:es10, e:es11)
                           else (e:es00, e:es01, es10, es11)
                    else if testBit i l
                           then if testBit j l
                                  then (es00, es01, es10, e:es11)
                                  else (es00, es01, e:es10, es11)
                           else if testBit j l
                                  then (es00, e:es01, es10, es11)
                                  else (e:es00, es01, es10, es11)
          where (i, j) = f e
-}
