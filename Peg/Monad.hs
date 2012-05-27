
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
import Peg.Parse --(traceStack)

import Control.Applicative
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Control.Exception
import Data.List

import Debug.Trace

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
newVar = do PegState s a w n c p <- get
#ifdef DEBUG
            when (n > 25) mzero
#endif
            put $ PegState s a w (n+1) c p
            return . V $ '_': letNum n

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

pushAnc = modify $ \(PegState s a w n c p) -> PegState s a w n c (s:p)
popAnc = modify $ \(PegState s a w n c (_:p)) -> PegState s a w n c p

doWord w = checkUnify $ do
  popStack
  m <- psWords <$> get
  pushArg (W w)
  case w `M.lookup` m of
    Nothing -> mzero --pushStack (W w)
    Just [x] -> x 
    Just x -> msum x
  popArg
  return ()

checkUnify :: Peg () -> Peg ()
checkUnify m = do
  PegState s _ _ _ _ p <- get
  if null ||. ((isList ||. (== W "]")) . head) $ s
    then m
    else case maybeAny (\x -> ((,) x) <$> unify s (x ++ [V "@x"]) []) p of
           Nothing -> --trace (show $ map showStack p) $
                      pushAnc >> m >> popAnc
           Just (s', b) -> do v <- newVar
                              trace (showStack s ++ " == " ++
                                     showStack s') $ return ()
                              addConstraint ([v], [L s])
                              setStack [W "$#", v]

force = do
  st <- get
  case psStack st of
    (W w : _) -> doWord w
#ifdef DEBUG
      >> traceStack
#endif
    _ -> return ()

(f ||. g) x = f x || g x
(f &&. g) x = f x && g x

minsert k x = M.insertWith (++) k [x]
mlookup k = maybe [] id . M.lookup k

subst a b xs = map f xs
  where f (L xs) = L $ subst a b xs
        f x | x == a = b
            | otherwise = x

addConstraint x = modify $ \(PegState s a w n c p) -> PegState s a w n (x:c) p

substVar f x y = do
  PegState s a w n c p <- get
  let (cp, cn) = partition (\(l, r) -> x `elem` l || x `elem` r) c
  put $ PegState (subst x y s) (subst x y a) w n cn p
  mapM_ (\(l, r) -> f (subst x y l, subst x y r)) cp

getConstraints :: Peg [(Stack, Stack)]
getConstraints = psConstraints <$> get
setConstraints c = do PegState s a w n _ p <- get
                      put $ PegState s a w n c p

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

maybeAny _ [] = Nothing
maybeAny f (x:xs) = case f x of
                      Nothing -> maybeAny f xs
                      r -> r

unify [] [] b = return b
unify [] _ b = mzero
unify _ [] b = mzero
unify [V v@('@':_)] ys b = updateBindings v (L ys) b
unify xs [V v@('@':_)] b = updateBindings v (L xs) b
unify (V x:xs) (y:ys) b | not (isWord y) =
  unify (subst (V x) y xs) ys =<< updateBindings x y b
unify (x:xs) (V y:ys) b | not (isWord x) =
  unify xs (subst (V y) x ys) =<< updateBindings y x b
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
  guard . not $ occurs (V v) x
  b' <- mapM (\(a, z) -> let z' = subst1 (V v) x z in
                           guard (not $ occurs (V a) z') >> return (a, z')) b
  return $ (v,x) : b

gatherList n l (w@(W "]") : s) = gatherList (n+1) (w:l) s
gatherList n l (w@(A "[") : s)
  | n <= 0 = Right (l,s)
  | otherwise = gatherList (n-1) (w:l) s
gatherList n l (w:s) = gatherList n (w:l) s
gatherList n l [] = Left l
