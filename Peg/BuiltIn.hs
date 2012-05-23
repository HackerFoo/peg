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

{-# LANGUAGE FlexibleInstances #-}
module Peg.BuiltIn where

import Peg.Types
import Peg.Monad
import Peg.Parse

import Control.Applicative
import Data.List
--import Control.Monad.Logic
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

-------------------- Converters --------------------

withArgs cI nO f = do
  mapM_ (getArg . (isVar ||.)) $ reverse cI
  i <- replicateM (length cI) popArg
  if any isVar i
    then do w@(W _) <- peekArg
            vs <- replicateM nO newVar
            addConstraintP (vs, w:i)
            appendStack vs
    else f i

op' cI nO f = withArgs cI nO (appendStack . f)

class Op f where
  op :: f -> Peg ()

instance Op (Integer -> Integer -> Integer) where op = op2i_i
instance Op (Double -> Double -> Double) where op = op2f_f
instance Op (Double -> Integer -> Double) where op = opfi_f
instance Op (Double -> Double) where op = opf_f
instance Op (Double -> Integer) where op = opf_i
instance Op (Integer -> Double) where op = opi_f
instance Op (Integer -> Integer -> Bool) where op = op2i_b
instance Op (Double -> Double -> Bool) where op = op2f_b
instance Op (Char -> Char -> Bool) where op = op2c_b

op2i_i f = op' [isInt, isInt] 1 $ \[I x, I y] -> [I $ x `f` y]
op2f_f f = op' [isFloat, isFloat] 1 $ \[F x, F y] -> [F $ x `f` y]
opfi_f f = op' [isFloat, isInt] 1 $ \[F x, I y] -> [F $ x `f` y]
opf_f f = op' [isFloat] 1 $ \[F x] -> [F $ f x]
opf_i f = op' [isFloat] 1 $ \[F x] -> [I $ f x]
opi_f f = op' [isInt] 1 $ \[I x] -> [F $ f x]
op2i_b f = op' [isInt, isInt] 1 $ \[I x, I y] -> [W . show $ x `f` y]
op2f_b f = op' [isFloat, isFloat] 1 $ \[F x, F y] -> [W . show $ x `f` y]
op2c_b f = op' [isChar, isChar] 1 $ \[C x, C y] -> [W . show $ x `f` y]
op2i_2i f = op' [isInt, isInt] 2 $ \[I x, I y] -> let (u, w) = f x y in [I w, I u]

isType :: (Value -> Bool) -> Peg ()
isType f = do
  getArg $ anything ||. (== W "]") ||. isVar
  x <- popArg
  pushStack x
  if isVar x
    then do w@(W _) <- peekArg
            --v <- newVar
            v <- return (W "True") `mplus` return (W "False")
            addConstraintP ([v], [w, x])
            pushStack v
    else pushStack . W . show $ f x

-------------------- Helpers for builtins --------------------

anything (W "]") = False
anything (W "[") = False
anything _ = True

unpackR = do
  getArg $ isList ||. (== W "]") ||. isVar
  x <- popArg
  case x of
    W "]" -> return ()
    L l -> do pushStack $ W "["
              appendStack l
    V v -> do pushStack $ W "["
              appendStackVar v

appendStackVar v = addConstraintP ([V v], [L []]) `mplus` do
  x <- newVar
  y <- newVar
  addConstraintP ([x, y], [W "popr", V v])
  appendStack [x, W "$#", y]

-- A (A -> B) -> B
-- replaces stack with entirely new stack generated inductively on demand
callVar v = addConstraintP ([V v], [L []]) `mplus`  do
  x <- newVar
  y <- newVar
  addConstraintP ([x, y], [W "popr", V v])
  s <- getStack
  case gatherList 0 [] s of
    Left _ -> setStack [x, W "$#", y]
    Right (_, s) -> setStack $ x : W "$#" : y : W "[" : s

gatherList n l (w@(W "]") : s) = gatherList (n+1) (w:l) s
gatherList n l (w@(W "[") : s)
  | n <= 0 = Right (l,s)
  | otherwise = gatherList (n-1) (w:l) s
gatherList n l (w:s) = gatherList n (w:l) s
gatherList n l [] = Left l

wordMap = foldl' (flip (uncurry minsert)) M.empty

hasIo (L l) = any hasIo l
hasIo Io = True
hasIo _ = False

-------------------- Built-ins --------------------

builtins = wordMap [

  -- numeric
  ("add_int#", op2i_i (+)),
  ("sub_int#", op2i_i (-)),
  ("mul_int#", op2i_i (*)),
  ("div_int#", op2i_i div),
  ("mod_int#", op2i_i mod),
  ("divMod_int#", op2i_2i divMod),
  ("quot_int#", op2i_i quot),
  ("rem_int#", op2i_i rem),
  ("quotRem_int#", op2i_2i quotRem),
  ("pos_power_int#", op2i_i (^)),
  ("pos_power_float#", opfi_f (^)),
  ("int_power_float#", opfi_f (^^)),
  ("power_float#", op2f_f (**)),
  ("exp#", opf_f exp),
  ("sqrt#", opf_f sqrt),
  ("log#", opf_f log),
  ("logBase#", op2f_f logBase),
  ("sin#", opf_f sin),
  ("tan#", opf_f tan),
  ("cos#", opf_f cos),
  ("asin#", opf_f asin),
  ("atan#", opf_f atan),
  ("acos#", opf_f acos),
  ("sinh#", opf_f sinh),
  ("tanh#", opf_f tanh),
  ("cosh#", opf_f cosh),
  ("asinh#", opf_f asinh),
  ("atanh#", opf_f atanh),
  ("acosh#", opf_f acosh),
  ("add_float#", op2f_f (+)),
  ("sub_float#", op2f_f (-)),
  ("mul_float#", op2f_f (*)),
  ("divide_float#", op2f_f (/)),
  ("lt_int#", op2i_b (<)),
  ("lte_int#", op2i_b (<=)),
  ("gt_int#", op2i_b (>)),
  ("gte_int#", op2i_b (>=)),
  ("lt_float#", op2f_b (<)),
  ("lte_float#", op2f_b (<=)),
  ("gt_float#", op2f_b (>)),
  ("gte_float#", op2f_b (>=)),
  ("lt_char#", op2c_b (<)),
  ("lte_char#", op2c_b (<=)),
  ("gt_char#", op2c_b (>)),
  ("gte_char#", op2c_b (>=)),
  ("intToFloat#", opi_f realToFrac),
  ("round#", opf_i round),
  ("floor#", opf_i floor),
  ("ceiling#", opf_i ceiling),

  -- stack manipulation
  ("pop#", getArg anything >> popArg >> force),
  ("swap#", do getArg anything
               getArg anything
               x <- popArg
               y <- popArg
               pushStack y
               pushStack x),
  ("dup#", do getArg anything
              x <- popArg
              pushStack x
              pushStack x),
  ("dip#", do getArg $ isList ||. isVar
              getArg $ anything
              x <- popArg
              y <- popArg
              case y of
                L l -> appendStack l
                V v -> callVar v
                _ -> mzero
              pushStack x),
  ("unpackR#", unpackR),
  ("$#", do getArg $ isList ||. isVar
            x <- popArg
            w <- popArg -- temporarily remove $ from the arg stack
            case x of
              L l -> appendStack l >> force
              V v -> callVar v 
            pushArg w),

  -- control
  ("seq", do getArg anything
             force
             pushStack =<< popArg),
  ("!", do getArg $ (== W "True") ||. isVar
           x <- popArg
           when (isVar x) $ addConstraintP ([x], [W "True"])
           force),

  -- lists
  ("]", do s <- getStack
           case gatherList 0 [] s of
             Left s' -> pushStack (W "]")
             Right (l, s') -> setStack . (:s') . L . reverse $ l),
  ("null?", do unpackR
               pushArg $ W "]"
               getArg $ const True
               x <- popArg
               pushStack x
               popArg >>= pushStack
               pushStack . W . show $ x == W "["),

  -- checks
  ("int?", isType isInt),
  ("float?", isType isFloat),
  ("word?", isType $ isWord &&. (/= W "]")),
  ("list?", isType $ isList ||. (== W "]")),
  ("char?", isType isChar),
  ("io?", isType isIo),
  ("hasIO?", isType hasIo),
  ("eq?", withArgs [anything, anything] 1 $ \[x, y] -> do
            guard . not $ isList x && isList y
            pushStack . W . show $ x == y),

  -- read/show
  ("show#", do getArg anything
               x <- popArg
               pushStack . L . map C $ showStack [x]),
  ("read#", do getArg isString
               Just s <- toString <$> popArg
               let Right x = parseStack s
               appendStack x
               force),

  -- I/O
  ("getChar#", do getArg isIo
                  pushStack =<< popArg
                  liftIO getChar >>= pushStack . C),
  ("putChar#", do getArg isChar
                  getArg isIo
                  io <- popArg
                  C c <- popArg
                  liftIO $ putChar c
                  pushStack io),

  -- word definition
  (":def", do getArg isString
              getArg isList
              L l <- popArg
              Just s <- toString <$> popArg
              bind s l),
  (":undef", do getArg isString
                Just s <- toString <$> popArg
                unbind s)]

subst a b xs = map f xs
  where f x | x == a = b
            | otherwise = x

substConstr a b = (([a], [b]) :) . foldr propRules [] . map (\(l,r) -> (subst a b l, subst a b r))

addConstraintP = addConstraint propRules

propRules ([l], [r]) cs = substConstr l r cs
propRules ([W "True"], [W "eq?", v@(V _), x]) cs = substConstr v x cs
propRules ([W "True"], [W "eq?", x, v@(V _)]) cs = substConstr v x cs
propRules ([I x], [W "add_int#", v@(V _), I y]) cs = substConstr v (I $ x - y) cs
propRules ([I x], [W "sub_int#", v@(V _), I y]) cs = substConstr v (I $ x + y) cs
propRules c cs = c:cs

