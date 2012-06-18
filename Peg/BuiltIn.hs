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
import Peg.Utils

import Control.Applicative
import Data.List
--import Control.Monad.Logic
import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow (second)

import Debug.Trace

-------------------- Converters --------------------

withArgs cI nO f = do
  mapM_ (getArg . (isVar ||.)) $ reverse cI
  i <- replicateM (length cI) popArg
  if any isVar i
    then do w@(W _) <- peekArg
            vs <- replicateM nO newVar
            appendStack vs
            addConstraint (vs, w : reverse i)
    else f i

op' cI nO f = withArgs cI nO (appendStack . f)

op_ nI nO = do
  replicateM_ nI $ getArg anything
  i <- replicateM nI popArg
  w@(W _) <- peekArg
  vs <- replicateM nO newVar
  appendStack vs
  addConstraint (vs, w : reverse i)

op_t = do
  getArg anything
  x <- popArg
  w@(W _) <- peekArg
  v <- newVar
  pushStack x
  pushStack v
  addConstraint ([v, x], [w, x])

op2i_i f = op' [isInt, isInt] 1 $ \[I x, I y] -> [I $ x `f` y]
op2f_f f = op' [isFloat, isFloat] 1 $ \[F x, F y] -> [F $ x `f` y]
opfi_f f = op' [isFloat, isInt] 1 $ \[F x, I y] -> [F $ x `f` y]
opf_f f = op' [isFloat] 1 $ \[F x] -> [F $ f x]
opf_i f = op' [isFloat] 1 $ \[F x] -> [I $ f x]
opi_f f = op' [isInt] 1 $ \[I x] -> [F $ f x]
op2i_b f = op' [isInt, isInt] 1 $ \[I x, I y] -> [A . show $ x `f` y]
op2f_b f = op' [isFloat, isFloat] 1 $ \[F x, F y] -> [A . show $ x `f` y]
op2c_b f = op' [isChar, isChar] 1 $ \[C x, C y] -> [A . show $ x `f` y]
op2i_2i f = op' [isInt, isInt] 2 $ \[I x, I y] -> let (u, w) = f x y in [I w, I u]

isType :: (Value -> Bool) -> Peg ()
isType f = do
  getArg $ anything ||. (== W "]") ||. isVar
  x <- popArg
  pushStack x
  if isVar x
    then do w@(W _) <- peekArg
            --v <- newVar
            v <- return (A "True") `mplus` return (A "False")
            pushStack v
            addConstraint ([v], [w, x])
    else pushStack . A . show $ f x

-------------------- Helpers for builtins --------------------

anything (W "]") = False
anything (A "[") = False
anything _ = True

unpackR = do
  getArg $ isList ||. (== W "]") ||. isVar
  x <- popArg
  case x of
    W "]" -> return ()
    L l -> do pushStack $ A "["
              appendStack l
    V v -> do pushStack $ A "["
              appendStackVar v

-- convert to constraint when accessing variable stack?

appendStackVar v = do
  sv <- newSVar
  pushStack sv
  addConstraint ([L [sv]], [V v])

-- A (A -> B) -> B
-- replaces stack with entirely new stack generated inductively on demand
callVar v = do
  sv <- newSVar
  st <- getStack
  case gatherList 0 [] st of
    Left _ -> setStack [sv]
    Right (_, s) -> setStack $ sv : A "[" : s
  addConstraint ([L [sv]], [V v])

wordMap = foldl' (flip (uncurry minsert)) M.empty

-------------------- Built-ins --------------------

-- need to parse into form [Word0 out0 in0 in1, Word1 out1 out2 in2, ...]
-- infinite DAG

builtins = wordMap [

  -- numeric
  ("add_int#", op_ 2 1),
  ("sub_int#", op_ 2 1),
  ("mul_int#", op_ 2 1),
  ("div_int#", op_ 2 1),
  ("mod_int#", op_ 2 1),
  ("divMod_int#", op_ 2 2),
  ("quot_int#", op_ 2 1),
  ("rem_int#", op_ 2 1),
  ("quotRem_int#", op_ 2 2),
  ("pos_power_int#", op_ 2 1),
  ("pos_power_float#", op_ 2 1),
  ("int_power_float#", op_ 2 1),
  ("power_float#", op_ 2 1),
  ("exp#", op_ 1 1),
  ("sqrt#", op_ 1 1),
  ("log#", op_ 1 1),
  ("logBase#", op_ 2 1),
  ("sin#", op_ 1 1),
  ("tan#", op_ 1 1),
  ("cos#", op_ 1 1),
  ("asin#", op_ 1 1),
  ("atan#", op_ 1 1),
  ("acos#", op_ 1 1),
  ("sinh#", op_ 1 1),
  ("tanh#", op_ 1 1),
  ("cosh#", op_ 1 1),
  ("asinh#", op_ 1 1),
  ("atanh#", op_ 1 1),
  ("acosh#", op_ 1 1),
  ("add_float#", op_ 2 1),
  ("sub_float#", op_ 2 1),
  ("mul_float#", op_ 2 1),
  ("divide_float#", op_ 2 1),
  ("lt_int#", op_ 2 1),
  ("lte_int#", op_ 2 1),
  ("gt_int#", op_ 2 1),
  ("gte_int#", op_ 2 1),
  ("lt_float#", op_ 2 1),
  ("lte_float#", op_ 2 1),
  ("gt_float#", op_ 2 1),
  ("gte_float#", op_ 2 1),
  ("lt_char#", op_ 2 1),
  ("lte_char#", op_ 2 1),
  ("gt_char#", op_ 2 1),
  ("gte_char#", op_ 2 1),
  ("intToFloat#", op_ 1 1),
  ("round#", op_ 1 1),
  ("floor#", op_ 1 1),
  ("ceiling#", op_ 1 1),

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
  ("<$#", do getArg anything
             x <- popArg
             pushStack $ W "$#"
             pushStack x),
  ("unpackR#", unpackR),
  ("$#", do getArg $ isList ||. isVar
            x <- popArg
            w <- popArg -- temporarily remove $ from the arg stack
            case x of
              L l -> appendStack l
              V v -> callVar v
              _ -> mzero
            force
            pushArg w),

  -- control
  ("seq", do getArg anything
             force
             pushStack =<< popArg),
  ("!", do getArg $ (== A "True") ||. isVar
           x <- popArg
           when (isVar x) $ addConstraint ([A "True"], [x])
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
               pushStack . A . show $ x == A "["),

  -- checks
  ("int?", op_t),
  ("float?", op_t),
  --("word?", isType $ isWord &&. (/= W "]")),
  ("word?", op_t),
  --("list?", isType $ isList ||. (== W "]")),
  ("list?", op_t),
  ("char?", op_t),
  ("io?", op_t),
  ("hasIO?", op_t),
  ("eq?", op_ 2 1),
{-
  ("eq?", withArgs [anything, anything] 1 $ \[x, y] -> do
            guard . not $ isList x && isList y
            pushStack . A . show $ x == y),
-}
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
                  --liftIO getChar >>=
                  v <- newVar
                  pushStack v
                  addConstraint ([v, Io], [W "getChar#", Io])),
  ("putChar#", do getArg isChar
                  getArg isIo
                  io <- popArg
                  C c <- popArg
                  --liftIO $ putChar c
                  pushStack io
                  addConstraint ([Io], [W "putChar#", C c, Io])),

  -- word definition
  (":def", do getArg isString
              getArg isList
              L l <- popArg
              Just s <- toString <$> popArg
              bind s l),
  (":undef", do getArg isString
                Just s <- toString <$> popArg
                unbind s)]
