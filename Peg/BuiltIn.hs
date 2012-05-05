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

module Peg.BuiltIn where

import Peg.Types
import Peg.Monad
import Peg.Parse

import Control.Applicative
import Control.Monad
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Data.List
import Data.Ord
import System.Console.Haskeline hiding (throwIO, handle)
import System.Environment
import System.FilePath
import System.IO
import Data.Either
import Control.Monad.Logic
import Control.Monad.State
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Control.Exception hiding (try)
import Data.Typeable

-------------------- Converters --------------------

op cI nO f = do
  mapM_ getArg $ reverse cI
  i <- replicateM (length cI) popArg
  if any isVar i
    then do w@(W _) <- peekArg
            vs <- replicateM nO newVar
            addConstraint [W "==", L vs, L (w:i)]
            appendStack vs
    else do appendStack $ f i

op2i_i f = op [isInt, isInt] 1 $ \[I x, I y] -> [I $ x `f` y]
op2f_f f = op [isFloat, isFloat] 1 $ \[F x, F y] -> [F $ x `f` y]
opfi_f f = op [isFloat, isInt] 1 $ \[F x, I y] -> [F $ x `f` y]
opf_f f = op [isFloat] 1 $ \[F x] -> [F $ f x]
opf_i f = op [isFloat] 1 $ \[F x] -> [I $ f x]
opi_f f = op [isInt] 1 $ \[I x] -> [F $ f x]
op2i_b f = op [isInt, isInt] 1 $ \[I x, I y] -> [W . show $ x `f` y]
op2f_b f = op [isFloat, isFloat] 1 $ \[F x, F y] -> [W . show $ x `f` y]
op2c_b f = op [isChar, isChar] 1 $ \[C x, C y] -> [W . show $ x `f` y]

isType :: (Value -> Bool) -> Peg ()
isType f = do
  getList $ anything ||. (== W "]")
  x <- popArg
  pushStack x
  if isVar x
    then do w@(W _) <- peekArg
            ((addConstraint [w, x] >> pushStack (W "True")) <|>
             (addConstraint [W "not", w, x] >> pushStack (W "False")))
    else pushStack . W . show $ f x

-------------------- Helpers for builtins --------------------

anything (W "]") = False
anything (W "[") = False
anything _ = True

unpackR = do
  getList $ isList ||. (== W "]")
  x <- popArg
  case x of
    W "]" -> return ()
    L l -> do pushStack $ W "["
              appendStack l
    V v -> do pushStack $ W "["
              appendStackVar v

appendStackVar v = addConstraint [W "null?", V v] <|> do
  x <- newVar
  y <- newVar
  addConstraint [W "==", L [x, y], L [W "popr", V v]]
  appendStack [x, W "$#", y]

bind nm l = modify $ \(PegState s a w n c) -> PegState s a (M.insertWith interleave nm (f l) w) n c
  where f l = do w <- popArg
                 appendStack l
                 force
                 pushArg w

unbind nm = modify $ \(PegState s a w n c) -> PegState s a (M.delete nm w) n c

gatherList n l (w@(W "]") : s) = gatherList (n+1) (w:l) s
gatherList n l (w@(W "[") : s)
  | n <= 0 = Right (l,s)
  | otherwise = gatherList (n-1) (w:l) s
gatherList n l (w:s) = gatherList n (w:l) s
gatherList n l [] = Left l

wordMap = foldl' (flip (uncurry $ M.insertWith mplus)) M.empty

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
  ("pop#", getList anything >> popArg >> force),
  ("swap#", do getList anything
               getList anything
               x <- popArg
               y <- popArg
               pushStack y
               pushStack x),
  ("dup#", do getList anything
              x <- popArg
              pushStack x
              pushStack x),
  ("dip#", do getList isList
              getList anything
              x <- popArg
              L l <- popArg
              appendStack $ x : l),
  ("unpackR#", unpackR),
  ("$#", do getList isList
            x <- popArg
            w <- popArg -- temporarily remove $ from the arg stack
            case x of
              L l -> appendStack l >> force
              V v -> appendStackVar v
            pushArg w),

  -- control
  ("seq", do getList anything
             force
             pushStack =<< popArg),
  ("!", do getArg anything -- $ (== W "True") ||. isVar
           x <- popArg
           case x of
             W "True" -> return ()
             W "False" -> mzero
             _ -> addConstraint . (x :) . psStack =<< get
           force),

  -- lists
  ("]", do PegState s a w n c <- get
           case gatherList 0 [] s of
             Left s' -> pushStack (W "]")
             Right (l, s') -> do
               put $ PegState s' a w n c
               pushStack . L . reverse $ l),
  ("null?", do unpackR
               pushArg $ W "]"
               getList $ const True
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
  ("eq?", do getList anything
             getList anything
             x <- popArg
             y <- popArg
             guard . not $ isList x && isList y
             pushStack . W . show $ x == y),

  -- read/show
  ("show#", do getList anything
               x <- popArg
               pushStack . L . map C $ showStack [x]),
  ("read#", do getList isString
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
  (":def", do getList isString
              getList isList
              L l <- popArg
              Just s <- toString <$> popArg
              bind s l),
  (":undef", do getList isString
                Just s <- toString <$> popArg
                unbind s)]
