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

op2i f = do
  getArgNS isInt
  getArgNS isInt
  I x <- popArg
  I y <- popArg
  pushStack $ I (x `f` y)

op2f f = do
  getArgNS isFloat
  getArgNS isFloat
  F x <- popArg
  F y <- popArg
  pushStack $ F (x `f` y)

op1f f = do
  getArgNS isFloat
  F x <- popArg
  pushStack . F . f $ x

opfi f = do
  getArg isFloat
  F x <- popArg
  pushStack . I . f $ x

reli f = do
  getArgNS isInt
  getArgNS isInt
  I x <- popArg
  I y <- popArg
  pushStack . W . show $ x `f` y

relf f = do
  getArgNS isFloat
  getArgNS isFloat
  F x <- popArg
  F y <- popArg
  pushStack . W . show $ x `f` y

relc f = do
  getArgNS isChar
  getArgNS isChar
  C x <- popArg
  C y <- popArg
  pushStack . W . show $ x `f` y

is_type :: (Value -> Bool) -> Peg ()
is_type f = do
  getArg anything
  pushStack . W . show . f =<< popArg

-------------------- Helpers for builtins --------------------

anythingIo (W "]") = False
anythingIo (W "[") = False
anythingIo _ = True

anything = anythingIo &&. (not . hasIo)

unpackList = do
  getArg (isList ||. (== W "]"))
  x <- popArg
  pushArg $ W "]"
  case x of
    W "]" -> return ()
    L l -> do pushStack $ W "["
              appendStack l

bind n l = modify $ \(PegState s a w xx) -> PegState s a (M.insertWith interleave n (f l) w) xx
  where f l = do force
                 w <- popArg
                 force >> appendStack l >> force
                 pushArg w

unbind n = modify $ \(PegState s a w xx) -> PegState s a (M.delete n w) xx

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
  ("+", op2i (+)),
  ("-", op2i (-)),
  ("*", op2i (*)),
  ("div", do getArgNS (isInt &&. (/= (I 0)))
             getArgNS isInt
             I x <- popArg
             I y <- popArg
             pushStack . I $ x `div` y),
  ("^", do getArgNS (isInt &&. (\(I x) -> x >= 0))
           getArgNS isInt
           I x <- popArg
           I y <- popArg
           pushStack . I $ x ^ y),
  ("^", do getArgNS (isInt &&. (\(I x) -> x >= 0))
           getArgNS isFloat
           F x <- popArg
           I y <- popArg
           pushStack . F $ x ^ y),
  ("^^", do getArgNS isInt
            getArgNS isFloat
            F x <- popArg
            I y <- popArg
            pushStack . F $ x ^^ y),
  ("**", op2f (**)),
  ("exp", op1f exp),
  ("sqrt", op1f sqrt),
  ("log", op1f log),
  ("logBase", op2f logBase),
  ("sin", op1f sin),
  ("tan", op1f tan),
  ("cos", op1f cos),
  ("asin", op1f asin),
  ("atan", op1f atan),
  ("acos", op1f acos),
  ("sinh", op1f sinh),
  ("tanh", op1f tanh),
  ("cosh", op1f cosh),
  ("asinh", op1f asinh),
  ("atanh", op1f atanh),
  ("acosh", op1f acosh),
  ("+", op2f (+)),
  ("-", op2f (-)),
  ("*", op2f (*)),
  ("/", op2f (/)),
  ("<", reli (<)),
  ("<=", reli (<=)),
  (">", reli (>)),
  (">=", reli (>=)),
  ("<", relf (<)),
  ("<=", relf (<=)),
  (">", relf (>)),
  (">=", relf (>=)),
  ("<", relc (<)),
  ("<=", relc (<=)),
  (">", relc (>)),
  (">=", relc (>=)),
  ("pop", getArg anything >> popArg >> force),
  ("swap", do getArg anythingIo
              getArg anythingIo
              x <- popArg
              y <- popArg
              pushStack y
              pushStack x),
  ("dup", do getArg anything
             x <- popArg
             pushStack x
             pushStack x),
  ("]", do PegState s a w xx <- get
           case gatherList 0 [] s of
             Left s' -> pushStack (W "]")
             Right (l, s') -> do
               put $ PegState s' a w xx
               pushStack . L . reverse $ l),
  ("pushr", do getArg anythingIo
               getArg $ isList ||. (== W "]")
               x <- popArg
               case x of
                 -- toss it over the fence
                 W "]" -> do pushStack =<< popArg
                             pushStack (W "]")
                 L l -> do x <- popArg
                           pushStack $ L (x:l)),
  ("popr", do unpackList
              -- reach across the fence
              getArg $ anythingIo ||. (== W "[")
              x <- popArg
              guard $ x /= W "["
              popArg >>= pushStack
              pushStack x),
  ("dupnull?", do unpackList
                  -- take a peek across the fence
                  getArg $ anythingIo ||. (== W "[") ||. isIo
                  x <- popArg
                  pushStack x
                  popArg >>= pushStack
                  pushStack . W . show $ x == W "["),
  (".", do getArg isList
           getArg $ isList ||. (== W "]")
           x <- popArg
           case x of
             -- remove the fence
             W "]" -> do L l <- popArg
                         appendStack l
                         pushStack $ W "]"
             L x -> do L y <- popArg
                       pushStack . L $ y ++ x),
  ("assert", getArgNS (== W "True") >> popArg >> force),
  ("deny", getArgNS (== W "False") >> popArg >> force),
  ("\\/", do getArg anything
             getArg anything
             x <- popArg
             y <- popArg
             pushStack x `interleave` pushStack y),
  ("int?", is_type isInt),
  ("float?", is_type isFloat),
  ("word?", is_type isWord),
  ("list?", is_type isList),
  ("char?", is_type isChar),
  ("string?", is_type isString),
  ("eq?", do getArg anything
             getArg anything
             x <- popArg
             y <- popArg
             guard . not $ isList x && isList y
             pushStack . W . show $ x == y),
  (":def", do getArg isString
              getArg isList
              L l <- popArg
              Just s <- toString <$> popArg
              bind s l),
  (":undef", do getArg isString
                Just s <- toString <$> popArg
                unbind s),
  ("$", do getArg isList
           L l <- popArg
           w <- popArg -- temporarily remove $ from the arg stack
           appendStack l
           force
           pushArg w),
  ("seq", do getArg anythingIo
             force
             pushStack =<< popArg),
  ("show", do getArg anything
              x <- popArg
              pushStack . L . map C $ showStack [x]),
  ("read", do getArg isString
              Just s <- toString <$> popArg
              let Right x = parseStack s
              appendStack x
              force),
  ("realToFrac", do getArg isInt
                    I x <- popArg
                    pushStack . F . realToFrac $ x),
  ("realToFrac", do getArg isFloat
                    pushStack =<< popArg),
  ("round", opfi round),
  ("floor", opfi floor),
  ("ceiling", opfi ceiling),
  ("getChar", do getArg isIo
                 pushStack =<< popArg
                 liftIO getChar >>= pushStack . C),
  ("putChar", do getArg isChar
                 getArg isIo
                 io <- popArg
                 C c <- popArg
                 liftIO $ putChar c
                 pushStack io),
  ("getLine", do getArg isIo
                 pushStack =<< popArg
                 liftIO getLine >>= pushStack . L . map C),
  ("putStr", do getArg isString
                getArg isIo
                io <- popArg
                Just s <- toString <$> popArg
                liftIO $ putStr s
                pushStack io),
  ("putStrLn", do getArg isString
                  getArg isIo
                  io <- popArg
                  Just s <- toString <$> popArg
                  liftIO $ putStrLn s
                  pushStack io)]

