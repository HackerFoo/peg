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
{-# LANGUAGE CPP, ScopedTypeVariables #-}
#ifdef MAIN
module Main where
#else
module Peg where
#endif

import Peg.Types
import Peg.BuiltIn
import Peg.Monad
import Peg.Parse

import Control.Applicative
import System.Console.Haskeline hiding (throwIO, handle)
import System.Environment
import System.FilePath
import System.IO
import Control.Monad.Logic
import Control.Monad.State
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import Data.List

import Debug.Trace
import System.IO.Unsafe

-- crude iterative deepening

depthInc :: Int
depthInc = 50

evalStack st = loop 0
  where loop d = do resetMore
                    r <- evalStackD st d
                    m <- isThereMore
                    if m
                      then do r' <- unsafeInterleaveIO $ loop (d+depthInc)
                              return $ r ++ r'
                      else return r
                    
evalStackD (s, m, c) d = observeAllT $ do
  PegState s _ m d' _ c <- execStateT force $ PegState s [] m d 0 c
  guard $ d' < depthInc
  return (s, m, c)

hGetLines h = do
  e <- hIsEOF h
  if e
    then return []
    else (:) <$> hGetLine h <*> hGetLines h

getLinesFromFile f = withFile f ReadMode hGetLines

main = do
  args <- getArgs
  let files = filter ((==".peg").takeExtension) args
  m <- foldM (\m f -> do
          l <- getLinesFromFile f
          load [] m l) builtins files
  runInputT defaultSettings $ evalLoop [] m

load :: Stack
  -> Env
  -> [String]
  -> IO Env
load s m [] = return m
load s m (input:r) =
  case parseStack input of
    Left e -> print e >> return m
    Right s -> do
      x <- evalStack (s, m, [])
      case x of
        (s', m', _) : _ -> load s' m' r
        [] -> load s m r

makeIOReal = map (\x -> if x == W "IO" then Io else x)

evalLoop :: Stack -> Env -> InputT IO ()
evalLoop p m = do
    let text = case p of
                 [] -> ""
                 s -> showStack s ++ " "
    minput <- getInputLineWithInitial ": " (text, "")
    case minput of
      Nothing -> return ()
      Just "" -> return ()
      Just input -> case parseStack input of
        Left e -> outputStrLn (show e) >> evalLoop p m
        Right s -> do
          x' <- liftIO $ evalStack (makeIOReal s, m, [])
          case take 5 (nubBy (\(a,_,_) (b,_,_) -> a == b) x') of
            [] -> evalLoop s m
            ((s',m',c'):r) -> do
              printConstraints c'
              mapM_ printAlt r
              evalLoop s' m'
  where printConstraints c = return ()
          --mapM_ (outputStrLn . ("= "++) . showStack) $ reverse c
        printAlt (s,_,c) = do
          outputStrLn . ("| "++) . showStack $ s
          printConstraints c
