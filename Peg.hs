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
{-# LANGUAGE CPP, PatternGuards, DeriveDataTypeable, ScopedTypeVariables #-}
#ifdef MAIN
module Main where
#else
module Peg where
#endif

import Control.Applicative
import Data.Maybe
import Debug.Trace
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

-------------------- Data Types --------------------

type Stack = [Value]
data PegState = PegState { psStack :: Stack,
                           psArgStack :: Stack,
                           psWords :: Map String (Peg ()),
                           psAvoid :: Set Stack }
type Peg = StateT PegState (LogicT IO)
data PegException = PegException Stack Stack deriving (Show, Typeable)
instance Exception PegException
data Rule = Rule { getRule :: Stack -> Peg Stack }
data Value = F Double | I Integer | C Char | L Stack | W String | Io deriving (Show, Eq, Ord)

isWord (W _) = True
isWord _ = False

isWordEq s (W s') = s == s'
isWordEq _ _ = False

isInt (I _) = True
isInt _ = False

isChar (C _) = True
isChar _ = False

isList (L _) = True
isList _  = False

isFloat (F _) = True
isFloat _ = False

toString :: Value -> Maybe String
toString (L l) = loop l
  where loop [] = return ""
        loop (C c : r) = (c:) <$> loop r
        loop _ = mzero
toString _ = mzero

isString = isJust . toString

isIo Io = True
isIo _ = False

-------------------- Debug --------------------

probe s x = trace (s ++ show x) x

traceStack :: Peg ()
traceStack = do
  s <- psStack <$> get
  trace (showStack s) $ return ()

-------------------- Peg Monad Operations --------------------

-- | pop an argument from the stack, push onto argument stack
getArg' check st = do
  force
  em <- emptyStack
  if em
    then done
    else do
      x <- popStack
      if check x
        then return ()
        else if st x
               then pushStack x >> done
               else mzero
      pushArg x

getArg ch = getArg' ch ((== W "[") ||. (== W "]"))
getArgNS ch = getArg' ch (== W "[")

pushStack x = modify (\(PegState s a m xx) -> PegState (x:s) a m xx)
appendStack x = modify (\(PegState s a m xx) -> PegState (x++s) a m xx)

popStack :: Peg Value
popStack = do PegState (x:s) a m xx <- get
              put $ PegState s a m xx
              return x
emptyStack = null . psStack <$> get

-- | can't go any further, bail
done = do
  st <- get
  liftIO . throwIO $ PegException (psStack st) (psArgStack st)

pushArg x = modify (\(PegState s a m xx) -> PegState s (x:a) m xx)
popArg = do PegState s (x:a) m xx <- get
            put $ PegState s a m xx
            return x

doWord w = do
  m <- psWords <$> get
  case w `M.lookup` m of
    Nothing -> pushStack (W w)
    Just f -> pushArg (W w) >> f >> popArg >> return ()

force = do
  st <- get
  case psStack st of
    (W w : _) -> popStack >> doWord w -- >> traceStack
    _ -> return ()

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

(f ||. g) x = f x || g x
(f &&. g) x = f x && g x

anything (W "]") = False
anything (W "[") = False
anything _ = True

unpackList = do
  getArg (isList ||. (== W "]"))
  x <- popArg
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
  ("swap", do getArg anything
              getArg anything
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
  ("pushr", do getArg anything
               getArg (isList ||. (== W "]"))
               x <- popArg
               case x of
                 -- toss it over the fence
                 W "]" -> do pushStack =<< popArg
                             pushStack (W "]")
                 L l -> do x <- popArg
                           pushStack $ L (x:l)),
  ("popr", do unpackList
              -- reach across the fence
              pushArg $ W "]"
              getArg (anything ||. (== W "["))
              x <- popArg
              guard $ x /= W "["
              popArg >>= pushStack
              pushStack x),
  ("dupnull?", do unpackList
                  -- take a peek across the fence
                  pushArg $ W "]"
                  getArg (anything ||. (== W "["))
                  x <- popArg
                  pushStack x
                  popArg >>= pushStack
                  pushStack . W . show $ x == W "["),
  (".", do getArg isList
           getArg (isList ||. (== W "]"))
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
  ("io?", is_type isIo),
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
  ("seq", do getArg anything
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
  ("realToFrac", do getArg (isInt ||. isFloat)
                    a <- popArg
                    case a of
                      I x -> pushStack . F . realToFrac $ x
                      F x -> pushStack a),
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

-------------------- Parsing --------------------

lexer = P.makeTokenParser haskellDef

integer = P.integer lexer
float = P.float lexer
naturalOrFloat = P.naturalOrFloat lexer
natural = P.natural lexer
whiteSpace = P.whiteSpace lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer

word :: Parser String
word = (:) <$> (letter <|> oneOf ":_?") <*> many (alphaNum <|> oneOf "?_'#")

symbol :: Parser String
symbol = many1 (oneOf "!@#$%^&*()_+=<>.~/\\|") <|>
        fmap (:[]) (oneOf "[]{};") <|>
        (string "-")

number = do m <- optionMaybe (char '-')
            let f = maybe (either I F)
                          (const $ either (I . negate) (F . negate)) m
            f <$> naturalOrFloat

value :: Parser Value
value = try number        <|>
        W <$> try symbol  <|>
        W <$> word        <|>
        C <$> charLiteral <|>
        L . map C <$> stringLiteral

comment = string "--" >> many (noneOf "\n")

stackExpr :: Parser Stack
stackExpr = concatMap f . reverse <$> (whiteSpace >> value `sepEndBy` whiteSpace <* optional comment)
  where f (W "{") = [W "[", W "["]
        f (W "}") = [W "]", W "]"]
        f (W ";") = [W "[", W "]"]
        f x = [x]

showStack :: Stack -> String
showStack s = drop 1 $ loop s []
  where loop [] = id
        loop (I x : s) = loop s . (' ':) . shows x
        loop (C x : s) = loop s . (' ':) . shows x
        loop (F x : s) = loop s . (' ':) . shows x
        loop (W x : s) = loop s . ((' ':x) ++)
        loop (Io : s) = loop s . (" IO" ++)
        loop (L [] : s) = loop s . (" [ ]" ++)
        loop (L x : s) = case toString (L x) of
                           Just str -> loop s . (' ':) . shows str
                           Nothing -> loop s . (" [" ++) . loop x . (" ]" ++)

parseStack = parse stackExpr ""

-------------------- Main --------------------

evalStack (s, m) = observeManyT 8 $ do
  PegState s _ m _ <- execStateT force $ PegState s [] m S.empty
  return (s, m)

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
  runInputT defaultSettings $ evalLoop (Right []) m

load :: Stack
  -> Map String (Peg ())
  -> [String]
  -> IO (Map String (Peg ()))
load s m [] = return m
load s m (input:r) =
  case parseStack input of
    Left e -> print e >> return m
    Right s -> handle (\(_ :: PegException) -> load s m r) $ do
      (s', m') : _ <- evalStack (s, m)
      load s' m' r

-- I/O ideas
-- I/O token: dup I/O --> spawn thread, pop I/O --> kill thread
-- x0 x1 x2      IO x3 x4  IO x5
-- | main thread | thread 0 | thread 1 ...
-- .. IO [ x ] dip
--   <------|
-- send to thread n-1

makeIOReal = map f
  where f (W "IO") = Io
        f x = x

evalLoop :: Either (Stack, Stack) Stack -> Map String (Peg ()) -> InputT IO ()
evalLoop p m = do
  let text = case p of
               Left (s, a) -> (showStack s, ' ' : showStack (reverse a))
               Right [] -> ("", "")
               Right s -> (showStack s ++ " ", "")
  minput <- getInputLineWithInitial ": " text
  case minput of
    Nothing -> return ()
    Just "" -> return ()
    Just input -> case parseStack input of
      Left e -> outputStrLn (show e) >> evalLoop p m
      Right s -> do
        x' <- liftIO . handle (\(PegException s a) -> return (Left (s, a))) $ Right <$> evalStack (makeIOReal s, m)
        case x' of
          Left s' -> evalLoop (Left s') m
          Right [] -> evalLoop (Right [W "no"]) m
          Right ((s',m'):r) -> do
            mapM_ (outputStrLn . showStack . fst) r
            evalLoop (Right s') m'
