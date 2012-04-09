{-# LANGUAGE CPP, PatternGuards #-}
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
import System.Console.Haskeline
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

lexer = P.makeTokenParser haskellDef

integer = P.integer lexer
float = P.float lexer
naturalOrFloat = P.naturalOrFloat lexer
natural = P.natural lexer
whiteSpace = P.whiteSpace lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer

probe s x = trace (s ++ show x) x

type Stack = [Value]
data PegState = PegState { psStack :: Stack,
                           psArgStack :: Stack,
                           psWords :: Map String (Peg ()),
                           psAvoid :: Set Stack }
type Peg = StateT PegState (LogicT (Either Stack))
data Rule = Rule { getRule :: Stack -> Peg Stack }
data Value = F Double | I Integer | C Char | L Stack | W String deriving (Show, Eq, Ord)

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

up :: Stack -> Peg ()
up = lift . lift . Left

-- | pop an argument from the stack, push onto argument stack
getArg check = do
  force
  em <- emptyStack
  if em
    then done
    else do
      x <- popStack
      if check x
        then return ()
        else if x == W "["
               then pushArg x >> done
               else mzero
      pushArg x

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
  up $ reverse (psArgStack st) ++ psStack st

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

traceStack :: Peg ()
traceStack = do
  s <- psStack <$> get
  trace (showStack s) $ return ()

wordMap = foldl' (flip (uncurry $ M.insertWith mplus)) M.empty

op2i f = do
  getArg isInt
  getArg isInt
  I x <- popArg
  I y <- popArg
  pushStack $ I (x `f` y)

op2f f = do
  getArg isFloat
  getArg isFloat
  F x <- popArg
  F y <- popArg
  pushStack $ F (x `f` y)

op1f f = do
  getArg isFloat
  F x <- popArg
  pushStack . F . f $ x

reli f = do
  getArg isInt
  getArg isInt
  I x <- popArg
  I y <- popArg
  pushStack . W . show $ x `f` y

relf f = do
  getArg isFloat
  getArg isFloat
  F x <- popArg
  F y <- popArg
  pushStack . W . show $ x `f` y

is_type :: (Value -> Bool) -> Peg ()
is_type f = do
  getArg anything
  pushStack . W . show . f =<< popArg

anything (W "]") = False
anything (W "[") = False
anything _ = True

brac (W "]") = True
brac _ = False

(f ||. g) x = f x || g x
(f &&. g) x = f x && g x

builtins = wordMap [
  ("+", op2i (+)),
  ("-", op2i (-)),
  ("*", op2i (*)),
  ("div", do getArg (isInt &&. (/= (I 0)))
             getArg isInt
             I x <- popArg
             I y <- popArg
             pushStack . I $ x `div` y),
  ("^", do getArg (isInt &&. (\(I x) -> x >= 0))
           getArg isInt
           I x <- popArg
           I y <- popArg
           pushStack . I $ x ^ y),
  ("^^", do getArg isInt
            getArg isFloat
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
               getArg (isList ||. brac)
               x <- popArg
               case x of
                 -- toss it over the fence
                 W "]" -> do pushStack =<< popArg
                             pushStack (W "]")
                 L l -> do x <- popArg
                           pushStack $ L (x:l)),
  ("popr", do getArg (isList ||. brac)
              x <- popArg
              case x of
                -- reach across the fence
                W "]" -> do getArg (anything ||. (== W "["))
                            x <- popArg
                            guard $ x /= W "["
                            pushStack (W "]")
                            pushStack x
                -- unpack the list and force it
                L l -> do pushStack $ W "["
                          pushArg $ W "]"
                          appendStack l
                          getArg (anything ||. (== W "["))
                          x <- popArg
                          guard $ x /= W "["
                          popArg >>= pushStack
                          pushStack x
                _ -> mzero),
  (".", do getArg isList
           getArg (isList ||. brac)
           x <- popArg
           case x of
             -- remove the fence
             W "]" -> do L l <- popArg
                         pushArg $ W "]"
                         appendStack l
                         popArg >>= pushStack
             L x -> do L y <- popArg
                       pushStack . L $ y ++ x),
  ("dupnull?", do getArg (isList ||. brac)
                  x <- popArg
                  case x of
                    -- take a peek across the fence
                    W "]" -> do pushArg $ W "]"
                                force
                                y <- peekStack
                                popArg >>= pushStack
                                pushStack . W . show $ y == W "["
                    L l -> do pushStack $ W "["
                              appendStack l
                              force
                              x <- peekStack
                              pushStack $ W "]"
                              pushStack . W . show $ x == W "["),
  ("assert", getArg (== W "True") >> popArg >> force),
  ("deny", getArg (== W "False") >> popArg >> force),
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
           appendStack l
           force),
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
              force)]
{-
runIO (L [W "IO", L (W op : args), L k] : s) =
  case (op, args) of
    ("getChar", []) -> getChar >>= runIO . (++s) . (:k) . C
    ("putChar", [C c]) -> putChar c >> runIO (k ++ s)
    ("return", [x]) -> runIO (x : k ++ s)
-}
bind n l = modify $ \(PegState s a w xx) -> PegState s a (M.insertWith interleave n (f l) w) xx
  where f l = do force
                 w <- popArg
                 force >> appendStack l >> force
                 pushArg w

unbind n = modify $ \(PegState s a w xx) -> PegState s a (M.delete n w) xx

peekStack = do
  (x:_) <- psStack <$> get
  return x

gatherList n l (w@(W "]") : s) = gatherList (n+1) (w:l) s
gatherList n l (w@(W "[") : s)
  | n <= 0 = Right (l,s)
  | otherwise = gatherList (n-1) (w:l) s
gatherList n l (w:s) = gatherList n (w:l) s
gatherList n l [] = Left l


word :: Parser String
word = (:) <$> (letter <|> oneOf ":_?") <*> many (alphaNum <|> oneOf "?_'#")

symbol :: Parser String
symbol = many1 (oneOf "!@#$%^&*()_+=<>.~/\\|") <|>
        fmap (:[]) (oneOf "[]{};") <|>
        (string "-")

--list = char '[' >> stackExpr <* char ']'

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
        loop (L [] : s) = loop s . (" [ ]" ++)
        loop (L x : s) = case toString (L x) of
                           Just str -> loop s . (' ':) . shows str
                           Nothing -> loop s . (" [" ++) . loop x . (" ]" ++)

parseStack = parse stackExpr ""

evalStack' fs m src = do
  s <- fs <$> parseStack src
  return . observeManyT 8 $ do
    PegState s _ m _ <- execStateT force $ PegState s [] m S.empty
    return (s, m)

evalStack fs m = fmap (either (\s -> [(s, m)]) id) . evalStack' fs m

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
          case load [] m l of
            Left e -> print e >> return m
            Right m' -> return m') builtins files
  runInputT defaultSettings (evalLoop True [] m)

load :: Stack
  -> Map String (Peg ())
  -> [String]
  -> Either ParseError (Map String (Peg ()))
load s m [] = Right m
load s m (input:r) =
  case head <$> evalStack (++s) m input of
    Left e -> Left e
    Right (s', m') -> load s' m' r

ifNotNull f [] = []
ifNotNull f x = f x

evalLoop :: Bool -> Stack -> Map String (Peg ()) -> InputT IO ()
evalLoop n s m = do
  minput <- getInputLineWithInitial ": " .
              (if n then (flip (,) "" . ifNotNull (++" "))
                    else ((,) "") . (" "++)) $ showStack s
  case minput of
    Nothing -> return ()
    Just "" -> return ()
    Just input -> case evalStack' id m input of
      Left e -> outputStrLn (show e) >> evalLoop n s m
      Right x -> case x of
        Left s' -> evalLoop False s' m
        Right [] -> outputStrLn "No" >> evalLoop n s m
        Right ((s',m'):r) -> do
          mapM_ (outputStrLn . showStack . fst) r
          evalLoop True s' m'
