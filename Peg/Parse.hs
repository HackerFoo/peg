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

module Peg.Parse where

import Peg.Types

import Control.Applicative
import Debug.Trace
import Text.Parsec hiding ((<|>), many, optional)
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (haskellDef)
import Control.Monad.State

lexer = P.makeTokenParser haskellDef

integer = P.integer lexer
float = P.float lexer
naturalOrFloat = P.naturalOrFloat lexer
natural = P.natural lexer
whiteSpace = P.whiteSpace lexer
charLiteral = P.charLiteral lexer
stringLiteral = P.stringLiteral lexer

word :: Parser String
word = (:) <$> (letter <|> oneOf ":_") <*> many (alphaNum <|> oneOf "?_'#")

var :: Parser String
var = char '?' *> many1 alphaNum

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
        V <$> var         <|>
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
        loop (V x : s) = loop s . ((' ':'?':x) ++)
        loop (Io : s) = loop s . (" IO" ++)
        loop (L [] : s) = loop s . (" [ ]" ++)
        loop (L x : s) = case toString (L x) of
                           Just str -> loop s . (' ':) . shows str
                           Nothing -> loop s . (" [" ++) . loop x . (" ]" ++)

parseStack = parse stackExpr ""

-------------------- Debug --------------------

probe s x = trace (s ++ show x) x

traceStack :: Peg ()
traceStack = do
  s <- psStack <$> get
  trace (showStack s) $ return ()
