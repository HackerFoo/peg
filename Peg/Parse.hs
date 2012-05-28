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

word :: Parser Value
word = W <$> ((:) <$> (lower <|> char ':') <*> many (alphaNum <|> oneOf "?_'#"))

atom :: Parser Value
atom = A <$> (((:) <$> upper <*> many (alphaNum <|> oneOf "?_'#")) <|>
              ((:[]) <$>  oneOf "[_"))

var :: Parser Value
var = V <$> (char '?' *> ((:) <$> lower <*> many (lower <|> digit <|> char '_')))

svar :: Parser Value
svar = S <$> (char '?' *> ((:) <$> upper <*> many (upper <|> digit <|> char '_')))

symbol :: Parser Value
symbol = W <$> (many1 (oneOf "!@#$%^&*()-+=<>.~/?\\|") <|>
                fmap (:[]) (oneOf "]{};"))

quote :: Parser Value
quote = L . (:[]) <$> (char '`' *> value)

number :: Parser Value
number = do m <- optionMaybe (char '-')
            let f = maybe (either I F)
                          (const $ either (I . negate) (F . negate)) m
            f <$> naturalOrFloat

value :: Parser Value
value = try number        <|>
        try var           <|>
        try svar          <|>
        try symbol        <|>
        word              <|>
        atom              <|>
        C <$> charLiteral <|>
        L . map C <$> stringLiteral <|>
        quote

comment = string "--" >> many (noneOf "\n")

stackExpr :: Parser Stack
stackExpr = concatMap f . reverse <$> (whiteSpace >> value `sepEndBy` whiteSpace <* optional comment)
  where f (W "{") = [A "[", A "["]
        f (W "}") = [W "]", W "]"]
        f (W ";") = [A "[", W "]"]
        f x = [x]

showStack :: Stack -> String
showStack s = drop 1 $ loop s []
  where loop [] = id
        loop (I x : s) = loop s . (' ':) . shows x
        loop (C x : s) = loop s . (' ':) . shows x
        loop (F x : s) = loop s . (' ':) . shows x
        loop (W x : s) = loop s . ((' ':x) ++)
        loop (A x : s) = loop s . ((' ':x) ++)
        loop (V x : s) = loop s . ((' ':'?':x) ++)
        loop (S x : s) = loop s . ((' ':'?':x) ++)
        loop (Io : s) = loop s . (" IO" ++)
        loop (L [] : s) = loop s . (" [ ]" ++)
        loop (L x : s) = case toString (L x) of
                           Just str -> loop s . (' ':) . shows str
                           Nothing -> case toQuote (L x) of
                             Just str -> loop s . (' ':) . (str ++)
                             Nothing -> loop s . (" [" ++) . loop x . (" ]" ++)

toQuote (L [x]) | isList x = ('`':) <$> toQuote x
                | otherwise = Just ('`' : showStack [x])
toQuote _ = Nothing

parseStack = parse stackExpr ""

-------------------- Debug --------------------

probe s x = trace (s ++ show x) x

traceStack :: Peg ()
traceStack = do
  s <- psStack <$> get
  when (not $ null s) . trace (showStack s) $ return ()
