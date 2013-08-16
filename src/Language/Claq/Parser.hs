{-
 - Claq (c) 2013 NEC Laboratories America, Inc.  All rights reserved.
 -
 - This file is part of Claq.
 - Claq is distributed under the 3-clause BSD license.
 - See the LICENSE file for more details.
 -}

{-# LANGUAGE FlexibleContexts #-}

module Language.Claq.Parser (circuit) where

import Control.Monad.Free
import Data.Functor ((<$>))
import Data.Maybe (catMaybes)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (GenLanguageDef, emptyDef)
import Text.Parsec.Token (GenTokenParser)
import qualified Text.Parsec.Token as P

import Data.ClassicalCircuit
import Language.Claq.Syntax

circuit :: Stream s m Char => ParsecT s u m [Stmt String]
circuit = do
    whiteSpace
    stmts <- catMaybes <$> semiSep1 (optionMaybe stmt)
    eof
    return stmts

stmt :: Stream s m Char => ParsecT s u m (Stmt String)
stmt =
    SInputs <$> inputsSpec
    <|>
    SOutputs <$> outputsSpec
    <|>
    do { v <- identifier
       ; _ <- symbol "="
       ; e <- expr
       ; return $ SEquation v e
       }
    <?> "statement"

inputsSpec :: Stream s m Char => ParsecT s u m [String]
inputsSpec =
    do { reserved ".inputs"
       ; commaSep identifier
       }

outputsSpec :: Stream s m Char => ParsecT s u m [Free ClaGate String]
outputsSpec =
    do { reserved ".outputs"
       ; commaSep expr
       }

expr :: Stream s m Char => ParsecT s u m (Free ClaGate String)
expr = buildExpressionParser opTable term
    <?> "expression"
  where
    opTable =
        [ [prefix "~" (Free . GNot)]
        , [binary "&" (\e1 e2 -> Free $ GAnd e1 e2) AssocLeft]
        , [binary "^" (\e1 e2 -> Free $ GXor e1 e2) AssocLeft]
        , [binary "|" (\e1 e2 -> Free $ GOr e1 e2) AssocLeft]
        ]
    binary  name fun = Infix (do { reservedOp name; return fun })
    prefix  name fun = Prefix (do { reservedOp name; return fun })
    postfix name fun = Postfix (do { reservedOp name; return fun })

term :: Stream s m Char => ParsecT s u m (Free ClaGate String)
term =
    parens expr
    <|>
    do { v <- identifier
       ; return $ Pure v
       }
    <|>
    do { n <- natural
       ; case n of
           0 -> return $ Free $ GConst False
           1 -> return $ Free $ GConst True
           _ -> parserZero
       }
    <?> "simple expression"

identifier :: Stream s m Char => ParsecT s u m String
identifier = P.identifier lexer
reserved :: Stream s m Char => String -> ParsecT s u m ()
reserved = P.reserved lexer
reservedOp :: Stream s m Char => String -> ParsecT s u m ()
reservedOp = P.reservedOp lexer
natural :: Stream s m Char => ParsecT s u m Integer
natural = P.natural lexer
symbol :: Stream s m Char => String -> ParsecT s u m String
symbol = P.symbol lexer
whiteSpace :: Stream s m Char => ParsecT s u m ()
whiteSpace = P.whiteSpace lexer
parens :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
parens = P.parens lexer
semiSep1 :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
semiSep1 = P.semiSep1 lexer
commaSep :: Stream s m Char => ParsecT s u m a -> ParsecT s u m [a]
commaSep = P.commaSep lexer

lexer :: Stream s m Char => GenTokenParser s u m
lexer = P.makeTokenParser claqDef

claqDef :: Stream s m Char => GenLanguageDef s u m
claqDef = emptyDef
                { P.commentStart   = "{-"
                , P.commentEnd     = "-}"
                , P.commentLine    = "--"
                , P.nestedComments = True
                , P.identStart     = letter <|> oneOf "_."
                , P.identLetter  = alphaNum <|> oneOf "_.'"
                , P.opStart  = P.opLetter claqDef
                , P.opLetter   = oneOf ":!#$%&*+/<=>?@\\^|-~"
                , P.reservedOpNames= ["~", "&", "^", "|"]
                , P.reservedNames  = [".inputs", ".outputs"]
                , P.caseSensitive  = True
                }
