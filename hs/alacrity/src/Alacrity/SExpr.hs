--- Copied from:
{-|
Module      : Text.SExpression.Internal
Description : Internal parser functions
Copyright   : (C) Richard Cook, 2019
Licence     : MIT
Maintainer  : rcook@rcook.org
Stability   : stable
Portability : portable
This module provides internal parser functions.
-}

module Alacrity.SExpr
    ( parseSExpr
    , Parser
    , SExpr(..)
    ) where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
    ( (<|>)
    , endBy
    , many
    , noneOf
    , oneOf
    , sepBy
    , some
    , try
    )
import Text.Megaparsec.Char
    ( char
    , digitChar
    , letterChar
    , space1
    )
import Text.Megaparsec.Char.Lexer
    ( space
    , skipLineComment
    )

import Data.Void (Void)
import Text.Megaparsec (Parsec)

-- | Parser context
type Parser = Parsec Void String

-- | S-expression values
data SExpr =
    Atom String                 -- ^ atom
    | List [SExpr]              -- ^ list
    | Number Integer            -- ^ number literal
    | String String             -- ^ string literal
    | Bool Bool                 -- ^ Boolean literal
    deriving (Eq, Read, Show)

sc :: Parser ()
sc = space space1 lineComment empty
    where
        lineComment = skipLineComment ";"

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~#"

-- | S-expression parser
parseSExpr :: Parser SExpr    -- ^ parser
parseSExpr = do
  sc
  se <- (parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseList)
  sc
  pure $ se

-- | Parse s-expression atom
parseAtom ::
    Parser SExpr    -- ^ parser
parseAtom = do
    h <- letterChar <|> symbol
    t <- many (letterChar <|> digitChar <|> symbol)
    let s = h : t
    pure $ case s of
                "#t" -> Bool True
                "#f" -> Bool False
                _ -> Atom s

-- | Parse s-expression list
parseList :: Parser SExpr    -- ^ parser
parseList = do
  void $ char '('
  lst <- parseSExpr `sepBy` sc
  void $ char ')'
  pure $ List lst

-- | Parse s-expression number literal
parseNumber :: Parser SExpr    -- ^ parser
parseNumber = (Number . read) <$> some digitChar

-- | Parse s-expression string literal
parseString :: Parser SExpr    -- ^ parser
parseString = do
    void $ char '"'
    s <- many (noneOf "\"")
    void $ char '"'
    pure $ String s
