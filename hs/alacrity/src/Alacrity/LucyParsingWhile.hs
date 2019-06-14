{-| Notes & Questions
   $1. (void)
   $2. L.lexeme
   $3. between -}

module Main (main) where

import Control.Monad (void) -- $1
import Control.Monad.Combinators.Expr
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

{-| Expression grammar:
    a   ::= x | n | - a | a opa a
    b   ::= true | false | not b | b opb b | a opr a
    opa ::= + | - | * | /
    opb ::= and | or
    opr ::= > | < -}

data AExpr
  = Var  String
  | IntC Integer
  | Neg  AExpr
  | ABin ABinOp AExpr AExpr

data BExpr
  = BoolC Bool
  | Not   BExpr
  | BBin  BBinOp BExpr BExpr
  | RBin  RBinOp AExpr AExpr

data ABinOp
  = Add
  | Subtract
  | Multiiply
  | Divide

data BBin
  = And
  | Or

data RBin
  = Greater
  | Less

{-| Statement grammar:
    S ::= x := a | skip | S1; S2 | ( S ) | if b then S1 else S2 | while b do S -}

data Stmt
  = Assign String AExpr
  | Skip
  | Seq [Stmt]
  | If BExpr Stmt Stmt
  | While BExpr Stmt

type Parser = Parsec Void String

{-| Lexer -}

sc :: Parser () -- $2
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "//"
    blockCmnt = L.skipBlockComment "/*" "*/"

symbol :: String -> Parser String
L.symbol = sc

-- This is a wrapper for lexemes.
-- Typical usage is to supply the first argument
-- (parser that consumes white space, probably
-- defined via space) and use the resulting function
-- to wrap parsers for every lexeme.
lexeme :: Parser a -> Parser a -- $2
lexeme = L.lexeme sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")") -- $3

integer :: Parser Integer
integer = lexeme L.decimal

semi :: Parser String
semi = symbol ";"

rws :: [String] -- list of reserved words
rws = ["if","then","else","while","do","skip","true","false","not","and","or"]

rword :: String -> Parser
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar) -- $ . try *>

identifier :: Parser String
identifier = (lexeme . try) (p >>= check) -- $
  where
    p       = (:) <$> letterChar <*> many alphaNumChar -- (:) <$> <*>
    check x = if x `elem` rws
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

{-| Parser -}

aTerm :: Parser AExpr
aTerm =  parens aExpr
     <|> Var      <$> identifier
     <|> IntConst <$> integer

bTerm :: Parser BExpr
bTerm =  parens bExpr
     <|> (BoolConst True  <$ rword "true")
     <|> (BoolConst False <$ rword "false")
     <|> rExpr

bOperators :: [[Operator Parser BExpr]]
bOperators =
  [ [Prefix (Not <$ rword "not") ]
  , [InfixL (BBinary And <$ rword "and")
    , InfixL (BBinary Or <$ rword "or") ]
  ]

aOperators :: [[Operator Parser AExpr]]
aOperators =
  [ [Prefix (Neg <$ symbol "-") ]
  , [ InfixL (ABinary Multiply <$ symbol "*")
    , InfixL (ABinary Divide   <$ symbol "/") ]
  , [ InfixL (ABinary Add      <$ symbol "+")
    , InfixL (ABinary Subtract <$ symbol "-") ]
  ]

aExpr :: Parser AExpr
aExpr = makeExprParser aTerm aOperators

bExpr :: Parser BExpr
bExpr = makeExprParser bTerm bOperators


stmt :: Parser Stmt
stmt = f <$> sepBy1 stmt' semi
  where
    f l = if length l == 1 then head l else Seq l

assignStmt :: Parser Stmt
assignStmt = do
  var  <- identifier
  void (symbol ":=") -- $
  expr <- aExpr
  return (Assign var expr)

skipStmt :: Parser Stmt
skipStmt = Skip <$ rword "skip" -- $ $>

ifStmt :: Parser Stmt
ifStmt = do
  rword "if"
  cond  <- bExpr
  rword "then"
  stmt1 <- stmt
  rword "else"
  stmt2 <- stmt
  return (If cond stmt1 stmt2)

whileParser :: Parser Stmt
whileParser = between sc eof stmt -- $

whileStmt :: Parser Stmt
whileStmt = do
  rword "while"
  cond <- bExpr
  rword "do"
  stmt1 <- stmt
  return (While cond stmt1)

stmt' :: Parser Stmt
stmt' = assignStmt
  <|> skipStmt
  <|> ifStmt
  <|> whileStmt
  <|> parens stmt

relation :: Parser RBinOp
relation = (Greater <$ symbol ">")
  <|> (Less <$ symbol "<")

rExpr :: Parser BExpr
rExpr = do
  a1 <- aExpr
  op <- relation
  a2 <- aExpr
  return (RBinary op a1 a2)

main :: IO ()
main = do
  input <- getContents
  parseTest whileParser input