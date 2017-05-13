module Lib where

import Control.Applicative (empty)
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

generateOutput :: IO ()
generateOutput = do
  parseTest expr "name Zeeshan"
  parseTest expr "number 9000"

-- grammar
-- Expr   ::= Name | Number
-- Name   ::= 'name' a
-- Number ::= 'number' a

data Expr
  = Name String
  | Number Integer
  deriving (Show)

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

integer :: Parser Integer
integer = lexeme L.integer

name :: Parser Expr
name = do
  symbol "name"
  n <- some letterChar
  return (Name n)

number :: Parser Expr
number = do
  symbol "number"
  n <- integer
  return (Number n)

expr :: Parser Expr
expr = between sc eof (name <|> number)
