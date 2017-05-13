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
  parseTest expr "name Zeeshan, number 9000 , number 9001"

data Expr
  = Name String
  | Number Integer
  | Seq [Expr]
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
  n <- lexeme (some letterChar)
  return (Name n)

number :: Parser Expr
number = do
  symbol "number"
  n <- integer
  return (Number n)

expr :: Parser Expr
expr = between sc eof (f <$> sepBy1 (name <|> number) (symbol ","))
  where f l = if length l == 1 then head l else Seq l
