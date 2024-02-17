module Lexer (
  LToken(..),
  pToken,
  pParens,
  pBrackets,
  pInteger,
  unInteger,
  pIdentifier,
  unIdentifier,
  pString,
  unString,
  pChar,
  unChar
) where

import Common
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1, string, alphaNumChar, letterChar, asciiChar)
import Data.Void
import Control.Monad

data LToken
  = Let

  | Stream

  | Rcv
  | Send
  | Catch

  | If
  | While
  | For

  | ContInt !Int
  | ConstFloat !Float
  | ConstChar !Char
  | ConstString !String

  | Dot
  | Comma
  | Colon

  | Lsr
  | Gtr
  | Eq
  | Neq
  | Geq
  | Leq

  | And
  | Or

  | Assign

  | Incr
  | Decr

  | Operate !Char

  | Pipe
  | Semicolon

  | Add
  | Sub
  | Mult
  | Div

  | Arrow

  | Identifier !String

  deriving (Eq, Ord, Show)

space :: Parser ()
space = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme = L.lexeme space

symbol = lexeme . string


pInteger :: Parser LToken
pInteger = ContInt <$> L.signed space (lexeme L.decimal)

unInteger (ContInt i) = i


-- XXX: could probably use between here
pString :: Parser LToken
pString = do -- ok if we don't backtrack, a " always starts a string, nothing else
  void $ single '"'
  s <- takeWhileP Nothing (/= '"')
  void $ single '"'
  return $ ConstString s

unString (ConstString s) = s


pChar :: Parser LToken
pChar = ConstChar <$> between (single '\'') (single '\'') asciiChar

unChar (ConstChar c) = c


pToken :: LToken -> Parser LToken
pToken Let = Let <$ symbol "let"

pToken Stream = Stream <$ symbol "stream"

pToken Catch = Catch <$ symbol "catch"

pToken If = If <$ symbol "if"
pToken While = While <$ symbol "while"
pToken For = For <$ symbol "for"
pToken Dot = Dot <$ symbol "."
pToken Comma = Comma <$ symbol ","
pToken Colon = Colon <$ symbol ":"

pToken Lsr = Lsr <$ symbol "<"
pToken Gtr = Gtr <$ symbol ">"
pToken Eq = Eq <$ symbol "=="
pToken Neq = Neq <$ symbol "!="
pToken Geq = Geq <$ symbol ">="
pToken Leq = Leq <$ symbol "<="

pToken And = And <$ symbol "&&"
pToken Or = Or <$ symbol "||"

pToken Assign = Assign <$ symbol "="

pToken Incr = Incr <$ symbol "++"
pToken Decr = Decr <$ symbol "--"

pToken Pipe = Pipe <$ symbol "|"
pToken Semicolon = Semicolon <$ symbol ";"

pToken Add = Add <$ symbol "+"
pToken Sub = Sub <$ symbol "-"
pToken Mult = Mult <$ symbol "*"
pToken Div = Div <$ symbol "/"

pToken Arrow = Arrow <$ symbol "->"


pParens    = between (symbol "(") (symbol ")")
pPraces    = between (symbol "{") (symbol "}")
pBrackets  = between (symbol "[") (symbol "]")


pIdentifier :: Parser LToken
pIdentifier = Identifier <$> try (lexeme ident)
  where
    ident = do
      c <- letterChar
      rest <- many (alphaNumChar <|> single '.')
      return (c:rest)

unIdentifier (Identifier i) = i
