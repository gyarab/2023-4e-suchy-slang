module Lexer (
  LToken(..),
  pType,
  pToken,
  pParens,
  pBraces,
  pBrackets,
  pInteger,
  unInteger,
  pIdentifier,
  pIdentifierSingle,
  pIdentifierName,
  unIdentifier,
  pString,
  unString,
  pChar,
  unChar,
  pBoolean,
  unBoolean
) where

import Common
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1, string, alphaNumChar, letterChar, asciiChar, char)
import Data.Void
import Control.Monad
import qualified Types as T

data LToken
  = Let

  | Stream
  | Function -- external functions
  | Struct

  | Rcv
  | Catch

  | If
  | Else
  | While
  | For

  | ConstInt !Int
  | ConstFloat !Float
  | ConstChar !Char
  | ConstString !String
  | ConstBoolean !Bool

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

  | Ellipsis

  | Extern

  | Deref
  | Ref

  | Identifier ![String]

  deriving (Eq, Ord, Show)

space :: Parser ()
space = L.space space1 (L.skipLineComment "//") (L.skipBlockComment "/*" "*/")

lexeme = L.lexeme space

symbol = lexeme . string


pInteger :: Parser LToken
pInteger = ConstInt <$> L.signed space (lexeme L.decimal)

unInteger (ConstInt i) = i


pString :: Parser LToken
pString = lexeme $ char '"' >> ConstString <$> manyTill L.charLiteral (char '"')

unString (ConstString s) = s


pChar :: Parser LToken
pChar = lexeme $ ConstChar <$> between (single '\'') (single '\'') L.charLiteral

unChar (ConstChar c) = c

pBoolean :: Parser LToken
pBoolean = ConstBoolean True <$ symbol "true" <|> ConstBoolean False <$ symbol "false"

unBoolean (ConstBoolean b) = b

pToken :: LToken -> Parser LToken
pToken Let = Let <$ symbol "let"

pToken Stream = Stream <$ symbol "stream"
pToken Function = Stream <$ symbol "fn"
pToken Struct = Struct <$ symbol "struct"

pToken Catch = Catch <$ symbol "catch"

pToken If = If <$ symbol "if"
pToken Else = Else <$ symbol "else"
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

pToken Ellipsis = Ellipsis <$ symbol "..."

pToken Extern = Extern <$ symbol "extern"

-- !! same as Mult, we will have to guess from context
pToken Deref = Deref <$ symbol "*"
pToken Ref = Ref <$ symbol "&"

pType :: Parser T.Type
pType = lexeme $ choice
  [ T.Boolean <$ symbol "bool"
  , T.I64 <$ symbol "i64"
  , T.I32 <$ symbol "i32"
  , T.Char <$ symbol "char"
  ]

pParens    = between (symbol "(") (symbol ")")
pBraces    = between (symbol "{") (symbol "}")
pBrackets  = between (symbol "[") (symbol "]")


pIdentifierName :: Parser String
pIdentifierName = (:) <$> letterChar <*> many alphaNumChar <?> "identifier"

pIdentifierSingle :: Parser String
pIdentifierSingle = lexeme pIdentifierName

pIdentifier :: Parser LToken
pIdentifier = Identifier <$> try (lexeme ident)
  where
    ident = do
      c <- pIdentifierName
      rest <- many (single '.' *> pIdentifierName)
      return $ c:rest

unIdentifier (Identifier i) = i
