module Parser where

import Common
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char (space1, string, alphaNumChar, letterChar)
import Data.Void
import Control.Monad
import qualified Lexer as L
import Lexer (pToken, pIdentifier)
import Control.Monad.Combinators.Expr

data ASTNode
  = Declare {
      identifier :: !String,
      iType :: !String,
      value :: !(Maybe ASTNode)
    }
  | Negate !ASTNode
  | Add !ASTNode !ASTNode
  | Subtract !ASTNode !ASTNode
  | Multiply !ASTNode !ASTNode
  | Divide !ASTNode !ASTNode
  | ConstInt !Int
  | ConstString !String
  | ConstChar !Char
  | ConstFloat !Float
  | Identifier ![String]
  | Call !ASTNode
  deriving (Eq, Ord, Show)


pLetStatement :: Parser ASTNode
pLetStatement = do
  void $ pToken L.Let

  L.Identifier ident <- pIdentifier

  void $ pToken L.Colon

  L.Identifier iType <- pIdentifier <?> "type"

  ex <- optional . try $ (void (pToken L.Assign) *> pExpression)

  void $ pToken L.Semicolon
  return (Declare ident iType ex)

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

pTerm = 
  L.pParens pExpression
  <|> Identifier . wordsWhen (=='.') . L.unIdentifier <$> L.pIdentifier
  <|> ConstInt . L.unInteger <$> L.pInteger
  <|> ConstString . L.unString <$> L.pString
  <|> ConstChar . L.unChar <$> L.pChar

table = [
  [
    Prefix (Negate <$ pToken L.Sub) -- negate the term
  ],
  [
    InfixL (Multiply <$ pToken L.Mult),
    InfixL (Divide <$ pToken L.Div)
  ],
  [
    InfixL (Add <$ pToken L.Add),
    InfixL (Subtract <$ pToken L.Sub)
  ]
 ]

pExpression = makeExprParser pTerm table
