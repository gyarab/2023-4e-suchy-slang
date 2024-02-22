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
  | Call ![ASTNode] !ASTNode
  | IfElse !ASTNode ![ASTNode] !(Maybe [ASTNode])
  deriving (Eq, Ord, Show)

pBlock :: Parser [ASTNode]
pBlock = L.pBraces $ many pStatement

pStatement :: Parser ASTNode
pStatement = choice [
    pLetStatement,
    pIfElseStatement,
    pExpression <* pToken L.Semicolon
  ]

pLetStatement :: Parser ASTNode
pLetStatement = do
  void $ pToken L.Let

  L.Identifier ident <- pIdentifier

  void $ pToken L.Colon

  L.Identifier iType <- pIdentifier <?> "type"

  ex <- optional . try $ (void (pToken L.Assign) *> pExpression)

  void $ pToken L.Semicolon

  return (Declare ident iType ex)

pIfElseStatement :: Parser ASTNode
pIfElseStatement = do
  void $ pToken L.If

  condition <- pExpression

  action <- pBlock <|> ((:[]) <$> pStatement)

  elseAction <- optional . try $ do
    void $ pToken L.Else
    pBlock <|> ((:[]) <$> pStatement)

  return (IfElse condition action elseAction)
    


wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s''
    where
      (w, s'') = break p s'

pArguments :: Parser [ASTNode]
pArguments = L.pParens $ do
  exp <- optional . try $ pExpression
  moreExprs <- nextExpr
  return $ case exp of
            Just e -> e:moreExprs
            Nothing -> moreExprs
  where
    nextExpr = many $ try (pToken L.Comma *> pExpression)


pTerm = 
  L.pParens pExpression
  <|> Identifier . wordsWhen (=='.') . L.unIdentifier <$> L.pIdentifier
  <|> ConstInt . L.unInteger <$> L.pInteger
  <|> ConstString . L.unString <$> L.pString
  <|> ConstChar . L.unChar <$> L.pChar

table = [
  [
    Postfix (Call <$> pArguments)
  ],
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
