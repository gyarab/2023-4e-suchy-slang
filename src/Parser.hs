module Parser (pModule, ASTNode (..), separatedBy, pExpression) where

import Common
import Control.Monad
import Control.Monad.Combinators.Expr
import Data.Void
import Lexer (pIdentifier, pIdentifierSingle, pToken)
import qualified Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char (alphaNumChar, letterChar, space1, string)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Types as T
import Types (Type)

data ASTNode
  = Declare
      { identifier :: !String,
        iType :: !Type,
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
  | Call ![ASTNode] !ASTNode -- args, name
  | IfElse !ASTNode ![ASTNode] !(Maybe [ASTNode])
  | Stream
      { name :: !String,
        arugments :: ![Type],
        retVal :: !Type,
        body :: !(Maybe [ASTNode])
      }
  | ExternFunction
      { name :: !String,
        arugments :: ![Type],
        retVal :: !Type
      }
  | Struct
      { name :: !String,
        attributes :: !(Map String Type)
      }
  | Dereference {
        times :: !Int,
        node :: !ASTNode
    }
  | Reference {
        times :: !Int,
        node :: !ASTNode
    }
  | Assign !ASTNode !ASTNode
  deriving (Eq, Ord, Show)

separatedBy :: Parser b -> Parser a -> Parser [a]
separatedBy pb pa = (:) <$> pa <*> many (pb *> pa)

-- Main parser. Expects a whole module as input.
pModule :: Parser [ASTNode]
pModule = many (pStream <|> pExtern <|> pStruct)

pStream :: Parser ASTNode
pStream = do
  void $ pToken L.Stream

  name <- pIdentifierSingle

  args <- pArguments

  void $ pToken L.Arrow

  retType <- pType

  body <- (Nothing <$ pToken L.Semicolon) <|> (Just <$> pBlock)

  return $ Stream name args retType body
  where
    pArgument :: Parser Type
    pArgument = pType

    pArguments :: Parser [Type]
    pArguments = L.pParens $ do
      exp <- optional . try $ pArgument
      moreExprs <- nextArg
      return $ case exp of
        Just e -> e : moreExprs
        Nothing -> moreExprs

    nextArg = many $ try (pToken L.Comma *> pArgument)

pExtern :: Parser ASTNode
pExtern = do
  void $ pToken L.Extern
  void $ pToken L.Function

  name <- pIdentifierSingle

  args <- pArguments

  void $ pToken L.Arrow

  retType <- pType

  void $ pToken L.Semicolon

  return $ ExternFunction name args retType
  where
    pArgument = pType <|> (T.Ellipsis <$ pToken L.Ellipsis) -- allow ellipsies for externs

    pArguments = L.pParens $ do
      exp <- optional . try $ pArgument
      moreExprs <- nextArg
      return $ case exp of
        Just e -> e : moreExprs
        Nothing -> moreExprs

    nextArg = many $ try (pToken L.Comma *> pArgument)

pStruct :: Parser ASTNode
pStruct = do
  void $ pToken L.Struct

  name <- pIdentifierSingle

  fields <- L.pBraces (separatedBy (pToken L.Comma) pField)

  return $ Struct name (Map.fromList fields)

  where
    pField = do
      name <- pIdentifierSingle
      void $ pToken L.Colon
      fType <- pType
      return (name, fType)

pBlock :: Parser [ASTNode]
pBlock = L.pBraces $ many pStatement

pType :: Parser T.Type
pType = do
  dereferences <- many (pToken L.Ref)
  baseType <- L.pType

  return $ foldr (const T.ref) baseType dereferences

pStatement :: Parser ASTNode
pStatement =
  choice
    [ pLetStatement,
      pIfElseStatement,
      pExpression <* pToken L.Semicolon
    ]

pLetStatement :: Parser ASTNode
pLetStatement = do
  void $ pToken L.Let

  ident <- pIdentifierSingle

  void $ pToken L.Colon

  iType <- pType

  ex <- optional . try $ (void (pToken L.Assign) *> pExpression)

  void $ pToken L.Semicolon

  return (Declare ident iType ex)

pIfElseStatement :: Parser ASTNode
pIfElseStatement = do
  void $ pToken L.If

  condition <- pExpression

  action <- pBlock <|> ((: []) <$> pStatement)

  elseAction <- optional . try $ do
    void $ pToken L.Else
    pBlock <|> ((: []) <$> pStatement)

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
    Just e -> e : moreExprs
    Nothing -> moreExprs
  where
    nextExpr = many $ try (pToken L.Comma *> pExpression)

pTerm =
  L.pParens pExpression
    <|> Identifier
    . L.unIdentifier
    <$> L.pIdentifier
      <|> ConstInt
      . L.unInteger
    <$> L.pInteger
      <|> ConstString
      . L.unString
    <$> L.pString
      <|> ConstChar
      . L.unChar
    <$> L.pChar

table =
  [ [ Prefix (Dereference . length <$> some (pToken L.Deref)),
      Prefix (Reference . length <$> some (pToken L.Ref))
    ],
    [ Postfix (Call <$> pArguments)
    ],
    [ Prefix (Negate <$ pToken L.Sub) -- negate the term
    ],
    [ InfixL (Multiply <$ pToken L.Mult),
      InfixL (Divide <$ pToken L.Div)
    ],
    [ InfixL (Add <$ pToken L.Add),
      InfixL (Subtract <$ pToken L.Sub)
    ],
    [
      InfixR (Assign <$ pToken L.Assign)
    ]
  ]

pExpression = makeExprParser pTerm table
