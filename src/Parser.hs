module Parser (pModule, ASTNode (..), separatedBy, pExpression, pTuple) where

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
        iType :: !(Maybe Type),
        value :: !(Maybe ASTNode)
      }
  | Not !ASTNode
  | And !ASTNode !ASTNode
  | Or !ASTNode !ASTNode
  | Negate !ASTNode
  | Add !ASTNode !ASTNode
  | Subtract !ASTNode !ASTNode
  | Multiply !ASTNode !ASTNode
  | Divide !ASTNode !ASTNode
  | Modulo !ASTNode !ASTNode
  | Lsr !ASTNode !ASTNode
  | Gtr !ASTNode !ASTNode
  | Eq !ASTNode !ASTNode
  | Neq !ASTNode !ASTNode
  | Geq !ASTNode !ASTNode
  | Leq !ASTNode !ASTNode
  | ConstInt !Int
  | ConstString !String
  | ConstChar !Char
  | ConstFloat !Float
  | ConstBoolean !Bool
  | Identifier ![String]
  | Call ![ASTNode] !ASTNode -- args, name
  | IfElse !ASTNode ![ASTNode] !(Maybe [ASTNode])
  | While !ASTNode ![ASTNode]
  | Stream
      { name :: !String,
        inType :: !Type,
        outType :: !Type,
        body :: !(Maybe [ASTNode])
      }
  | ExternFunction
      { name :: !String,
        arguments :: ![Type],
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
  | Catch !ASTNode
  | Pipe !ASTNode !ASTNode
  | Cast !Type !ASTNode
  | MakeTuple ![ASTNode]
  | Index !ASTNode !ASTNode
  | IfLet !String !ASTNode ![ASTNode] !(Maybe [ASTNode]) -- identifier, val, action, else
  | WhileLet !String !ASTNode ![ASTNode] -- identifier, val, loop
  deriving (Eq, Ord, Show)

separatedBy :: Parser b -> Parser a -> Parser [a]
separatedBy pb pa = (:) <$> pa <*> many (pb *> pa)

-- Main parser. Expects a whole module as input.
pModule :: Parser [ASTNode]
pModule = L.space >> some (pStream <|> pExtern <|> pStruct)

pStream :: Parser ASTNode
pStream = do
  void $ pToken L.Stream

  name <- pIdentifierSingle

  args <- pType

  void $ pToken L.Arrow

  retType <- pType

  body <- (Nothing <$ pToken L.Semicolon) <|> (Just <$> pBlock)

  return $ Stream name args retType body

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

  fields <- try pSimple <|> L.pBraces (separatedBy (pToken L.Comma) pField)

  return $ Struct name (Map.fromList fields)

  where
    pField = do
      name <- pIdentifierSingle
      void $ pToken L.Colon
      fType <- pType
      return (name, fType)

    pSimple = do
      void $ pToken L.Assign
      typ <- pType
      case typ of
        T.Tuple types -> return $ zipWith (\x y -> (show x, y)) [1..] types
        other -> fail "structure can be either a tuple or a fully defined structure"


pBlock :: Parser [ASTNode]
pBlock = L.pBraces $ many pStatement

pType :: Parser T.Type
pType = atom <|> T.Tuple <$> pMultiple

  where
    atom :: Parser Type
    atom = do
      dereferences <- many (pToken L.Ref)
      baseType <- L.pType

      return $ foldr (const T.ref) baseType dereferences

    pMultiple :: Parser [Type]
    pMultiple = L.pParens $ do
      exp <- optional . try $ atom
      moreExprs <- nextAtom
      return $ case exp of
        Just e -> e : moreExprs
        Nothing -> moreExprs

    nextAtom = many $ try (pToken L.Comma *> atom)

pStatement :: Parser ASTNode
pStatement =
  choice
    [ pLetStatement,
      try pIfLetStatement,
      pIfElseStatement,
      try pWhileLetStatement,
      pWhileStatement,
      pExpression <* pToken L.Semicolon
    ]

pLetStatement :: Parser ASTNode
pLetStatement = do
  void $ pToken L.Let

  ident <- pIdentifierSingle

  iType <- optional . try $ do
    void $ pToken L.Colon
    pType

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

pIfLetStatement :: Parser ASTNode
pIfLetStatement = do
  void $ pToken L.If
  void $ pToken L.Let

  ident <- pIdentifierSingle

  iType <- optional . try $ do
    void $ pToken L.Colon
    pType

  void $ pToken L.Assign

  ex <- pExpression

  action <- pBlock <|> ((: []) <$> pStatement)

  elseAction <- optional . try $ do
    void $ pToken L.Else
    pBlock <|> ((: []) <$> pStatement)

  return (IfLet ident ex action elseAction)


pWhileLetStatement :: Parser ASTNode
pWhileLetStatement = do
  void $ pToken L.While
  void $ pToken L.Let

  ident <- pIdentifierSingle

  iType <- optional . try $ do
    void $ pToken L.Colon
    pType

  void $ pToken L.Assign

  ex <- pExpression

  action <- pBlock <|> ((: []) <$> pStatement)

  return (WhileLet ident ex action)


pWhileStatement :: Parser ASTNode
pWhileStatement = do
  void $ pToken L.While

  condition <- pExpression

  loop <- pBlock <|> ((: []) <$> pStatement)

  return (While condition loop)

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

pTuple :: Parser ASTNode
pTuple = L.pParens $ MakeTuple <$> do
  t <- many (try $ pExpression <* pToken L.Comma)
  if not (null t) then do
    last <- optional . try $ pExpression
    case last of
      Just l -> return (t ++ [l])
      Nothing -> return t
  else
    return t

pIndex :: Parser ASTNode
pIndex = L.pBrackets pExpression

  
pTerm =
  try pTuple <|>
  L.pParens pExpression
    <|> ConstBoolean . L.unBoolean <$> L.pBoolean
    <|> Identifier . L.unIdentifier <$> L.pIdentifier
    <|> ConstInt . L.unInteger <$> L.pInteger
    <|> ConstString . L.unString <$> L.pString
    <|> ConstChar . L.unChar <$> L.pChar

pCast :: Parser Type
pCast = do
  void $ pToken L.As
  pType

table =
  [ [ Postfix (Call <$> pArguments)
    ],
    [ Postfix (Index <$> pIndex)
    ],
    [ Prefix (Dereference . length <$> some (pToken L.Deref)),
      Prefix (Reference . length <$> some (pToken L.Ref))
    ],
    [ Prefix (Negate <$ pToken L.Sub) -- negate the term
    ],
    [ Postfix (Cast <$> pCast)
    ],
    [ InfixL (Multiply <$ pToken L.Mult),
      InfixL (Divide <$ pToken L.Div),
      InfixL (Modulo <$ pToken L.Mod)
    ],
    [ InfixL (Add <$ pToken L.Add),
      InfixL (Subtract <$ pToken L.Sub)
    ],
    [
      InfixL (Eq <$ pToken L.Eq),
      InfixL (Neq <$ pToken L.Neq),
      InfixL (Geq <$ pToken L.Geq),
      InfixL (Leq <$ pToken L.Leq),
      InfixL (Gtr <$ pToken L.Gtr),
      InfixL (Lsr <$ pToken L.Lsr)
    ],
    [
      Prefix (Not <$ pToken L.Not),
      InfixL (And <$ pToken L.And),
      InfixL (Or <$ pToken L.Or)
    ],
    [
      InfixL (Pipe <$ pToken L.Pipe)
    ],
    [
      Prefix (Catch <$ pToken L.Catch),
      InfixR (Assign <$ pToken L.Assign)
    ]
  ]

pExpression = makeExprParser pTerm table
