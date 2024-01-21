{-# LANGUAGE TypeFamilies #-}
module Parser where

import qualified Lexer

import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Void
import Text.Megaparsec
import qualified Data.List          as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set


data ASTNode
    = Function {
        arguments :: ![String],
        rval :: !String,
        body :: ![ASTNode]
    }
    | Statement ![ASTNode]
    | Assign
    deriving (Eq, Show)

type MyToken = Lexer.Token

type MyStream = [MyToken]

pxy :: Proxy MyStream
pxy = Proxy

-- instance Stream [MyToken] where
--   type Token  [MyToken] = MyToken
--   type Tokens [MyToken] = [MyToken]
--
--   tokensToChunk Proxy xs = xs
--   chunkToTokens Proxy = id
--   chunkLength Proxy = length
--
--   take1_ [] = Nothing
--   take1_ (t:ts) = Just (t, ts)
--
--   takeN_ n s
--     | n <= 0    = Just ([], s)
--     | null s    = Nothing
--     | otherwise = Just $ splitAt n s
--   takeWhile_ = DL.span

type Parser = Parsec Void Lexer.Token


