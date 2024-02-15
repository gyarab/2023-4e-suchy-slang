{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RecordWildCards #-}

module Parser where

import Text.Megaparsec
import qualified Lexer
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Void
import qualified Data.List          as DL
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as Set

type TokStream = [Lexer.TWithRaw]
instance VisualStream TokStream where
  showTokens Proxy toks = Lexer.raw frst ++ restStr
    where
      (frst:rest) = NE.toList toks
      restStr = concatMap (\t -> Lexer.wspc t ++ Lexer.raw t) rest
  tokensLength p toks = length (showTokens p toks)

instance TraversableStream TokStream where
  reachOffset o PosState {..} =
    ( Just (prefix ++ restOfLine)
    , PosState
        { pstateInput = restStream
        , pstateOffset = max pstateOffset o
        , pstateSourcePos = newSourcePos
        , pstateTabWidth = pstateTabWidth
        , pstateLinePrefix = prefix
        }
    )
    where
      (consumedStream, restStream) = splitAt (o - pstateOffset) pstateInput

      -- 
      prefix =
        if sameLine
          then pstateLinePrefix ++ preLine
          else preLine

      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos

      newSourcePos =
        case restStream of
          [] -> case pstateInput of
            [] -> pstateSourcePos
            xs -> updateSourcePosTk pstateSourcePos (last xs)
          (x:_) -> updateSourcePosTk pstateSourcePos x

      -- formatted line up until the current token (it could be missing a part
      preLine =
        if not (null onPrevLines)
          then let (newLineTok:_) = onPrevLines in (reverse . takeWhile (/= 'n') . reverse . Lexer.wspc $ newLineTok) ++ Lexer.raw newLineTok ++ lineWithoutFirstTok
          else lineWithoutFirstTok
        where
          lineWithoutFirstTok = concatMap (\t -> Lexer.wspc t ++ Lexer.raw t) (reverse onThisLine)
          (onThisLine, onPrevLines) = break (\t -> "\n" `DL.isInfixOf` Lexer.wspc t) . reverse $ consumedStream

      restOfTokensOnLine = takeWhile (\t -> not ("\n" `DL.isInfixOf` Lexer.wspc t)) restStream
      restOfLine = concatMap (\t -> Lexer.wspc t ++ Lexer.raw t) restOfTokensOnLine

updateSourcePos (SourcePos fn _ _) (Lexer.AlexPn _ l c) = SourcePos fn (mkPos l) (mkPos c)
updateSourcePosTk sp tk = updateSourcePos sp (Lexer.pos tk)

type Parser = Parsec Void TokStream


pToken :: Lexer.Token -> Parser Lexer.Token
pToken c = token test (Set.singleton . Tokens . nes . liftMyToken $ c)
  where
    test (Lexer.TWithRaw _ x _ _) =
      if x == c
        then Just x
        else Nothing
    nes x = x :| []

pLet :: Parser Bool
pLet = do
  single Lexer.Let
  return True
