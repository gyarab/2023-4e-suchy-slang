module Main (main) where

import qualified Lexer
import Parser(pModule)
import Codegen(testAssembleModule)
import Text.Megaparsec (parse, errorBundlePretty, parseTest)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import qualified Analyser as A
import Data.Maybe (isJust)
import Control.Monad (when)
import qualified Data.Map as Map

main :: IO ()
main = do
    s <- getContents
    case parse pModule "main.slg" s of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right parsed -> do
            let types = Map.fromList (map A.extractType parsed)
            -- mapM_ ((\x -> when (isJust x) $ print x) . A.getTypeError types) parsed
            case testAssembleModule types parsed of
                Left err -> putStr err
                Right assembled ->
                    putStr $ T.unpack assembled
