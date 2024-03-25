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
import System.Exit (exitWith, ExitCode (ExitFailure))

main :: IO ()
main = do
    s <- getContents
    case parse pModule "main.slg" s of
        Left bundle -> do
            putStr (errorBundlePretty bundle)
            exitWith (ExitFailure 1)
        Right parsed -> do
            let types = Map.fromList (map A.extractType parsed)
            case testAssembleModule types parsed of
                Left err -> do
                    putStrLn err
                    exitWith (ExitFailure 1)
                Right assembled ->
                    putStr $ T.unpack assembled
