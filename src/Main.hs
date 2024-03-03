module Main (main) where

import qualified Lexer
import Parser(pModule)
import Codegen(testAssembleStream)
import Text.Megaparsec (parse, errorBundlePretty, parseTest)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T

main :: IO ()
main = do
    s <- getContents
    case parse pModule "main.slg" s of
        Left bundle -> putStr (errorBundlePretty bundle)
        Right parsed -> putStr $ T.unpack (testAssembleStream $ head parsed)
