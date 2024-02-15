module Main (main) where

import qualified Lexer
import Parser
import Text.Megaparsec (parse, errorBundlePretty)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    s <- getContents
    print ((Lexer.joinWhitespaceToks . Lexer.alexScanTokens) s)
