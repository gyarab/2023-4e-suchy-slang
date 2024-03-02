module Main (main) where

import qualified Lexer
import Parser
import Text.Megaparsec (parse, errorBundlePretty, parseTest)
import Control.Concurrent (threadDelay)

main :: IO ()
main = do
    s <- getContents
    parseTest pModule s
    -- case parse pModule "main.slg" s of
    --     Left bundle -> putStr (errorBundlePretty bundle)
    --     Right parsed -> print parsed
