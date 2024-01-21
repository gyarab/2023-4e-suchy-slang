module Main (main) where

import qualified Lexer
import Parser

main :: IO ()
main = do
    s <- getContents
    tok <- Lexer.alexScanTokens s
    
