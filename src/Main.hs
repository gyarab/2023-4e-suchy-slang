module Main (main) where

import qualified Lexer

main :: IO ()
main = do
    s <- getContents
    print $ Lexer.alexScanTokens s
