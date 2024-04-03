{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use second" #-}
module Main (main) where

import qualified Lexer
import Parser(pModule, ASTNode(ExternFunction))
import Codegen(testAssembleModule)
import Text.Megaparsec (parse, errorBundlePretty, parseTest)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import qualified Analyser as A
import Data.Maybe (isJust)
import Control.Monad (when, foldM)
import qualified Data.Map.Ordered as OMap
import qualified Data.Map as Map
import System.Exit (exitWith, ExitCode (ExitFailure, ExitSuccess), exitSuccess, exitFailure)
import System.Environment (getArgs, getProgName)
import System.Process (createProcess, proc, CreateProcess (std_out, std_in, cmdspec), StdStream (UseHandle, CreatePipe), callProcess, waitForProcess)
import System.IO (openTempFile, hPutStr, hClose)
import System.IO.Temp (withSystemTempFile)

data ParsedArgs = ParsedArgs {
        opt :: !String,
        sources :: ![(String, String)],
        outputFile :: !String,
        link :: !Bool,
        verbose :: !Bool,
        llvmOnly :: !Bool
    }

main :: IO ()
main = do
    args <- getArgs
    pa <- parseArgs args
    llvmCode <- case concat <$> mapM (uncurry (parse pModule)) (sources pa) of
        Left bundle -> do
            putStr (errorBundlePretty bundle)
            exitWith (ExitFailure 1)
        Right parsed -> do
            deduped <- OMap.assocs <$> foldM dedupe OMap.empty parsed
            let types = Map.fromList (map (\x -> (fst x, snd . snd $ x)) deduped)
            case testAssembleModule types (map (fst . snd) deduped) of
                Left err -> do
                    putStrLn err
                    exitWith (ExitFailure 1)
                Right assembled ->
                    return assembled

    when (llvmOnly pa) $ do
        putStr (T.unpack llvmCode)
        exitSuccess

    withSystemTempFile "slang" $ \fp h -> do
        let out = if link pa then fp else outputFile pa
        let prc = proc "llc" [opt pa, "-filetype=obj", "-opaque-pointers", "-o", out, "-"]

        when (verbose pa) $ do
            putStrLn ("verbose: llc command: " ++ show (cmdspec prc))

        (Just inp, _, _, ph) <- createProcess prc{std_in = CreatePipe}
        hPutStr inp (T.unpack llvmCode)
        hClose inp
        exit <- waitForProcess ph
        when (exit /= ExitSuccess) $ exitWith (ExitFailure 1)

        when (link pa) $ do
            let args = ["-o", outputFile pa, "-dynamic-linker", "/lib64/ld-linux-x86-64.so.2", "/lib/crt1.o", "/lib/crti.o", fp, "/lib/crtn.o", "-L", "/lib", "-lc"]
            when (verbose pa) $ do
                putStrLn ("verbose: ld.lld arguments: " ++ show args)
            callProcess "ld.lld" args

    where
        dedupe types node = do
            let (name, typ) = A.extractType node
            when (OMap.member name types && not (allowDedupe node)) $ do
                putStrLn ("redefinition of " ++ name)
                exitWith (ExitFailure 1)

            return (types OMap.>| (name, (node, typ)))

        allowDedupe ExternFunction {} = True
        allowDedupe _ = False

parseArgs [] = showHelp
parseArgs ("-v":xs) = do
    pa <- parseArgs xs
    return pa{verbose=True}
parseArgs ["-h"] = showHelp
parseArgs ["--help"] = showHelp
parseArgs ["--version"] = showVersion
parseArgs ["-o"] = showHelp
parseArgs ["-o", f] = showHelp

parseArgs ("-S":xs) = do
    pa <- parseArgs xs
    return pa{llvmOnly = True}

parseArgs ("-O0":xs) = setOpt "-O0" xs
parseArgs ("-O1":xs) = setOpt "-O1" xs
parseArgs ("-O2":xs) = setOpt "-O2" xs
parseArgs ("-O3":xs) = setOpt "-O3" xs

parseArgs ("-L":xs) = do
    pa <- parseArgs xs
    return pa{link = False}
parseArgs ("--no-link":xs) = do
    pa <- parseArgs xs
    return pa{link = False}

parseArgs ("-o":f:sources) = do
    pa <- parseArgs sources
    return pa{outputFile=f}
parseArgs sources = do
    contents <- mapM readFile sources
    return (ParsedArgs "-O3" (zip sources contents) "a.out" True False False)

showHelp = do
    name <- getProgName
    putStrLn ("Usage: " ++ name ++ " [OPTION]... [-o OUTPUT_FILE] SOURCE_FILE...")
    putStrLn "  -h, --help       Show this help message"
    putStrLn "  --version        Show version info"
    putStrLn "  -OX              Optimization level (O0,O1,O2,O3) (default: O3)"
    putStrLn "  -o               Output file (default: a.out)"
    putStrLn "  -L, --no-link    Do not link the output object into an executable"
    putStrLn "  -v               Be verbose"
    putStrLn "  -S               Print LLVM code instead of compiling it"
    putStr "\n"
    showVersion

showVersion = putStrLn "slangc v1.0.0" >> exitSuccess

setOpt lvl xs = do
    pa <- parseArgs xs
    return pa{opt=lvl}
