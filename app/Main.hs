module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Text.Megaparsec.Error (errorBundlePretty)
import Control.Exception (catch)
import System.IO (hPutStrLn, stderr)

import Parsing (parseProgram, ParseResult(..))
import Analysis (runAnalysis, testAnalysis)
import Pretty (pretty)

-- | Read file with error handling
readFileWithError :: FilePath -> IO String
readFileWithError path = catch (readFile path) handleError
  where
    handleError :: IOError -> IO String
    handleError e = do
        hPutStrLn stderr $ "\ESC[31mError reading file " ++ path ++ ":\ESC[0m"
        hPutStrLn stderr $ "  " ++ show e
        exitFailure

-- | Error formatting
printError :: String -> IO ()
printError msg = do
    hPutStrLn stderr $ "\ESC[31mError:\ESC[0m " ++ msg
    exitFailure

-- | Main program
main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            putStrLn $ "\ESC[34mParsing file: " ++ filename ++ "\ESC[0m"
            contents <- readFileWithError filename
            case parseProgram filename contents of
                Left err -> do
                    hPutStrLn stderr "\ESC[31mParse error:\ESC[0m"
                    hPutStrLn stderr $ errorBundlePretty err
                    exitFailure
                Right parseResult -> do
                    putStrLn "\ESC[32mSuccessfully parsed:\ESC[0m"
                    let preProgram = pprogram parseResult
                    putStrLn $ pretty preProgram 

                    -- Run analysis
                    putStrLn "\n\ESC[34mRunning analysis:\ESC[0m"
                    testAnalysis parseResult

                    putStrLn "\ESC[32mDone!\ESC[0m"
        _ -> do
            printError "Usage: program <filename>"