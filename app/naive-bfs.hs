module Main (main) where

-- import Lib

import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            _ <- readFile filename
            putStrLn "done"
        _ -> do
            die "Usage: stack run <filename>"

