module Main where

import Lib
import System.Environment (getArgs)
import Data.Bool (bool)
import Data.Text (pack)

main :: IO ()
main = do
    arguments <- getArgs
    let (output:prefix:suffix:_) = take 3 $ arguments ++ repeat ""
    bool (plot output prefix suffix) printHelp (output == "")

    where
        plot output prefix suffix = plotGlueDags output (pack prefix) (pack suffix)

printHelp :: IO ()
printHelp = do
    putStrLn "glue-diagram plots dependency diagrams of AWS Glue triggers and jobs"
    putStrLn ""
    putStrLn "Usage: glue-diagram file [prefix [suffix]]"
    putStrLn ""
    putStrLn "file - path to the output PNG file"
    putStrLn "prefix - analyze only triggers starting with this prefix"
    putStrLn "suffix - analyze only triggers ending with this suffix"
    putStrLn ""
    putStrLn "Example: glue-diagram dev.png 'my-subproject-' '-development'"
