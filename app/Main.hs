module Main where

import           Options.Applicative

import SchemaLib
import Sheet


main :: IO ()
main = do
    options <- execParser opts
    print options
    contents <- readFile $ filename options
    putStrLn contents
    putStrLn $ handleText contents options
