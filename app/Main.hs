module Main where

import System.Environment (getArgs)

import SchemaLib
import Sheet

getFirstOrDefault :: [String] -> String
getFirstOrDefault (filename : _rest) = filename
getFirstOrDefault [] = "data/fixed_width.txt"



main :: IO ()
main = do
    args <- getArgs
    contents <- readFile $ getFirstOrDefault args
    putStrLn contents
    putStrLn $ handleText contents
