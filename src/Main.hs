module Main where

import Parser
import Interpreter

import Data.Text (pack)
import System.Console.Haskeline

main :: IO ()
main = runInputT defaultSettings loop
    where
        loop = do
            minput <- getInputLine "λ> "
            case minput of
                Nothing -> loop
                Just line -> accept (pack line) >> loop
        accept line = case parse line of
            Nothing -> outputStrLn "Bad syntax, idiot"
            Just ast -> outputStrLn . show $ interpret ast
