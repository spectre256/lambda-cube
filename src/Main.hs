module Main where

import Parser
import Interpreter

import Data.Text (pack)
import System.Console.Haskeline
import Control.Monad.Trans

type App = InterpretT (InputT IO)

runApp :: App () -> IO ()
runApp = runInputT defaultSettings . runInterpretT

main :: IO ()
main = runApp loop
    where
        loop :: App ()
        loop = do
            minput <- lift $ getInputLine "λ> "
            case minput of
                Nothing -> loop
                Just "" -> loop
                Just line -> accept line
                    >>= lift . outputStrLn  >> loop

        accept :: Monad m => String -> InterpretT m String
        accept line = case parse (pack line) of
            Nothing -> return "Bad syntax, idiot"
            Just ast -> show <$> exec ast
