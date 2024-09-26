module Main where

import Parser
import Interpreter

import Data.Text (pack)
import Control.Monad.Trans
import Data.Char (isSpace)
import System.Console.Haskeline
import System.Directory
import System.FilePath ((</>))

type App = InterpretT (InputT IO)

historyPath :: IO FilePath
historyPath = do
    configDir <- getXdgDirectory XdgConfig "lambda"
    createDirectoryIfMissing True configDir
    return (configDir </> "history")

runApp :: App () -> IO ()
runApp app = do
    path <- historyPath
    runInputT (settings path) $ runInterpretT app
    where
        settings path = Settings
            { complete = noCompletion
            , historyFile = Just path
            , autoAddHistory = True }

main :: IO ()
main = runApp loop
    where
        loop :: App ()
        loop = do
            minput <- lift $ getInputLine "λ> "
            case minput of
                Nothing -> loop
                Just line | all isSpace line -> loop
                Just line -> accept line
                    >>= lift . outputStrLn  >> loop

        accept :: Monad m => String -> InterpretT m String
        accept line = case parse (pack line) of
            Nothing -> return "Bad syntax, idiot"
            Just ast -> exec ast
