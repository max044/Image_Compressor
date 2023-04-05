module ErrorManager (
    errorHandler,
    ExceptionType (..)
    ) where

import Control.Exception
import System.Exit
import System.Environment
import System.IO

data ExceptionType = ArgError String | ExecError String deriving (Show)

instance Exception ExceptionType

exitWithErrorMessage :: String -> IO a
exitWithErrorMessage str = hPutStrLn stderr str >> exitWith (ExitFailure 84)

errorHandler :: ExceptionType -> IO ()
errorHandler (ArgError s) = exitWithErrorMessage s
errorHandler (ExecError s) = exitWithErrorMessage s