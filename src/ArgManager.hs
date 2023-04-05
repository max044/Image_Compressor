module ArgManager (ArgumentType(..), Options(..), parser) where

import ErrorManager
import Control.Exception

data Options = Options
    { nbColors          :: Int
    , convLim  :: Float
    , pathImage         :: String
    } deriving Show

data ArgumentType =     Invalid
                    |   Helper
                    |   Other
    deriving (Show, Enum)

parser :: [String] -> IO (Either ArgumentType Options)
parser args = case parseArgument args of
                Right t -> return $ Right t
                Left Helper -> return $ Left Helper
                Left _ -> throw $ ArgError "invalid argument"

parseArgument :: [String] -> Either ArgumentType Options
parseArgument ["--help"]    = Left  Helper
parseArgument ["-n", n, "-l", l, "-f", f] = Right Options
    { nbColors = read n :: Int, convLim = read l :: Float, pathImage = f }
parseArgument ["-n", n, "-f", f, "-l", l] = Right Options
    { nbColors = read n :: Int, pathImage = f, convLim = read l :: Float }
parseArgument ["-l", l, "-n", n, "-f", f] = Right Options
    { convLim = read l :: Float, nbColors = read n :: Int, pathImage = f }
parseArgument ["-l", l, "-f", f, "-n", n] = Right Options
    { convLim = read l :: Float, pathImage = f, nbColors = read n :: Int }
parseArgument ["-f", f, "-l", l, "-n", n] = Right Options
    { pathImage = f, convLim = read l :: Float, nbColors = read n :: Int }
parseArgument ["-f", f, "-n", n, "-l", l] = Right Options
    { pathImage = f, nbColors = read n :: Int, convLim = read l :: Float }
parseArgument _             = Left  Invalid