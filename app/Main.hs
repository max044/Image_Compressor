module Main where

import System.Environment ( getArgs )
import Control.Exception ( handle )
import System.Exit ( exitWith, ExitCode(ExitSuccess) )

import ImageCompressor
import ErrorManager ( errorHandler )
import ArgManager

main :: IO ()
main = handle errorHandler $ do
  args <- getArgs
  optionsList <- parser args
  case optionsList of
    Right opt -> do
      file <- readFile (pathImage opt)
      let pixelList = workingIMGC (file, nbColors opt, convLim opt)
      kMeanAlgo pixelList (nbColors opt) (convLim opt)
    _ -> programUsage

programUsage :: IO ()
programUsage = putStrLn "USAGE: ./imageCompressor -n N -l L -f F\n\n\n\
\      N       number of colors in the final image\n\
\      L       convergence limit\n\
\      F       path to the file containing the colors of the pixels"