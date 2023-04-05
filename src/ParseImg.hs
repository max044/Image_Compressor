module ParseImg
    ( readImg,
    ) where

import System.IO ()
import Control.Monad ()
import GHC.Base ()
import Data.List ( (\\) )

type Point = (Int, Int)
type Color = (Int, Int, Int)
type Pixel = (Point, Color)

readImg :: String -> [Pixel]
readImg s =  map readPixel (lines s)

readPixel :: String -> Pixel
readPixel a = (getPositions (head (words a) \\ "()"),
    getColors (words a !! 1 \\ "()"))

getPositions :: String -> (Int , Int)
getPositions a = tupleToTupleInt (getPosition a, getPosition2 a)

getPosition :: String -> String
getPosition a = head (words (map comaToSpace a))
getPosition2 :: String -> String
getPosition2 a = words (map comaToSpace a) !! 1

tupleToTupleInt :: (String , String) -> (Int , Int)
tupleToTupleInt (a,b) = (read a :: Int, read b :: Int)

getColors :: String -> (Int , Int, Int)
getColors a = trupleToIntTruple (getColor a, getColor2 a, getColor3 a)

getColor :: String -> String
getColor a = head (words (map comaToSpace a))
getColor2 :: String -> String
getColor2 a = words (map comaToSpace a) !! 1
getColor3 :: String -> String
getColor3 a = words (map comaToSpace a) !! 2

trupleToIntTruple :: (String , String, String ) -> (Int , Int, Int)
trupleToIntTruple (a,b,c) = (read a :: Int, read b :: Int, read c :: Int)

comaToSpace :: Char -> Char
comaToSpace ',' = ' '
comaToSpace a = a