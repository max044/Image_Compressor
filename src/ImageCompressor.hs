module ImageCompressor
    ( workingIMGC, kMeanAlgo,
    ) where

import System.Environment ()
import System.Exit
import System.Random
import Text.Printf
import Data.List
import Data.Maybe
import ParseImg ( readImg )

type Point = (Int, Int)
type Color = (Int, Int, Int)
type Pixel = (Point, Color)
type Cluster = (Pixel, [Pixel])

workingIMGC :: (String, Int, Float) -> [Pixel]
workingIMGC (s, i, f) = readImg s

clearClusters :: [Cluster] -> [Cluster]
clearClusters [] = []
clearClusters ((a,b):xs) = (a, []) : clearClusters xs

getSomme :: [Pixel] -> Int -> Int -> Int -> Int -> Color
getSomme [] r2 g2 b2 0 = (r2, g2 , b2)
getSomme [] r2 g2 b2 le = (r2 `div` le, g2 `div` le, b2 `div` le)
getSomme [(a,(r,g,b))] r2 g2 b2 le = getSomme [] (r+r2) (g+g2) (b+b2) le
getSomme ((a,(r,g,b)):xs) r2 g2 b2 le = getSomme xs (r+r2) (g+g2) (b+b2) le

calcMean :: [Cluster] -> [Cluster]
calcMean [] = []
calcMean (((a,(r,g,b)), c):xs) =
    ((a,getSomme c 0 0 0 (length c)), c): calcMean xs

getMeanConv :: [Cluster] -> [Cluster] -> [Float]
getMeanConv [] _ = []
getMeanConv (((a,color1), c):xs) (((a2,color2), c2):xs2) =
    getDistance color1 color2 : getMeanConv xs xs2

mydiv :: Float -> Int -> Float
mydiv x y =
    if b == 0
        then 1
    else x / b where
    b = fromIntegral y :: Float
    

myDivAll :: [Float] -> Int -> [Float]
myDivAll [] _ = []
myDivAll (x:xs) i = mydiv x i : myDivAll xs i

algo :: [Pixel] -> [Cluster] -> Float -> [Cluster]
algo pixel cluster conv =
    if a < conv
        then newClust
    else algo pixel (clearClusters newClust) conv where
    clustersFilled = fillClusters pixel cluster
    newClust = calcMean clustersFilled
    mean = myDivAll (getMeanConv clustersFilled newClust) (length newClust)
    a = mydiv (sum mean) (length mean)

kMeanAlgo :: [Pixel] -> Int -> Float -> IO ()
kMeanAlgo s n convergence = do
    seed <- newStdGen
    let rands = take n (randomRs (0, length s-1) seed :: [Int])
    let clusters = distribInCluster s rands []
    let clustersFilled = fillClusters s clusters
    let res = algo s clustersFilled convergence
    printClusters res

-- calculer la moyenne des clusters (centroids)
-- checker si la moyenne entre ancienne pos et new pos est < convergence
-- si oui => stop algo
-- si non => clear les pixels des clusters ++ fill les clusters ++ recommencer

-- create n clusters with n randoms values
distribInCluster:: [Pixel] -> [Int] -> [Cluster] -> [Cluster]
distribInCluster p [] c = c
distribInCluster p (x:xs) c = distribInCluster p xs ((pixel, []):c) where
    pixel = p !! x

-- get colors of clusters
getClustColors :: [Cluster] -> [Color] -> [Color]
getClustColors [] colors = colors
getClustColors (((pos, col), list):xs) colors = getClustColors xs (col:colors)

setCluster :: Pixel -> [Cluster] -> Int -> [Cluster]
setCluster x l pos = a ++ [(e, x : f)] ++ d where
    (a,b) = splitAt pos l
    (c,d) = splitAt 1 b
    [(e,f)] = c

getDistance :: Color -> Color -> Float
getDistance (r1,g1,b1) (r2,g2,b2) = sqrt (fromIntegral (r*r + g*g + b*b))
    where
        r = r1-r2
        g = g1-g2
        b = b1-b2

getDistList :: Color -> [Cluster] -> [Float]
getDistList _ [] = []
getDistList c (((p,a), co):cs) =  getDistance c a : getDistList c cs

mymin :: Float -> Float -> Float
mymin a b
    | a > b  = b
    | a < b  = a
    | a == b = a

minim :: [Float] -> Int -> Float
minim [] _ = 0
minim [x] pos = x
minim (x:xs) pos =  mymin x (minim xs (pos + 1))

getCluster :: Pixel -> [Cluster] -> Int
getCluster (p,c) cl  = posi where
    distList = getDistList c cl
    elem = minimum distList
    posi = Data.Maybe.fromMaybe 0 (elemIndex elem distList)

fillClusters :: [Pixel] -> [Cluster] -> [Cluster]
fillClusters [] c = c
fillClusters (x:xs) (c:cs) =
    fillClusters xs (setCluster x (c:cs) (getCluster x (c:cs)))

printPixel :: [Pixel] -> IO ()
printPixel [] = return ()
printPixel (((x, y), (r, g, b)):xs) =
    printf "(%d,%d) (%d,%d,%d)\n" x y r g b >>
    printPixel xs

printClusters :: [Cluster] -> IO ()
printClusters [] = return ()
printClusters (((cpos, (r, g, b)), pixels):xs) =
    printf "--\n(%d,%d,%d)\n-\n" r g b >>
    printPixel pixels >>
    printClusters xs