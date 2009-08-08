
module TagSoup.Benchmark where

import Text.HTML.TagSoup
import Text.StringLike(StringLike)

import Control.Monad
import Data.List
import Data.Char
import System.CPUTime
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BS


-- figure out how many characters per second it can parse, based on a small sample
-- try it with a default of 100 repititions, figure out what a better length might be
-- and try again
-- want to measure a time of > 1 second
sample :: String
sample = "<this is a test with='attributes' and other=\"things&quot;tested\" /><neil> is </here>" ++
         "<!-- comment --> and some just random &amp; test &gt;&lt;<foo></bar><bar><bob href=no>"

pico :: Integer
pico = 1000000000000


-- FIXME: Add class Benchmark properly

stringLength x = fromIntegral $ BS.length x :: Int
stringReadFile = BS.readFile
stringRep i s = BS.concat (replicate (fromIntegral d) s) `BS.append` BS.take (fromIntegral m) s
    where (d,m) = i `divMod` stringLength s
stringPack = BS.pack
{-
stringLength x = length x
stringReadFile = readFile
stringRep i s = concat $ replicate i s
stringPack = id
-}


timefile :: FilePath -> IO ()
timefile xs = do
    s <- stringReadFile xs
    let n = stringLength s
    r <- n `seq` timeN n s
    printCps n r


time :: IO ()
time = do
        putStrLn "Timing parseTags"
        f 100
    where
        str = stringPack sample
    
        f n = do
            let s = stringRep n str
            i <- stringLength s `seq` timeN n s
            let n2 = min (n*10) (abs $ floor $ (fromIntegral n*11) / (i*10))
            if i > 1
                then printCps n i
                else f n2


printCps :: Int -> Double -> IO ()
printCps n i = putStrLn $ "parseTags = " ++ showUnit (floor cps) ++ " characters/second"
    where cps = fromIntegral n / i


-- number of repetitions, time in seconds
timeN :: StringLike s => Int -> s -> IO Double
timeN n str = do
    hSetBuffering stdout NoBuffering
    putStr $ show n ++ " repetitions = "
    start <- getCPUTime
    let res = parseTags str
    () <- length res `seq` return ()
    end <- getCPUTime
    let t = fromInteger (1 + end - start) / fromInteger pico
    putStrLn $ show t ++ " seconds"
    return t


showUnit :: Integer -> String
showUnit x = num ++ unit
    where
        units = " KMGTPEZY"
        (use,skip) = splitAt 3 $ show x

        unit = [units !! ((length skip + 2) `div` 3)]

        dot = ((length skip - 1) `mod` 3) + 1
        num = a ++ ['.' | b /= ""] ++ b
            where (a,b) = splitAt dot use
