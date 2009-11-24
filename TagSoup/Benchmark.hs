
module TagSoup.Benchmark where

import Text.HTML.TagSoup

import Control.Monad
import Data.List
import Data.Char
import System.CPUTime
import System.IO
import System.IO.Unsafe(unsafeInterleaveIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Time.Clock.POSIX(getPOSIXTime)


timefile :: FilePath -> IO ()
timefile xs = error "todo"


sample :: String
sample = "<this is a test with='attributes' and other=\"things&quot;tested\" /><neil> is </here>" ++
         "<!-- comment --> and some just random &amp; test &gt;&lt;<foo></bar><bar><bob href=no>"

nsample = genericLength sample :: Integer


time :: IO ()
time = do
        putStrLn "Timing parseTags in characters/second"
        let header = map (:[]) ["","String","BS","LBS"]
        rows <- mapM row $ replicateM 3 [False,True]
        mapM_ (putStrLn . strict . grid) $ delay2 $ header : rows
    where
        row [a,b,c] = do
            let header = intercalate "," [g a "pos", g b "warn", g c "merge"]
                g b x = (if b then ' ' else '!') : x
                f x = bench $ \i -> length (parseTagsOptions parseOptions{optTagPosition=False,optTagWarning=False,optTagTextMerge=True} $ x i) `seq` ()
            c1 <- f $ \i -> concat $ genericReplicate i sample
            c2 <- let s = BS.pack sample in f $ \i -> BS.concat (genericReplicate i s)
            c3 <- let s = LBS.pack sample in f $ \i -> LBS.concat (genericReplicate i s)
            return [[header],c1,c2,c3]

        strict = reverse . reverse


---------------------------------------------------------------------
-- BENCHMARK ON THE SAMPLE INPUT

aimTime = 0.3 :: Double -- seconds to aim for
minTime = 0.2 :: Double -- below this a test is considered invalid


-- given a number of times to repeat sample, return a list of what
-- to display
bench :: (Integer -> ()) -> IO [String]
bench op = cons "?" $ f 10 []
    where
        f i seen | length seen > 9 = cons ("  " ++ disp seen) $ return []
                 | otherwise = unsafeInterleaveIO $ do
            now <- timer $ op i
            let cps = if now == 0 then 0 else round $ fromInteger (i * nsample) / now
            if now < minTime || (null seen && now < aimTime) then do
                let factor = min 7 $ max 2 $ floor $ aimTime / now
                cons ("? " ++ showUnit cps) $ f (i * factor) []
             else
                cons (show (9 - length seen) ++ " " ++ disp (cps:seen)) $ f i (cps:seen)

        disp = showUnit . summarise
        cons x = fmap (x:)


---------------------------------------------------------------------
-- UTILITY FUNCTIONS

-- | Given a number, show it using a unit and decimal place
showUnit :: Integer -> String
showUnit x = num ++ unit
    where
        units = " KMGTPEZY"
        (use,skip) = splitAt 3 $ show x

        unit = [units !! ((length skip + 2) `div` 3)]

        dot = ((length skip - 1) `mod` 3) + 1
        num = a ++ ['.' | b /= ""] ++ b
            where (a,b) = splitAt dot use


pico :: Integer
pico = 1000000000000


-- copied from the criterion package
getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime

timer :: () -> IO Double
timer x = do
    start <- getTime
    () <- return x
    end <- getTime
    return $ end - start -- fromInteger (1 + end - start) / fromInteger pico


summarise :: [Integer] -> Integer
summarise xs = sum ys `div` genericLength ys
    where ys = take (max 3 $ (length xs + 1) `div` 2) $ sort xs


-- display a grid
grid :: [[String]] -> String
grid xs = unlines $ map (concat . zipWith f cols) xs
    where cols = map (maximum . map length) $ transpose xs
          f n x = x ++ replicate (n+1 - length x) ' '


-- display a series of grids over time
-- when a grid gets to [] keep its value at that
-- when all grids get to [] return []
delay2 :: [[[String]]] -> [[[String]]]
delay2 xs = map (map head) xs : (if all (null . tail) (concat xs) then [] else delay2 $ map (map tl) xs)
    where tl (x:xs) = if null xs then x:xs else xs
