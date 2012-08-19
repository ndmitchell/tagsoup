{-# LANGUAGE CPP #-}

module TagSoup.Benchmark where

import Text.HTML.TagSoup

import Control.DeepSeq
import Control.Monad
import Data.List
import Data.Maybe
import System.IO.Unsafe(unsafeInterleaveIO)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Time.Clock.POSIX(getPOSIXTime)

conf = 0.95


timefile :: FilePath -> IO ()
timefile file = do
    -- use LBS to be most representative of real life
    lbs <- LBS.readFile file
    let str = LBS.unpack lbs
        bs = BS.concat $ LBS.toChunks lbs
    () <- LBS.length lbs `seq` length str `seq` BS.length bs `seq` return ()
    benchWith (const str, const bs, const lbs) $ benchStatic (toInteger $ LBS.length lbs)


sample :: String
sample = "<this is a test with='attributes' and other=\"things&quot;tested\" /><neil> is </here>" ++
         "<!-- comment --> and some just random &amp; test &gt;&lt;<foo></bar><bar><bob href=no>"

nsample = genericLength sample :: Integer

time :: IO ()
time = benchWith (str,bs,lbs) benchVariable
    where
        str = \i -> concat $ genericReplicate i sample
        bs  = let s = BS.pack sample in \i -> BS.concat (genericReplicate i s)
        lbs = let s = LBS.pack sample in \i -> LBS.concat (genericReplicate i s)



benchWith :: (Integer -> String, Integer -> BS.ByteString, Integer -> LBS.ByteString)
          -> ((Integer -> ()) -> IO [String]) -> IO ()
benchWith (str,bs,lbs) bench = do
        putStrLn "Timing parseTags in characters/second"
        let header = map (:[]) ["(" ++ show (round $ conf * 100) ++ "% confidence)","String","BS","LBS"]
        rows <- mapM row $ replicateM 3 [False,True]
        mapM_ (putStrLn . strict . grid) $ delay2 $ header : rows
    where
        row [a,b,c] = do
            let header = intercalate "," [g a "pos", g b "warn", g c "merge"]
                g b x = (if b then ' ' else '!') : x
                f x = bench $ \i -> rnf $ parseTagsOptions parseOptions{optTagPosition=a,optTagWarning=b,optTagTextMerge=c} $ x i
            c1 <- f str
            c2 <- f bs
            c3 <- f lbs
            return [[header],c1,c2,c3]

        strict = reverse . reverse


---------------------------------------------------------------------
-- BENCHMARK ON THE SAMPLE INPUT

disp xs = showUnit (floor xbar) ++ " (~" ++ rng ++ "%)"
    where xbar = mean xs
          rng = if length xs <= 1 then "?" else show (ceiling $ (range conf xs) * 100 / xbar) 

cons x = fmap (x:)


aimTime = 0.3 :: Double -- seconds to aim for
minTime = 0.2 :: Double -- below this a test is considered invalid


-- given a number of times to repeat sample, return a list of what
-- to display
benchVariable :: (Integer -> ()) -> IO [String]
benchVariable op = cons "?" $ f 10 []
    where
        f i seen | length seen > 9 = cons ("  " ++ disp seen) $ return []
                 | otherwise = unsafeInterleaveIO $ do
            now <- timer $ op i
            let cps = if now == 0 then 0 else fromInteger (i * nsample) / now
            if now < minTime || (null seen && now < aimTime) then do
                let factor = min 7 $ max 2 $ floor $ aimTime / now
                cons ("? " ++ disp [cps]) $ f (i * factor) []
             else
                cons (show (9 - length seen) ++ " " ++ disp (cps:seen)) $ f i (cps:seen)



benchStatic :: Integer -> (Integer -> ()) -> IO [String]
benchStatic nsample op = cons "?" $ f []
    where
        f seen | length seen > 9 = cons ("  " ++ disp seen) $ return []
               | otherwise = unsafeInterleaveIO $ do
            now <- timer $ op $ genericLength seen
            let cps = if now == 0 then 0 else fromInteger nsample / now
            cons (show (9 - length seen) ++ " " ++ disp (cps:seen)) $ f (cps:seen)


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


-- copied from the criterion package
getTime :: IO Double
getTime = (fromRational . toRational) `fmap` getPOSIXTime

timer :: () -> IO Double
timer x = do
    start <- getTime
    () <- return x
    end <- getTime
    return $ end - start


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


---------------------------------------------------------------------
-- INSTANCES

instance NFData a => NFData (Tag a) where
    rnf (TagOpen x y) = rnf x `seq` rnf y
    rnf (TagClose x) = rnf x
    rnf (TagText x) = rnf x
    rnf (TagComment x) = rnf x
    rnf (TagWarning x) = rnf x
    rnf (TagPosition x y) = () -- both are already ! bound


#ifndef BYTESTRING_HAS_NFDATA
# ifdef MIN_VERSION_bytestring
#  define BYTESTRING_HAS_NFDATA (MIN_VERSION_bytestring(0,10,0))
# else
#  define BYTESTRING_HAS_NFDATA (__GLASGOW_HASKELL__ >= 706)
# endif
#endif

#if !BYTESTRING_HAS_NFDATA
instance NFData LBS.ByteString where
    rnf x = LBS.length x `seq` ()

instance NFData BS.ByteString where
    rnf x = BS.length x `seq` ()
#endif


---------------------------------------------------------------------
-- STATISTICS
-- Provided by Emily Mitchell

confNs = let (*) = (,) in
    [0.95 * 1.96
    ,0.90 * 1.644]

size :: [Double] -> Double
size = genericLength

mean :: [Double] -> Double
mean xs = sum xs / size xs

stddev :: [Double] -> Double
stddev xs = sqrt $ sum [sqr (x - xbar) | x <- xs] / size xs
    where xbar = mean xs
          sqr x = x * x

-- given a sample, and a required confidence
-- of the mean (i.e. 2.5% = 0.025)
range ::Double -> [Double] -> Double
range conf xs = conf2 * stddev xs / sqrt (size xs)
    where conf2 = fromMaybe (error $ "Unknown confidence interval: " ++ show conf) $ lookup conf confNs
