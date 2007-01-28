-- Original version by Alistair Bayley
-- Much help from Daniel McAllansmith
-- Taken from Haskell-Cafe mailing list
-- "Simple HTTP lib for Windows?", 18 Jan 2007

-- Does not seem very reliable, does not work for various websites (including Google)

module Data.Html.Download(openURL) where

import System.IO
import Network
import Data.List

-- | http:// prefix is ignored
--   www.haskell.org/haskellwiki/Haskell
openURL :: String -> IO String
openURL url | "http://" `isPrefixOf` url = openURL (drop 7 url)
openURL url = client server 80 (if null path then "/" else path)
    where (server,path) = break (== '/') url


client :: [Char] -> PortNumber -> [Char] -> IO String
client server port page = withSocketsDo $ do
    hndl <- connectTo server (PortNumber port)
    let out x = hPutStrLn hndl (x ++ "\r")
    hSetBuffering hndl NoBuffering

    out $ "GET " ++ page ++ " HTTP/1.1"
    out $ "Host: " ++ server ++ ""
    out $ "Connection: close"
    out ""
    out ""
    readResponse hndl


readResponse :: Handle -> IO String
readResponse hndl = do
    closed <- hIsClosed hndl
    eof <- hIsEOF hndl
    if closed || eof
        then return []
        else do
            c <- hGetChar hndl
            cs <- readResponse hndl
            return (c:cs)
