{-|
    Module      :  Text.HTML.Download
    Copyright   :  (c) Neil Mitchell 2006-2007
    License     :  BSD-style

    Maintainer  :  http://www.cs.york.ac.uk/~ndm/
    Stability   :  unstable
    Portability :  portable

    This module simply downloads a page off the internet. It is very restricted,
    and it not intended for proper use. The primary purpose is to allow more
    interesting examples for the "Data.Html.TagSoup" module.
    
    The original version was by Alistair Bayley, with additional help from
    Daniel McAllansmith. It is taken from the Haskell-Cafe mailing list
    \"Simple HTTP lib for Windows?\", 18 Jan 2007.
    <http://thread.gmane.org/gmane.comp.lang.haskell.cafe/18443/>
-}

module Text.HTML.Download(openURL) where

import System.IO
import Network
import Data.List

-- | This function opens a URL on the internet.
--   Any @http:\/\/@ prefix is ignored.
--
-- > openURL "www.haskell.org/haskellwiki/Haskell"
--
-- Known Limitations:
--
-- * Only HTTP on port 80
--
-- * Outputs the HTTP Headers as well
--
-- * Does not work with all servers
--
-- It is hoped that a more reliable version of this function will be
-- placed in a new HTTP library at some point!
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
