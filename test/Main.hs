
module Main(main) where

import System.Environment
import TagSoup.Sample
import TagSoup.Test
import TagSoup.Benchmark
import Data.Char(toLower)


helpMsg :: IO ()
helpMsg = putStr $ unlines $
    ["TagSoup, (C) Neil Mitchell 2006-2009"
    ,""
    ,"  tagsoup arguments"
    ,""
    ,"<url> may either be a local file, or a http[s]:// page"
    ,""
    ] ++ map f res
    where
        width = maximum $ map (length . fst) res
        res = map g actions

        g (nam,msg,Left  _) = (nam,msg)
        g (nam,msg,Right _) = (nam ++ " <url>",msg)

        f (lhs,rhs) = "  " ++ lhs ++ replicate (4 + width - length lhs) ' ' ++ rhs


actions :: [(String, String, Either (IO ()) (String -> IO ()))]
actions = [("test","Run the test suite",Left test)
          ,("grab","Grab a web page",Right grab)
          ,("parse","Parse a web page",Right parse)
          ,("bench","Benchmark the parsing",Left time)
          ,("benchfile","Benchmark the parsing of a file",Right timefile)
          ,("validate","Validate a page",Right validate)
          ,("lastmodifieddate","Get the wiki.haskell.org last modified date",Left haskellLastModifiedDateTime)
          ,("spj","Simon Peyton Jones' papers",Left spjPapers)
          ,("ndm","Neil Mitchell's papers",Left ndmPapers)
          ,("time","Current time",Left currentTime)
          ,("google","Google Tech News",Left googleTechNews)
          ,("sequence","Creators on sequence.complete.org",Left rssCreators)
          ,("table","Parse a table",Left $ print parseTable)
          ,("help","This help message",Left helpMsg)
          ]

main :: IO ()
main = do
    args <- getArgs
    case (args, lookup (map toLower $ head args) $ map (\(a,_,c) -> (a,c)) actions) of
        ([],_) -> do
            putStrLn "No arguments specifying, defaulting to test"
            helpMsg
            putStrLn $ replicate 70 '-'
            test
        (x:_,Nothing) -> putStrLn ("Error: unknown command " ++ x) >> helpMsg
        ([_],Just (Left a)) -> a
        (x:xs,Just (Left a)) -> do
            putStrLn $ "Warning: expected no arguments to " ++ x ++ " but got: " ++ unwords xs
            a
        ([_,y],Just (Right a)) -> a y
        (x:xs,Just (Right _)) -> do
            putStrLn $ "Error: expected exactly one argument to " ++ x ++ " but got: " ++ unwords xs
            helpMsg
