
module TagSoup.Sample where

import Text.HTML.TagSoup

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Cmd
import System.Directory
import System.Exit
import System.IO


openItem :: String -> IO String
openItem url | not $ "http://" `isPrefixOf` url = readFile url
openItem url = bracket
    (openTempFile "." "tagsoup.tmp")
    (\(file,hndl) -> removeFile file)
    $ \(file,hndl) -> do
        hClose hndl
        putStrLn $ "Downloading: " ++ url
        res <- system $ "wget " ++ url ++ " -O " ++ file
        when (res /= ExitSuccess) $ error $ "Failed to download using wget: " ++ url
        src <- readFile file
        length src `seq` return src


grab :: String -> IO ()
grab x = openItem x >>= putStr

parse :: String -> IO ()
parse x = openItem x >>= putStr . show2 . parseTags
    where
        show2 [] = "[]"
        show2 xs = "[" ++ concat (intersperseNotBroken "\n," $ map show xs) ++ "\n]\n"


-- the standard intersperse has a strictness bug which sucks!
intersperseNotBroken :: a -> [a] -> [a]
intersperseNotBroken _ [] = []
intersperseNotBroken sep (x:xs) = x : is xs
    where
        is [] = []
        is (y:ys) = sep : y : is ys


{-
<li id="viewcount">This page has been accessed 6,985,922 times.</li>
-}
haskellHitCount = do
    src <- openItem "http://haskell.org/haskellwiki/Haskell"
    let count = fromFooter $ parseTags src
    putStrLn $ "haskell.org has been hit " ++ count ++ " times"
    where fromFooter = filter isDigit . innerText . take 2 . dropWhile (~/= "<li id=viewcount>")


googleTechNews :: IO ()
googleTechNews = do
        tags <- fmap parseTags $ openItem "http://news.google.com/?ned=us&topic=t"
        let links = [ ascii name ++ " <" ++ maybe "unknown" shortUrl (lookup "href" atts) ++ ">"
                    | TagOpen "h2" [("class","title")]:TagText spaces:TagOpen "a" atts:TagText name:_ <- tails tags]
        putStr $ unlines links
    where
        shortUrl x | "http://" `isPrefixOf` x = shortUrl $ drop 7 x
                   | "www." `isPrefixOf` x = shortUrl $ drop 4 x
                   | otherwise = takeWhile (/= '/') x

        ascii ('\226':'\128':'\147':xs) = '-' : ascii xs
        ascii ('\194':'\163':xs) = "#GBP " ++ ascii xs
        ascii (x:xs) = x : ascii xs
        ascii [] = []


spjPapers :: IO ()
spjPapers = do
        tags <- fmap parseTags $ openItem "http://research.microsoft.com/en-us/people/simonpj/"
        let links = map f $ sections (~== "<A>") $
                    takeWhile (~/= "<a name=haskell>") $
                    drop 5 $ dropWhile (~/= "<a name=current>") tags
        putStr $ unlines links
    where
        f :: [Tag String] -> String
        f = dequote . unwords . words . fromTagText . head . filter isTagText

        dequote ('\"':xs) | last xs == '\"' = init xs
        dequote x = x


ndmPapers :: IO ()
ndmPapers = do
        tags <- fmap parseTags $ openItem "http://community.haskell.org/~ndm/downloads/"
        let papers = map f $ sections (~== "<li class=paper>") tags
        putStr $ unlines papers
    where
        f :: [Tag String] -> String
        f xs = fromTagText (xs !! 2)


currentTime :: IO ()
currentTime = do
        tags <- fmap parseTags $ openItem "http://www.timeanddate.com/worldclock/city.html?n=136"
        let res = fromTagText (dropWhile (~/= "<strong id=ct>") tags !! 1)
        putStrLn res



type Section = String
data Package = Package {name :: String, desc :: String, href :: String}
               deriving Show

hackage :: IO [(Section,[Package])]
hackage = do
    tags <- fmap parseTags $ openItem "http://hackage.haskell.org/packages/archive/pkg-list.html"
    return $ map parseSect $ partitions (~== "<h3>") tags
    where
        parseSect xs = (nam, packs)
            where
                nam = fromTagText $ xs !! 2
                packs = map parsePackage $ partitions (~== "<li>") xs

        parsePackage xs =
           Package
              (fromTagText $ xs !! 2)
              (drop 2 $ dropWhile (/= ':') $ fromTagText $ xs !! 4)
              (fromAttrib "href" $ xs !! 1)

-- rssCreators Example: prints names of story contributors on
-- sequence.complete.org. This content is RSS (not HTML), and the selected
-- tag uses a different XML namespace "dc:creator".
rssCreators :: IO ()
rssCreators = do
    tags <- fmap parseTags $ openItem "http://sequence.complete.org/node/feed"
    putStrLn $ unlines $ map names $ partitions (~== "<dc:creator>") tags
    where names xs = fromTagText $ xs !! 1


validate :: String -> IO ()
validate x = putStr . unlines . g . f . parseTagsOptions opts =<< openItem x
    where
        opts = parseOptions{optTagPosition=True, optTagWarning=True}

        f :: [Tag String] -> [String]
        f (TagPosition row col:TagWarning warn:rest) =
            ("Warning (" ++ show row ++ "," ++ show col ++ "): " ++ warn) : f rest
        f (TagWarning warn:rest) =
            ("Warning (?,?): " ++ warn) : f rest
        f (_:rest) = f rest
        f [] = []

        g xs = xs ++ [if n == 0 then "Success, no warnings"
                      else "Failed, " ++ show n ++ " warning" ++ ['s'|n>1]]
            where n = length xs
