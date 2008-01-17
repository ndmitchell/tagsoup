
module Example.Example where

import Text.HTML.TagSoup
import Text.HTML.Download

import Control.Monad
import Data.List
import Data.Char


grab :: String -> IO ()
grab x = openItem x >>= putStr

parse :: String -> IO ()
parse x = openItem x >>= putStr . show2 . parseTags
    where
        show2 [] = "[]"
        show2 xs = "[" ++ concat (intersperseNotBroken "\n," $ map show xs) ++ "\n]"


-- the standard intersperse has a strictness bug which sucks!
intersperseNotBroken :: a -> [a] -> [a]
intersperseNotBroken _ [] = []
intersperseNotBroken sep (x:xs) = x : is xs
    where
        is [] = []
        is (y:ys) = sep : y : is ys


{-
<div class="printfooter">
<p>Retrieved from "<a href="http://haskell.org/haskellwiki/Haskell">http://haskell.org/haskellwiki/Haskell</a>"</p>

<p>This page has been accessed 507,753 times. This page was last modified 08:05, 24 January 2007. Recent content is available under <a href="/haskellwiki/HaskellWiki:Copyrights" title="HaskellWiki:Copyrights">a simple permissive license</a>.</p>
</div>
-}
haskellHitCount :: IO ()
haskellHitCount = do
        tags <- liftM parseTags $ openURL "http://haskell.org/haskellwiki/Haskell"
        let count = fromFooter $ head $ sections (~== "<div class=printfooter>") tags
        putStrLn $ "haskell.org has been hit " ++ show count ++ " times"
    where
        fromFooter x = read (filter isDigit num) :: Int
            where
                num = ss !! (i - 1)
                Just i = findIndex (== "times.") ss
                ss = words s
                TagText s = sections (~== "<p>") x !! 1 !! 1


{-
<a href="http://www.cbc.ca/technology/story/2007/04/10/tech-bloggers.html" id=r-5_1115205181>
<b>Blogger code of conduct proposed</b>
-}
googleTechNews :: IO ()
googleTechNews = do
        tags <- liftM parseTags $ openURL "http://news.google.com/?ned=us&topic=t"
        let links = [ x
                    | TagOpen "a" atts:_:TagText x:_ <- tails tags
                    , ("id",'r':val) <- atts, 'i' `notElem` val]
        putStr $ unlines links


spjPapers :: IO ()
spjPapers = do
        tags <- liftM parseTags $ openURL "http://research.microsoft.com/~simonpj/"
        let links = map f $ sections (~== "<a>") $
                    takeWhile (~/= "<a name=haskell>") $
                    drop 5 $ dropWhile (~/= "<a name=current>") tags
        putStr $ unlines links
    where
        f :: [Tag] -> String
        f = dequote . unwords . words . fromTagText . head . filter isTagText

        dequote ('\"':xs) | last xs == '\"' = init xs
        dequote x = x


ndmPapers :: IO ()
ndmPapers = do
        tags <- liftM parseTags $ openURL "http://www-users.cs.york.ac.uk/~ndm/downloads/"
        let papers = map f $ sections (~== "<li class=paper>") tags
        putStr $ unlines papers
    where
        f :: [Tag] -> String
        f xs = fromTagText (xs !! 2)


currentTime :: IO ()
currentTime = do
        tags <- liftM parseTags $ openURL "http://www.timeanddate.com/worldclock/city.html?n=136"
        let time = fromTagText (dropWhile (~/= "<strong id=ct>") tags !! 1)
        putStrLn time



type Section = String
data Package = Package {name :: String, desc :: String, href :: String}
               deriving Show

hackage :: IO [(Section,[Package])]
hackage = do
    tags <- liftM parseTags $ openURL "http://hackage.haskell.org/packages/archive/pkg-list.html"
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
    tags <- liftM parseTags $ openURL "http://sequence.complete.org/node/feed"
    putStrLn $ unlines $ map names $ partitions (~== "<dc:creator>") tags
    where
      names xs = fromTagText $ xs !! 1


validate :: String -> IO ()
validate x = putStr . unlines . g . f . parseTagsOptions opts =<< openItem x
    where
        opts = parseOptions{optTagPosition=True, optTagWarning=True}

        f :: [Tag] -> [String]
        f (TagPosition row col:TagWarning warn:rest) =
            ("Warning (" ++ show row ++ "," ++ show col ++ "): " ++ warn) : f rest
        f (TagWarning warn:rest) =
            ("Warning (?,?): " ++ warn) : f rest
        f (_:rest) = f rest
        f [] = []

        g xs = xs ++ [if n == 0 then "Success, no warnings"
                      else "Failed, " ++ show n ++ " warning" ++ ['s'|n>1]]
            where n = length xs
