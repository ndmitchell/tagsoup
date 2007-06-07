
module Example.Example where

import Text.HTML.TagSoup
import Text.HTML.Download

import qualified Text.HTML.TagSoup.Match as Match

import Control.Monad (liftM)
import Data.List (isPrefixOf, findIndex)
import Data.Char (isDigit)


{-
<div class="printfooter">
<p>Retrieved from "<a href="http://haskell.org/haskellwiki/Haskell">http://haskell.org/haskellwiki/Haskell</a>"</p>

<p>This page has been accessed 507,753 times. This page was last modified 08:05, 24 January 2007. Recent content is available under <a href="/haskellwiki/HaskellWiki:Copyrights" title="HaskellWiki:Copyrights">a simple permissive license</a>.</p>
</div>
-}
haskellHitCount :: IO ()
haskellHitCount = do
        tags <- liftM parseTags $ openURL "http://haskell.org/haskellwiki/Haskell"
        let count = fromFooter $ head $ sections (Match.tagOpenAttrLit "div" ("class","printfooter")) tags
        putStrLn $ "haskell.org has been hit " ++ show count ++ " times"
    where
        fromFooter x = read (filter isDigit num) :: Int
            where
                num = ss !! (i - 1)
                Just i = findIndex (== "times.") ss
                ss = words s
                TagText s = sections (Match.tagOpenNameLit "p") x !! 1 !! 1


{-
<a href="http://www.cbc.ca/technology/story/2007/04/10/tech-bloggers.html" id=r-5_1115205181>
<b>Blogger code of conduct proposed</b>
-}
googleTechNews :: IO ()
googleTechNews = do
        tags <- liftM parseTags $ openURL "http://news.google.com/?ned=us&topic=t"
        let links = map extract $ sections match tags
        putStr $ unlines links
    where
        extract xs = fromTagText (xs !! 2)

        match =
           Match.tagOpenAttrNameLit "a" "id"
              (\value -> "r" `isPrefixOf` value && 'i' `notElem` value)


spjPapers :: IO ()
spjPapers = do
        tags <- liftM parseTags $ openURL "http://research.microsoft.com/~simonpj/"
        let links = map f $ sections (Match.tagOpenNameLit "a") $
                    takeWhile (not . Match.tagOpenAttrLit "a" ("name","haskell")) $
                    drop 5 $ dropWhile (not . Match.tagOpenAttrLit "a" ("name","current")) tags
        putStr $ unlines links
    where
        f :: [Tag] -> String
        f = dequote . unwords . words . fromTagText . head . filter isTagText

        dequote ('\"':xs) | last xs == '\"' = init xs
        dequote x = x


ndmPapers :: IO ()
ndmPapers = do
        tags <- liftM parseTags $ openURL "http://www-users.cs.york.ac.uk/~ndm/downloads/"
        let papers = map f $ sections (Match.tagOpenAttrLit "li" ("class","paper")) tags
        putStr $ unlines papers
    where
        f :: [Tag] -> String
        f xs = fromTagText (xs !! 2)


currentTime :: IO ()
currentTime = do
        tags <- liftM parseTags $ openURL "http://www.timeanddate.com/worldclock/city.html?n=136"
        let time = fromTagText (dropWhile (not . Match.tagOpenAttrLit "strong" ("id","ct")) tags !! 1)
        putStrLn time



type Section = String
data Package = Package {name :: String, desc :: String, href :: String}
               deriving Show

hackage :: IO [(Section,[Package])]
hackage = do
    tags <- liftM parseTags $ openURL "http://hackage.haskell.org/packages/archive/pkg-list.html"
    return $ map parseSect $ partitions (Match.tagOpenNameLit "h3") tags
    where
        parseSect xs = (nam, packs)
            where
                nam = fromTagText $ xs !! 2
                packs = map parsePackage $ partitions (Match.tagOpenNameLit "li") xs

        parsePackage xs =
           Package
              (fromTagText $ xs !! 2)
              (drop 2 $ dropWhile (/= ':') $ fromTagText $ xs !! 4)
              (fromAttrib "href" $ xs !! 1)

-- rssCreators Example: prints names of story contributors on
-- sequence.complete.org. This content is RSS (not HTML), and the selected
-- tag uses a different XML namespace "dc:creator".
rssCreators :: IO [String]
rssCreators = do
    tags <- liftM parseTags $ openURL "http://sequence.complete.org/node/feed"
    return $ map names $ partitions (Match.tagOpenNameLit "dc:creator") tags
    where
      names xs = fromTagText $ xs !! 1

-- getTagContent Example ( prints content of first td as text )
-- should print "header"
getTagContentExample :: String
getTagContentExample =
   innerText . Match.getTagContent "tr" Match.ignore $
   parseTags "<table><tr><td><th>header</th></td><td></tr><tr><td>2</td></tr>...</table>"

tests :: Bool
tests =
   and $
       Match.tagText Match.ignore (TagText "test") :
       Match.tagText ("test"==) (TagText "test") :
       Match.tagText ("soup"/=) (TagText "test") :
       Match.tagOpenNameLit "table"
           (TagOpen "table" [("id", "name")]) :
       Match.tagOpenLit "table" (Match.anyAttrLit ("id", "name"))
           (TagOpen "table" [("id", "name")]) :
       Match.tagOpenLit "table" (Match.anyAttrNameLit "id")
           (TagOpen "table" [("id", "name")]) :
       not (Match.tagOpenLit "table" (Match.anyAttrLit ("id", "name"))
              (TagOpen "table" [("id", "other name")])) :
       (parseInnerOfTag "table" == TagOpen "table" []) :
       (parseInnerOfTag "/table" == TagClose "table") :
       (parseInnerOfTag "table id=frog" == TagOpen "table" [( "id", "frog")]) :
       []

