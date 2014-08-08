{-# OPTIONS_GHC -fno-warn-deprecations #-}

module TagSoup.Test(test) where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Entity
import Text.HTML.TagSoup.Match

import Control.Monad
import Data.List
import Test.QuickCheck(Arbitrary(..), Testable(..), quickCheckWithResult, stdArgs,
                       Args(..), listOf, elements, Result(..))

-- * The Test Monad

type Test a = IO a

pass :: Test ()
pass = return ()

runTest :: Test () -> IO ()
runTest x = x >> putStrLn "All tests passed"

(===) :: (Show a, Eq a) => a -> a -> IO ()
a === b = if a == b then pass else fail $ "Does not equal: " ++ show a ++ " =/= " ++ show b

check :: Testable prop => prop -> IO ()
check prop = do
    res <- quickCheckWithResult stdArgs{maxSuccess=1000} prop
    case res of
        Success{} -> pass
        _ -> fail "Property failed"

newtype HTML = HTML String deriving Show
instance Arbitrary HTML where
    arbitrary = fmap (HTML . concat) $ listOf $ elements frags
        where frags = map (:[]) " \n!-</>#&;xy01[]?'\"" ++ ["CDATA","amp","gt","lt"]
    shrink (HTML x) = map HTML $ zipWith (++) (inits x) (tail $ tails x)


-- * The Main section

test :: IO ()
test = runTest $ do
    warnTests
    parseTests
    optionsTests
    renderTests
    combiTests
    entityTests
    lazyTags == lazyTags `seq` pass
    matchCombinators


{- |
This routine tests the laziness of the TagSoup parser.
For each critical part of the parser we provide a test input
with a token of infinite size.
Then the output must be infinite too.
If the laziness is broken, then the output will stop early.
We collect the thousandth character of the output of each test case.
If computation of the list stops somewhere,
you have found a laziness stopper.
-}


lazyTags :: [Char]
lazyTags = map ((!!1000) . show . parseTags)
    [cycle "Rhabarber"
    ,repeat '&'
    ,"<"++cycle "html"
    ,"<html "++cycle "na!me=value "
    ,"<html name="++cycle "value"
    ,"<html name=\""++cycle "value"
    ,"<html name="++cycle "val!ue"
    ,"<html "++cycle "name"
    ,"</"++cycle "html"
    ,"<!-- "++cycle "comment"
    ,"<!"++cycle "doctype"
    ,"<!DOCTYPE"++cycle " description"
    ,cycle "1<2 "
    ,"&" ++ cycle "t"
    ,"<html name="++cycle "val&ue"
    ,"<html name="++cycle "va&l!ue"
    ,cycle "&amp; test"

    -- i don't see how this can work unless the junk gets into the AST?
    -- ,("</html "++cycle "junk") :
    ]



matchCombinators :: Test ()
matchCombinators = do
    tagText (const True) (TagText "test") === True
    tagText ("test"==) (TagText "test") === True
    tagText ("soup"/=) (TagText "test") === True
    tagOpenNameLit "table" (TagOpen "table" [("id", "name")]) === True
    tagOpenLit "table" (anyAttrLit ("id", "name")) (TagOpen "table" [("id", "name")]) === True
    tagOpenLit "table" (anyAttrNameLit "id") (TagOpen "table" [("id", "name")]) === True
    tagOpenLit "table" (anyAttrLit ("id", "name")) (TagOpen "table" [("id", "other name")]) === False


parseTests :: Test ()
parseTests = do
    parseTags "<!DOCTYPE TEST>" === [TagOpen "!DOCTYPE" [("TEST","")]]
    parseTags "<test \"foo bar\">" === [TagOpen "test" [("\"foo",""),("bar\"","")]]
    parseTags "<test baz \"foo\">" === [TagOpen "test" [("baz",""),("\"foo\"","")]]
    parseTags "<test 'foo bar'>" === [TagOpen "test" [("'foo",""),("bar'","")]]
    parseTags "<test bar=''' />" === [TagOpen "test" [("bar",""),("'","")], TagClose "test"]
    parseTags "<test2 a b>" === [TagOpen "test2" [("a",""),("b","")]]
    parseTags "<test2 ''>" === [TagOpen "test2" [("''","")]]
    parseTags "</test foo>" === [TagClose "test"]
    parseTags "<test/>" === [TagOpen "test" [], TagClose "test"]
    parseTags "<test1 a = b>" === [TagOpen "test1" [("a","b")]]
    parseTags "hello &amp; world" === [TagText "hello & world"]
    parseTags "hello &#64; world" === [TagText "hello @ world"]
    parseTags "hello &#x40; world" === [TagText "hello @ world"]
    parseTags "hello &haskell; world" === [TagText "hello &haskell; world"]
    parseTags "hello \n\t world" === [TagText "hello \n\t world"]
    parseTags "<a href=http://www.google.com>" === [TagOpen "a" [("href","http://www.google.com")]]
    parseTags "<foo bar=\"bar&#54;baz\">" === [TagOpen "foo" [("bar","bar6baz")]]
    parseTags "<foo bar=\"bar&amp;baz\">" === [TagOpen "foo" [("bar","bar&baz")]]
    parseTags "hey &how are you" === [TagText "hey &how are you"]
    parseTags "hey &how; are you" === [TagText "hey &how; are you"]
    parseTags "hey &amp are you" === [TagText "hey & are you"]
    parseTags "hey &amp; are you" === [TagText "hey & are you"]

    -- real cases reported by users
    parseTags "&nwarr;x&ngeqq;" === [TagText ['\x2196','x','\x2267','\x0338']]
    parseTags "test &#10933649; test" === [TagText "test ? test"]

    parseTags "<a href=\"series.php?view=single&ID=72710\">" === [TagOpen "a" [("href","series.php?view=single&ID=72710")]]

    parseTags "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" ===
        [TagOpen "!DOCTYPE" [("HTML",""),("PUBLIC",""),("","-//W3C//DTD HTML 4.01//EN"),("","http://www.w3.org/TR/html4/strict.dtd")]]

    parseTags "<script src=\"http://edge.jobthread.com/feeds/jobroll/?s_user_id=100540&subtype=slashdot\">" ===
        [TagOpen "script" [("src","http://edge.jobthread.com/feeds/jobroll/?s_user_id=100540&subtype=slashdot")]]

    parseTags "<a title='foo'bar' href=correct>text" === [TagOpen "a" [("title","foo"),("bar'",""),("href", "correct")],TagText "text"]

    parseTags "<test><![CDATA[Anything goes, <em>even hidden markup</em> &amp; entities]]> but this is outside</test>" ===
        [TagOpen "test" [],TagText "Anything goes, <em>even hidden markup</em> &amp; entities but this is outside",TagClose "test"]

    parseTags "<a \r\n href=\"url\">" === [TagOpen "a" [("href","url")]]

    parseTags "<a href='random.php'><img src='strips/130307.jpg' alt='nukular bish'' title='' /></a>" === 
        [TagOpen "a" [("href","random.php")],TagOpen "img" [("src","strips/130307.jpg"),("alt","nukular bish"),("'",""),("title","")],TagClose "img",TagClose "a"]

    parseTags "<p>some text</p\n<img alt='&lt; &yyy; &gt;' src=\"abc.gif\">" ===
        [TagOpen "p" [],TagText "some text",TagClose "p"]

    parseTags "<script> if (x<bomb) </script>" === [TagOpen "script" [], TagText " if (x<bomb) ", TagClose "script"]
    parseTags "<script> if (x<bomb) " === [TagOpen "script" [], TagText " if (x<bomb) "]
    parseTags "<SCRIPT language=foo> if (x<bomb) </SCRIPT>" === [TagOpen "SCRIPT" [("language","foo")], TagText " if (x<bomb) ", TagClose "SCRIPT"]
    parseTags "<script /><test>" === [TagOpen "script" [], TagClose "script", TagOpen "test" []]


optionsTests :: Test ()
optionsTests = check $ \(HTML x) -> all (f x) $ replicateM 3 [False,True]
    where
        f str [pos,warn,merge] =
                bool "merge" (not merge || adjacentTagText tags) &&
                bool "warn" (warn || all (not . isTagWarning) tags) &&
                bool "pos" (if pos then alternatePos tags else all (not . isTagPosition) tags)
            where tags = parseTagsOptions parseOptions{optTagPosition=pos,optTagWarning=warn,optTagTextMerge=merge} str
                  bool x b = b || error ("optionsTests failed with " ++ x ++ " on " ++ show (pos,warn,merge,str,tags))

        -- optTagTextMerge implies no adjacent TagText cells
        -- and none separated by only warnings or positions
        adjacentTagText = g True -- can the next be a tag text
            where g i (x:xs) | isTagText x = i && g False xs
                             | isTagPosition x || isTagWarning x = g i xs
                             | otherwise = g True xs
                  g i [] = True

        -- optTagPosition implies every element must be followed
        -- by a position node, no two position nodes must be adjacent
        -- and all positions must be increasing
        alternatePos (TagPosition l1 c1 : x : TagPosition l2 c2 : xs)
            | (l1,c1) <= (l2,c2) && not (isTagPosition x) = alternatePos $ TagPosition l2 c2 : xs
        alternatePos [TagPosition l1 c1, x] | not $ isTagPosition x = True
        alternatePos [] = True
        alternatePos _ = False


renderTests :: Test ()
renderTests = do
    let rp = renderTags . parseTags
    rp "<test>" === "<test>"
    rp "<br></br>" === "<br />"
    rp "<script></script>" === "<script></script>"
    rp "hello & world" === "hello &amp; world"
    rp "<a href=test>" === "<a href=\"test\">"
    rp "<a href>" === "<a href>"
    rp "<a href?>" === "<a href?>"
    rp "<?xml foo?>" === "<?xml foo ?>"
    rp "<?xml foo?>" === "<?xml foo ?>"
    rp "<!-- neil -->" === "<!-- neil -->"
    rp "<a test=\"a&apos;b\">" === "<a test=\"a'b\">"
    escapeHTML "this is a &\" <test> '" === "this is a &amp;&quot; &lt;test&gt; '"
    check $ \(HTML x) -> let y = rp x in rp y == (y :: String)

    
entityTests :: Test ()
entityTests = do
    lookupNumericEntity "65" === Just "A"
    lookupNumericEntity "x41" === Just "A"
    lookupNumericEntity "x4E" === Just "N"
    lookupNumericEntity "x4e" === Just "N"
    lookupNumericEntity "Haskell" === Nothing
    lookupNumericEntity "" === Nothing
    lookupNumericEntity "89439085908539082" === Nothing
    lookupNamedEntity "amp" === Just "&"
    lookupNamedEntity "haskell" === Nothing
    escapeXML "hello world" === "hello world"
    escapeXML "hello & world" === "hello &amp; world"


combiTests :: Test ()
combiTests = do
    (TagText "test" ~== TagText ""    ) === True
    (TagText "test" ~== TagText "test") === True
    (TagText "test" ~== TagText "soup") === False
    (TagText "test" ~== "test") === True
    (TagOpen "test" [] ~== "<test>") === True
    (TagOpen "test" [] ~== "<soup>") === False
    (TagOpen "test" [] ~/= "<soup>") === True
    (TagComment "foo" ~== "<!--foo-->") === True
    (TagComment "bar" ~== "<!--bar-->") === True


warnTests :: Test ()
warnTests = do
    let p = parseTagsOptions parseOptions{optTagPosition=True,optTagWarning=True}
        wt x = [(msg,c) | TagWarning msg:TagPosition _ c:_ <- tails $ p x]
    wt "neil &foo bar" === [("Unknown entity: foo",10)]
