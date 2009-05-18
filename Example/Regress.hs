{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Example.Regress (regress) where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Render
import Text.HTML.TagSoup.Entity
import qualified Text.HTML.TagSoup.Match as Match
import Control.Exception

-- * The Test Monad

data Test a = Pass
instance Monad Test where
    a >> b = a `seq` b
    return = error "No return for Monad Test"
    (>>=) = error "No bind (>>=) for Monad Test"
instance Show (Test a) where
    show x = x `seq` "All tests passed"

pass :: Test ()
pass = Pass

(===) :: (Show a, Eq a) => a -> a -> Test ()
a === b = if a == b then pass else fail $ "Does not equal: " ++ show a ++ " =/= " ++ show b

-- * The Main section

regress :: IO ()
regress = print $ do
    parseTests
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
lazyTags =
   map ((!!1000) . show . parseTags) $
      (cycle "Rhabarber") :
      (repeat '&') :
      ("<"++cycle "html") :
      ("<html "++cycle "na!me=value ") :
      ("<html name="++cycle "value") :
      ("<html name=\""++cycle "value") :
      ("<html name="++cycle "val!ue") :
      ("<html "++cycle "name") :
      ("</"++cycle "html") :
      ("<!-- "++cycle "comment") :
      ("<!"++cycle "doctype") :
      ("<!DOCTYPE"++cycle " description") :
      (cycle "1<2 ") :
      
      -- need further analysis
      ("<html name="++cycle "val&ue") :
      ("<html name="++cycle "va&l!ue") :
      (cycle "&amp; test") :

      -- i don't see how this can work unless the junk gets into the AST?
      ("</html "++cycle "junk") :
      -- ("&" ++ cycle "t") :

      []



matchCombinators :: Test ()
matchCombinators = assert (and tests) pass
    where
        tests =
            Match.tagText (const True) (TagText "test") :
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
            []


parseTests :: Test ()
parseTests = do
    parseTags "<!DOCTYPE TEST>" === [TagOpen "!DOCTYPE" [("TEST","")]]
    parseTags "<test \"foo bar\">" === [TagOpen "test" [("","foo bar")]]
    parseTags "<test \'foo bar\'>" === [TagOpen "test" [("","foo bar")]]
    parseTags "<:test \'foo bar\'>" === [TagOpen ":test" [("","foo bar")]]
    parseTags "<test2 a b>" === [TagOpen "test2" [("a",""),("b","")]]
    parseTags "<test1 a = b>" === [TagOpen "test1" [("a","b")]]
    parseTags "hello &amp; world" === [TagText "hello & world"]
    parseTags "hello &#64; world" === [TagText "hello @ world"]
    parseTags "hello &#x40; world" === [TagText "hello @ world"]
    parseTags "hello &haskell; world" === [TagText "hello &haskell; world"]
    parseTags "hello \n\t world" === [TagText "hello \n\t world"]
    parseTags "<a href=http://www.google.com>" === [TagOpen "a" [("href","http://www.google.com")]]

    parseTags "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">" ===
        [TagOpen "!DOCTYPE" [("HTML",""),("PUBLIC",""),("","-//W3C//DTD HTML 4.01//EN"),("","http://www.w3.org/TR/html4/strict.dtd")]]
    --parseTags "<script src=\"http://edge.jobthread.com/feeds/jobroll/?s_user_id=100540&subtype=slashdot\">" ===
    --    [TagOpen "script" [("src","http://edge.jobthread.com/feeds/jobroll/?s_user_id=100540&subtype=slashdot")]]

    --parseTags "<a title='foo'bar' href=correct>text" === [TagOpen "a" [("title", "foo"),
    --                                                                   ("bar",   ""),
    --                                                                   ("href", "correct")],
    --                                                     TagText "text"]
    parseTags "<test><![CDATA[Anything goes, <em>even hidden markup</em> &amp; entities]]> but this is outside</test>" ===
        [ TagOpen "test" []
        , TagCData "Anything goes, <em>even hidden markup</em> &amp; entities"
        , TagText " but this is outside"
        , TagClose "test"
        ]


renderTests :: Test ()
renderTests = do
    let rp = renderTags . parseTags
    rp "<test>" === "<test>"
    rp "<br></br>" === "<br />"
    rp "<script></script>" === "<script></script>"
    rp "hello & world" === "hello &amp; world"
    rp "<a href=test>" === "<a href=\"test\">"
    rp "<a href>" === "<a href>"
    rp "<!-- neil -->" === "<!-- neil -->"


entityTests :: Test ()
entityTests = do
    lookupNumericEntity "65" === Just 'A'
    lookupNumericEntity "x41" === Just 'A'
    lookupNumericEntity "x4E" === Just 'N'
    lookupNumericEntity "x4e" === Just 'N'
    lookupNumericEntity "Haskell" === Nothing
    lookupNumericEntity "" === Nothing
    lookupNumericEntity "89439085908539082" === Nothing
    lookupNamedEntity "amp" === Just '&'
    lookupNamedEntity "haskell" === Nothing
    escapeXMLChar 'a' === Nothing
    escapeXMLChar '&' === Just "amp"


combiTests :: Test ()
combiTests = do
    (TagText "test" ~== TagText ""    ) === True
    (TagText "test" ~== TagText "test") === True
    (TagText "test" ~== TagText "soup") === False
    (TagText "test" ~== "test") === True
    (TagOpen "test" [] ~== "<test>") === True
    (TagOpen "test" [] ~== "<soup>") === False
    (TagOpen "test" [] ~/= "<soup>") === True

