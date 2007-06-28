module Example.Regress (
    regress
   ) where

import Text.HTML.TagSoup
import qualified Text.HTML.TagSoup.Match as Match
import Control.Exception

-- * The Test Monad

data Test a = Pass
instance Monad Test where
    a >> b = a `seq` b
instance Show (Test a) where
    show x = x `seq` "All tests passed"
pass :: Test ()
pass = Pass


-- * The Main section

regress :: IO ()
regress = print $ do
    lazyTags == lazyTags `seq` pass
    lazyWarnings == lazyWarnings `seq` pass
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
      -- ("<html "++cycle "name") :
      ("<html "++cycle "na!me=value ") :
      ("<html name="++cycle "value") :
      ("<html name=\""++cycle "value") :
      ("<html name="++cycle "val!ue") :
      --("<html name="++cycle "val&ue") :
      --("<html name="++cycle "va&l!ue") :
      --("</"++cycle "html") :
      
      -- i don't see how this can work unless the junk gets into the AST?
      --("</html "++cycle "junk") :
      
      ("<!-- "++cycle "comment") :
      ("<!"++cycle "doctype") :
      ("<!DOCTYPE"++cycle " description") :
      (cycle "1<2 ") :
      []

lazyWarnings :: [Char]
lazyWarnings =
   map ((!!1000) . show . tail . parseTags) $
      (repeat '&') :
      --("<html "++cycle "na!me=value ") :
      --("<html name="++cycle "val!ue") :
      --("<html name="++cycle "val&ue") :
      --("<html name="++cycle "va&l!ue") :
      --("</html "++cycle "junk") :
      --(cycle "1<2 ") :
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

