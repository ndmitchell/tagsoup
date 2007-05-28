module Data.Html.TagSoup.Test where

import qualified Data.Html.TagSoup as TagSoup

{-
*Data.Html.TagSoup> mapM print $ parseTags $ "</html " ++ cycle " abc=a_b&c"
("anonymous input" (line 1, column 1),TagClose "html")
("anonymous input" (line 1, column 9),TagWarning "Junk in closing tag: \"abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a_b&c abc=a
-}

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
laziness :: [Char]
laziness = lazyTags ++ lazyWarnings

lazyTags :: [Char]
lazyTags =
   map ((!!1000) . show . TagSoup.parseTags) $
      (cycle "Rhabarber") :
      (repeat '&') :
      ("<"++cycle "html") :
      ("<html "++cycle "name") :
      ("<html "++cycle "na!me=value ") :
      ("<html name="++cycle "value") :
      ("<html name=\""++cycle "value") :
      ("<html name="++cycle "val!ue") :
      ("<html name="++cycle "val&ue") :
      ("<html name="++cycle "va&l!ue") :
      ("</"++cycle "html") :
      ("</html "++cycle "junk") :
      ("<!-- "++cycle "comment") :
      ("<!"++cycle "doctype") :
      ("<!DOCTYPE"++cycle " description") :
      []

lazyWarnings :: [Char]
lazyWarnings =
   map ((!!1000) . show . tail . TagSoup.parseTags) $
      (repeat '&') :
      ("<html "++cycle "na!me=value ") :
      ("<html name="++cycle "val!ue") :
      ("<html name="++cycle "val&ue") :
      ("<html name="++cycle "va&l!ue") :
      ("</html "++cycle "junk") :
      []
