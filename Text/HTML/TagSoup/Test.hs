module Text.HTML.TagSoup.Test (
    -- * Tests on laziness
    laziness, lazyTags, lazyWarnings,

    -- * QuickCheck properties
    propSections, propPartitions,
   ) where

import qualified Text.HTML.TagSoup as TagSoup

{-
*Text.HTML.TagSoup> mapM print $ parseTags $ "</html " ++ cycle " abc=a_b&c"
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
      (cycle "1<2 ") :
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
      (cycle "1<2 ") :
      []



infixr 5 ?:

(?:) :: (Bool, a) -> [a] -> [a]
(?:) (True,  x) xs = x:xs
(?:) (False, _) xs = xs


sections_rec :: (a -> Bool) -> [a] -> [[a]]
sections_rec f =
   let recurse [] = []
       recurse (x:xs) = (f x, x:xs) ?: recurse xs
   in  recurse

propSections :: Int -> [Int] -> Bool
propSections y xs  =
   let p = (<=y)
   in  TagSoup.sections p xs == sections_rec p xs



partitions_rec :: (a -> Bool) -> [a] -> [[a]]
partitions_rec f = g . dropWhile (not . f)
    where
        g [] = []
        g (x:xs) = (x:a) : g b
            where (a,b) = break f xs

propPartitions :: Int -> [Int] -> Bool
propPartitions y xs  =
   let p = (<=y)
   in  TagSoup.partitions p xs == partitions_rec p xs
