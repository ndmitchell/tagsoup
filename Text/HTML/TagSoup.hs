{-# LANGUAGE FlexibleInstances, PatternGuards #-}

{-|
    Module      :  Text.HTML.TagSoup
    Copyright   :  (c) Neil Mitchell 2006-2009
    License     :  BSD-style

    Maintainer  :  http://community.haskell.org/~ndm/
    Stability   :  unstable
    Portability :  portable

    This module is for extracting information out of unstructured HTML code,
    sometimes known as tag-soup. This is for situations where the author of
    the HTML is not cooperating with the person trying to extract the information,
    but is also not trying to hide the information.

    The standard practice is to parse a String to 'Tag String's using 'parseTags',
    then operate upon it to extract the necessary information.
-}

module Text.HTML.TagSoup(
    -- * Data structures and parsing
    Tag(..), Attribute,
    module Text.HTML.TagSoup.Parser,
    module Text.HTML.TagSoup.Render,
    canonicalizeTags,

    -- * Tag identification
    isTagOpen, isTagClose, isTagText, isTagWarning, isTagPosition,
    isTagOpenName, isTagCloseName,

    -- * Extraction
    fromTagText, fromAttrib,
    maybeTagText, maybeTagWarning,
    innerText,

    -- * Utility
    sections, partitions,
    
    -- * Combinators
    TagRep, (~==),(~/=)
    ) where

import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Parser
import Text.HTML.TagSoup.Render
import Data.Char
import Data.List
import Text.StringLike


-- | Turns all tag names and attributes to lower case and
--   converts DOCTYPE to upper case.
canonicalizeTags :: StringLike str => [Tag str] -> [Tag str]
canonicalizeTags = map f
    where
        f (TagOpen tag attrs) | Just ('!',name) <- uncons tag = TagOpen ('!' `cons` ucase name) attrs
        f (TagOpen name attrs) = TagOpen (lcase name) [(lcase k, v) | (k,v) <- attrs]
        f (TagClose name) = TagClose (lcase name)
        f a = a

        ucase = fromString . map toUpper . toString
        lcase = fromString . map toLower . toString


-- | Define a class to allow String's or Tag str's to be used as matches
class TagRep a where
    toTagRep :: StringLike str => a -> Tag str

instance StringLike str => TagRep (Tag str) where toTagRep = fmap castString

instance TagRep [Char] where
    toTagRep x = case parseTags x of
                     [a] -> toTagRep a
                     _ -> error $ "When using a TagRep it must be exactly one tag, you gave: " ++ x



-- | Performs an inexact match, the first item should be the thing to match.
-- If the second item is a blank string, that is considered to match anything.
-- For example:
--
-- > (TagText "test" ~== TagText ""    ) == True
-- > (TagText "test" ~== TagText "test") == True
-- > (TagText "test" ~== TagText "soup") == False
--
-- For 'TagOpen' missing attributes on the right are allowed.
(~==) :: (StringLike str, TagRep t) => Tag str -> t -> Bool
(~==) a b = f a (toTagRep b)
    where
        f (TagText y) (TagText x) = strNull x || x == y
        f (TagClose y) (TagClose x) = strNull x || x == y
        f (TagOpen y ys) (TagOpen x xs) = (strNull x || x == y) && all g xs
            where
                g (name,val) | strNull name = val  `elem` map snd ys
                             | strNull val  = name `elem` map fst ys
                g nameval = nameval `elem` ys
        f _ _ = False

-- | Negation of '~=='
(~/=) :: (StringLike str, TagRep t) => Tag str -> t -> Bool
(~/=) a b = not (a ~== b)



-- | This function takes a list, and returns all suffixes whose
--   first item matches the predicate.
sections :: (a -> Bool) -> [a] -> [[a]]
sections p = filter (p . head) . init . tails

-- | This function is similar to 'sections', but splits the list
--   so no element appears in any two partitions.
partitions :: (a -> Bool) -> [a] -> [[a]]
partitions p =
   let notp = not . p
   in  groupBy (const notp) . dropWhile notp
