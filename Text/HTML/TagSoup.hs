{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

{-|
    This module is for working with HTML/XML. It deals with both well-formed XML and
    malformed HTML from the web. It features:

    * A lazy parser, based on the HTML 5 specification - see 'parseTags'.

    * A renderer that can write out HTML/XML - see 'renderTags'.

    * Utilities for extracting information from a document - see '~==', 'sections' and 'partitions'.

    The standard practice is to parse a 'String' to @[@'Tag' 'String'@]@ using 'parseTags',
    then operate upon it to extract the necessary information.
-}

module Text.HTML.TagSoup(
    -- * Data structures and parsing
    Tag(..), Row, Column, Attribute,
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
    TagRep(..), (~==),(~/=)
    ) where

import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Parser
import Text.HTML.TagSoup.Render
import Data.Char
import Data.List (groupBy, tails)
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

instance TagRep String where
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
        f (TagComment x) (TagComment y) = strNull x || x == y
        f (TagWarning x) (TagWarning y) = strNull x || x == y
        f (TagPosition x1 x2) (TagPosition y1 y2) = x1 == y1 && x2 == y2
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
