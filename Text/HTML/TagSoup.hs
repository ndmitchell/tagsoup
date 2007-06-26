{-|
    Module      :  Text.HTML.TagSoup
    Copyright   :  (c) Neil Mitchell 2006-2007
    License     :  BSD-style

    Maintainer  :  http://www.cs.york.ac.uk/~ndm/
    Stability   :  moving towards stable
    Portability :  portable

    This module is for extracting information out of unstructured HTML code,
    sometimes known as tag-soup. This is for situations where the author of
    the HTML is not cooperating with the person trying to extract the information,
    but is also not trying to hide the information.

    The standard practice is to parse a String to 'Tag's using 'parsePosTags',
    then operate upon it to extract the necessary information.
-}

module Text.HTML.TagSoup(
    -- * Data structures and parsing
    Tag(..), Attribute, CharType, HTMLChar(..),
    TagPos(..), TagType,
    parseTags, parseTagsGeneric,
    canonicalizeTags,

    -- * Tag identification
    isTagOpen, isTagClose, isTagText, isTagWarning,
    isTagOpenName, isTagCloseName,

    -- * Extraction
    fromTagText, fromAttrib,
    maybeTagText, maybeTagWarning,
    innerText,

    -- * Utility
    sections, partitions,
    
    -- * Combinators
    (~==),(~/=)
    ) where

import Text.HTML.TagSoup.Parser
import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.TagPos
import Data.Char
import Data.List


{- |
Turns all tag names to lower case and
converts DOCTYPE to upper case.
-}
canonicalizeTags :: TagType tag => [tag char] -> [tag char]
canonicalizeTags = map canonicalizeTag

canonicalizeTag :: TagType tag => tag char -> tag char
canonicalizeTag x = setTag x $ f $ getTag x
    where
        f (TagOpen  name attrs ) = TagOpen    (map toLower name) attrs
        f (TagClose name       ) = TagClose   (map toLower name)
        f (TagSpecial name info) = TagSpecial (map toUpper name) info
        f x = x


-- | Define a class to allow String's or Tag's to be used as matches
class TagRep a where
    toTagRep :: a -> Tag HTMLChar

instance CharType char => TagRep (Tag char) where
    toTagRep = tagToHTMLChar

class    IsChar a    where toChar :: a -> Char
instance IsChar Char where toChar =  id

instance IsChar c => TagRep [c] where
    toTagRep x = case parseTagsGeneric s of
                    [a] -> a
                    _ -> error $ "When using a TagRep it must be exactly one tag, you gave: " ++ s
        where s = map toChar x



-- | Performs an inexact match, the first item should be the thing to match.
-- If the second item is a blank string, that is considered to match anything.
-- For example:
--
-- > (TagText "test" ~== TagText ""    ) == True
-- > (TagText "test" ~== TagText "test") == True
-- > (TagText "test" ~== TagText "soup") == False
--
-- For 'TagOpen' missing attributes on the right are allowed.
(~==) :: (CharType char, TagRep t) => Tag char -> t -> Bool
(~==) a b = f (tagToHTMLChar a) (toTagRep b)
    where
        f (TagText y) (TagText x) = null x || x == y
        f (TagClose y) (TagClose x) = null x || x == y
        f (TagOpen y ys) (TagOpen x xs) = (null x || x == y) && all g xs
            where
                g (name,val) | null name = val  `elem` map snd ys
                             | null val  = name `elem` map fst ys
                g nameval = nameval `elem` ys
        f _ _ = False

-- | Negation of '~=='
(~/=) :: (CharType char, TagRep t) => Tag char -> t -> Bool
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
