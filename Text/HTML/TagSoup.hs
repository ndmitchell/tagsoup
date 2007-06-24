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
    Tag(..), PosTag, Attribute, CharType, HTMLChar(..),
    parseTags, parsePosTags, parseFilePosTags,
    parseTag, parseInnerOfTag,
    canonicalizeTags, canonicalizePosTags,

    -- * Tag identification
    isTagOpen, isTagClose, isTagText, isTagWarning,

    -- * Extraction
    fromTagText, fromAttrib,
    maybeTagText, maybeTagWarning,
    innerText,

    -- * Utility
    sections, partitions,
    ) where

import Text.HTML.TagSoup.Parser
import Text.HTML.TagSoup.Type
import Data.Char
import Data.List


{- |
Turns all tag names to lower case and
converts DOCTYPE to upper case.
-}
canonicalizePosTags :: [PosTag char] -> [PosTag char]
canonicalizePosTags =
   map (\(i,tag) -> (i, canonicalizeTag tag))

canonicalizeTags :: [Tag char] -> [Tag char]
canonicalizeTags =
   map canonicalizeTag

canonicalizeTag :: Tag char -> Tag char
canonicalizeTag t =
   case t of
      TagOpen  name attrs  -> TagOpen  (map toLower name) attrs
      TagClose name        -> TagClose (map toLower name)
      TagSpecial name info -> TagSpecial (map toUpper name) info
      _ -> t


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
