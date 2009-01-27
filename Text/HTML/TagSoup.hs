{-# LANGUAGE FlexibleInstances #-}

{-|
    Module      :  Text.HTML.TagSoup
    Copyright   :  (c) Neil Mitchell 2006-2007
    License     :  BSD-style

    Maintainer  :  http://www.cs.york.ac.uk/~ndm/
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
    module Parser,
    canonicalizeTags,

    -- * Tag String identification
    isTagOpen, isTagClose, isTagText, isTagWarning,
    isTagOpenName, isTagCloseName,

    -- * Extraction
    fromTagText, fromAttrib,
    maybeTagText, maybeTagWarning,
    innerText,

    -- * Utility
    sections, partitions,
    
    -- * Combinators
    TagRep, IsChar, (~==),(~/=)
    ) where

import Text.HTML.TagSoup.Parser2 as Parser
import Text.HTML.TagSoup.Type
import Data.Char
import Data.List


-- | Turns all tag names and attributes to lower case and
--   converts DOCTYPE to upper case.
canonicalizeTags :: [Tag String] -> [Tag String]
canonicalizeTags = map f
    where
        f (TagOpen ('!':name) attrs) = TagOpen ('!':map toUpper name) attrs
        f (TagOpen name attrs) = TagOpen (map toLower name) [(map toLower k, v) | (k,v) <- attrs]
        f (TagClose name) = TagClose (map toLower name)
        f a = a


-- | Define a class to allow String's or Tag String's to be used as matches
class TagRep a where
    toTagRep :: a -> Tag String

instance TagRep (Tag String) where toTagRep = id

class    IsChar a    where toChar :: a -> Char
instance IsChar Char where toChar =  id

instance IsChar c => TagRep [c] where
    toTagRep x = case parseTags s of
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
(~==) :: TagRep t => Tag String -> t -> Bool
(~==) a b = f a (toTagRep b)
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
(~/=) :: TagRep t => Tag String -> t -> Bool
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
