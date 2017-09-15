
-- | Case-insensitive combinators for matching tags. See "Text.HTML.TagSoup.Match"
--   for the original case-sensitive versions of this module.
--
module Text.HTML.TagSoup.MatchCI
    ( tagOpen
    , tagClose
    , tagText
    , tagComment
    , tagOpenLit
    , tagCloseLit
    , tagOpenAttrLit
    , tagOpenAttrNameLit
    , tagOpenNameLit
    , tagCloseNameLit
    , anyAttr
    , anyAttrName
    , anyAttrValue
    , anyAttrLit
    , anyAttrNameLit
    , anyAttrValueLit
    , getTagContent
    )
where

import           Data.List ( tails )

import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match hiding ( tagOpenLit , tagCloseLit , tagOpenAttrLit , tagOpenAttrNameLit , tagOpenNameLit , tagCloseNameLit , anyAttrLit , anyAttrNameLit , getTagContent )
import           Text.StringLike
import           Text.HasCase


-- * Matching tags

-- | Uses a case-insensitive comparison for the element name
tagOpenLit :: (HasCase s, StringLike s) => s -> ([Attribute s] -> Bool) -> Tag s -> Bool
tagOpenLit s test (TagOpen name attrs) = s `caselessEq` name && (test attrs)
tagOpenLit _ _ _ = False

-- | Uses a case-insensitive comparison for the element name
tagCloseLit :: (HasCase s, StringLike s) => s -> Tag s -> Bool
tagCloseLit = tagCloseNameLit

-- | Case-insensitive match of element and attribute names, and a case-sensitive match of the attribute value
tagOpenAttrLit :: (HasCase s, StringLike s) => s -> (Attribute s) -> Tag s -> Bool
tagOpenAttrLit s (n,v) (TagOpen name attrs) = s `caselessEq` name && any eq_attr attrs
    where
        eq_attr (n',v') = n `caselessEq` n' && v == v'
tagOpenAttrLit _ _ _ = False


-- | Case-insensitive match of element and attribute names, with a predicate on the attribute value.
tagOpenAttrNameLit :: (HasCase s, StringLike s) => s -> s -> (s -> Bool) -> Tag s -> Bool
tagOpenAttrNameLit elem_name attr_name value_test (TagOpen name attrs) = elem_name `caselessEq` name && any eq_attr attrs
    where
        eq_attr (n,v) = attr_name `caselessEq` n && (value_test v)


-- | Case-insensitive match of an element name.
tagOpenNameLit :: (HasCase s, StringLike s) => s -> Tag s -> Bool
tagOpenNameLit s (TagOpen name _) = s `caselessEq` name
tagOpenNameLit _ _ = False

-- | Case-insensitive match of an element name.
tagCloseNameLit :: (HasCase s, StringLike s) => s -> Tag s -> Bool
tagCloseNameLit s (TagClose name) = s `caselessEq` name
tagCloseNameLit _ _ = False


-- * Matching attributes


-- | Test attributes for a literal match using case-insensitive matching of the attribute name.
anyAttrLit :: (HasCase s, StringLike s) => (Attribute s) -> [Attribute s] -> Bool
anyAttrLit (name,value) attrs = any eq_attr attrs
    where
        eq_attr (n,v) = name `caselessEq` n && value == v

-- | Test attributes for a name using case-insensitive matching.
anyAttrNameLit :: (HasCase s, StringLike s) => s -> [Attribute s] -> Bool
anyAttrNameLit name attrs = any eq_attr attrs
    where
        eq_attr (n,v) = name `caselessEq` n

-- | Get the tags under tags with a given name where the attributes match some predicate.
getTagContent :: (HasCase s, StringLike s) => s -> ([Attribute s] -> Bool) -> [Tag s] -> [Tag s]
getTagContent name pAttrs =
   takeWhile (not . tagCloseLit name) . drop 1 .
      head . sections (tagOpenLit name pAttrs)
          where sections p = filter (p . head) . init . tails

