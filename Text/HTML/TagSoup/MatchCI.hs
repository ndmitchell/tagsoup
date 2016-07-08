
-- | Case-insensitive combinators for matching tags. See "Text.HTML.TagSoup.Match"
--   for the original case-sensitive versions of this module.
--
module Text.HTML.TagSoup.MatchCI
    ( AttributeCI
    , tagOpen
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

import qualified Data.CaseInsensitive as CI
import           Data.CaseInsensitive ( CI , FoldCase )
import           Data.List ( tails )

import           Text.HTML.TagSoup
import           Text.HTML.TagSoup.Match hiding ( tagOpenLit , tagCloseLit , tagOpenAttrLit , tagOpenAttrNameLit , tagOpenNameLit , tagCloseNameLit , anyAttrLit , anyAttrNameLit , getTagContent )
import           Text.StringLike

-- * Matching tags

-- | Type for a literal attribute with a case-insensitive name
type AttributeCI s = (CI s,s)

-- | Uses a case-insensitive comparison for the element name
tagOpenLit :: (FoldCase s, StringLike s) => CI s -> ([Attribute s] -> Bool) -> Tag s -> Bool
tagOpenLit s test (TagOpen name attrs) = s == (CI.mk name) && (test attrs)
tagOpenLit _ _ _ = False

-- | Uses a case-insensitive comparison for the element name
tagCloseLit :: (FoldCase s, StringLike s) => CI s -> Tag s -> Bool
tagCloseLit = tagCloseNameLit

-- | Case-insensitive match of element and attribute names, and a case-sensitive match of the attribute value
tagOpenAttrLit :: (FoldCase s, StringLike s) => CI s -> (AttributeCI s) -> Tag s -> Bool
tagOpenAttrLit s (n,v) (TagOpen name attrs) = s == (CI.mk name) && any eq_attr attrs
    where
        eq_attr (n',v') = n == (CI.mk n') && v == v'
tagOpenAttrLit _ _ _ = False


-- | Case-insensitive match of element and attribute names, with a predicate on the attribute value.
tagOpenAttrNameLit :: (FoldCase s, StringLike s) => CI s -> CI s -> (s -> Bool) -> Tag s -> Bool
tagOpenAttrNameLit elem_name attr_name value_test (TagOpen name attrs) = elem_name == (CI.mk name) && any eq_attr attrs
    where
        eq_attr (n,v) = attr_name == (CI.mk n) && (value_test v)


-- | Case-insensitive match of an element name.
tagOpenNameLit :: (FoldCase s, StringLike s) => CI s -> Tag s -> Bool
tagOpenNameLit s (TagOpen name _) = s == (CI.mk name)
tagOpenNameLit _ _ = False

-- | Case-insensitive match of an element name.
tagCloseNameLit :: (FoldCase s, StringLike s) => CI s -> Tag s -> Bool
tagCloseNameLit s (TagClose name) = s == (CI.mk name)
tagCloseNameLit _ _ = False


-- * Matching attributes


-- | Test attributes for a literal match using case-insensitive matching of the attribute name.
anyAttrLit :: (FoldCase s, StringLike s) => (AttributeCI s) -> [Attribute s] -> Bool
anyAttrLit (name,value) attrs = any eq_attr attrs
    where
        eq_attr (n,v) = name == (CI.mk n) && value == v

-- | Test attributes for a name using case-insensitive matching.
anyAttrNameLit :: (FoldCase s, StringLike s) => CI s -> [Attribute s] -> Bool
anyAttrNameLit name attrs = any eq_attr attrs
    where
        eq_attr (n,v) = name == (CI.mk n)

-- | Get the tags under tags with a given name where the attributes match some predicate.
getTagContent :: (FoldCase s, StringLike s) => CI s -> ([Attribute s] -> Bool) -> [Tag s] -> [Tag s]
getTagContent name pAttrs =
   takeWhile (not . tagCloseLit name) . drop 1 .
      head . sections (tagOpenLit name pAttrs)
          where sections p = filter (p . head) . init . tails

