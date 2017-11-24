-- | Combinators to match tags. Some people prefer to use @(~==)@ from
--   "Text.HTML.TagSoup", others prefer these more structured combinators.
--   Which you use is personal preference.
--
-- The functions below offer maximum flexibility for matching tags.
-- Using 'tagOpen', for example, you can match all links or buttons that have the "btn" class.
--
-- For simple uses cases—like matching all comment tags, or matching opening @\<a>@ tags,
-- use the tag identification functions in "Text.HTML.TagSoup#tag-identification".
module Text.HTML.TagSoup.Match where

import Text.HTML.TagSoup.Type (Tag(..), Attribute)
import Data.List (tails)

-- * Matching Tags

-- | Match an opening tag
--
-- ==== __Examples__
--
-- /Matching an opening @\<a>@ tag with a @"btn"@ class:/
--
-- >>> let tag = TagOpen "a" [("class", "btn")]
-- >>> tagOpen (== "a") (\attrs -> any (== ("class", "btn")) attrs) tag
-- True
tagOpen :: (str -> Bool) -> ([Attribute str] -> Bool) -> Tag str -> Bool
tagOpen pName pAttrs (TagOpen name attrs) =
   pName name && pAttrs attrs
tagOpen _ _ _ = False

-- | Match a closing tag
--
-- ==== __Examples__
--
-- /Matching a closing @\<\/a>@ tag:/
--
-- >>> tagClose (== "a") (TagClose "a")
-- True
--
-- >>> tagClose (== "a") (TagOpen "a" [])
-- False
tagClose :: (str -> Bool) -> Tag str -> Bool
tagClose pName (TagClose name) = pName name
tagClose _ _ = False

-- | Match text tags
--
-- ==== __Examples__
--
-- /Match all text tags:/
--
-- >>> let tags = parseTags "<p>This is a paragraph</p>"
-- [TagOpen "p" [],TagText "This is a paragraph",TagClose "p"]
-- >>> filter (tagText (const True)) tags
-- [TagText "This is a paragraph"]
tagText :: (str -> Bool) -> Tag str -> Bool
tagText p (TagText text) = p text
tagText _ _ = False

-- | Match comment tags
--
-- ==== __Examples__
--
-- /Matching comment tags that include an exclamation mark:/
--
-- >>> let tags = parseTags "<!--This is a comment-->"
-- [TagComment "This is a comment!"]
-- >>> all (tagComment (\s -> '!' `elem` s)) tags
-- True
tagComment :: (str -> Bool) -> Tag str -> Bool
tagComment p (TagComment text) = p text
tagComment _ _ = False


-- | Match an opening tag's name literally
--
-- ==== __Examples__
--
-- /Matching @\<a>@ tags with the @id@ "foo":/
--
-- >>> let tag = TagOpen "a" [("id", "foo")]
-- TagOpen "a" [("id","foo")]
-- >>> tagOpenLit "a" (\attrs -> any (== ("id", "foo")) attrs) tag
-- True
--
tagOpenLit :: Eq str => str -> ([Attribute str] -> Bool) -> Tag str -> Bool
tagOpenLit name = tagOpen (name==)

-- | Match a closing tag's name literally
--
-- ==== __Examples__
--
-- /Match a closing @\<a>@ tag:/
--
-- >>> tagCloseLit "a" (TagClose "a")
-- True
--
-- >>> tagCloseLit "a" (TagClose "em")
-- False
tagCloseLit :: Eq str => str -> Tag str -> Bool
tagCloseLit name = tagClose (name==)

-- | Match an opening tag's name literally, and at least one of its attributes
--
-- ==== __Examples__
--
-- /Matching a @\<div>@ tag with the @id@ "foo":/
--
-- >>> tagOpenAttrLit "div" ("id", "foo") (TagOpen "div" [("id", "foo")])
-- True
tagOpenAttrLit :: Eq str => str -> Attribute str -> Tag str -> Bool
tagOpenAttrLit name attr =
   tagOpenLit name (anyAttrLit attr)

{- |
Match a tag with given name, that contains an attribute
with given name, that satisfies a predicate.
If an attribute occurs multiple times,
all occurrences are checked.

==== __Examples__

/Matching an @\<a>@ tag with an ID that starts with "comment-":/

>>> let commentTag = TagOpen "a" [("id", "comment-45678")]
>>> tagOpenAttrNameLit "a" "id" (\idValue -> "comment-" `Data.List.isPrefixOf` idValue) commentTag
True
-}
tagOpenAttrNameLit :: Eq str => str -> str -> (str -> Bool) -> Tag str -> Bool
tagOpenAttrNameLit tagName attrName pAttrValue =
   tagOpenLit tagName
      (anyAttr (\(name,value) -> name==attrName && pAttrValue value))


-- | Check if the 'Tag str' is 'TagOpen' and matches the given name
--
-- ==== __Examples__
--
-- /Matching an @\<a>@ tag:/
--
-- >>> tagOpenNameLit "a" (TagOpen "a" [])
-- True
--
-- >>> tagOpenNameLit "a" (TagOpen "div" [])
-- False
tagOpenNameLit :: Eq str => str -> Tag str -> Bool
tagOpenNameLit name = tagOpenLit name (const True)

-- | Check if the 'Tag str' is 'TagClose' and matches the given name
--
-- ==== __Examples__
--
-- /Matching a closing @\<\/a>@ tag:/
--
-- >>> tagCloseNameLit "a" (TagClose "a")
-- True
--
-- >>> tagCloseNameLit "a" (TagClose "div")
-- False
tagCloseNameLit :: Eq str => str -> Tag str -> Bool
tagCloseNameLit name = tagCloseLit name


-- * Matching attributes

-- | Does any attribute name/value match the predicate.
anyAttr :: ((str,str) -> Bool) -> [Attribute str] -> Bool
anyAttr = any

-- | Does any attribute name match the predicate.
anyAttrName :: (str -> Bool) -> [Attribute str] -> Bool
anyAttrName p = any (p . fst)

-- | Does any attribute value match the predicate.
anyAttrValue :: (str -> Bool) -> [Attribute str] -> Bool
anyAttrValue p = any (p . snd)


-- | Does any attribute name/value match.
anyAttrLit :: Eq str => (str,str) -> [Attribute str] -> Bool
anyAttrLit attr = anyAttr (attr==)

-- | Does any attribute name match.
anyAttrNameLit :: Eq str => str -> [Attribute str] -> Bool
anyAttrNameLit name = anyAttrName (name==)

-- | Does any attribute value match.
anyAttrValueLit :: Eq str => str -> [Attribute str] -> Bool
anyAttrValueLit value = anyAttrValue (value==)



-- | Get the tags under tags with a given name where the attributes match some predicate.
getTagContent :: Eq str => str -> ([Attribute str] -> Bool) -> [Tag str] -> [Tag str]
getTagContent name pAttrs =
   takeWhile (not . tagCloseLit name) . drop 1 .
   head . sections (tagOpenLit name pAttrs)
    where sections p = filter (p . head) . init . tails
