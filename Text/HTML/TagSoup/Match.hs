-- | Combinators to match tags. Some people prefer to use @(~==)@ from
--   "Text.HTML.TagSoup", others prefer these more structured combinators.
--   Which you use is personal preference.

module Text.HTML.TagSoup.Match where

import Text.HTML.TagSoup.Type (Tag(..), Attribute)
import Data.List (tails)


-- | match an opening tag
tagOpen :: (str -> Bool) -> ([Attribute str] -> Bool) -> Tag str -> Bool
tagOpen pName pAttrs (TagOpen name attrs) =
   pName name && pAttrs attrs
tagOpen _ _ _ = False

-- | match an closing tag
tagClose :: (str -> Bool) -> Tag str -> Bool
tagClose pName (TagClose name) = pName name
tagClose _ _ = False

-- | match a text
tagText :: (str -> Bool) -> Tag str -> Bool
tagText p (TagText text) = p text
tagText _ _ = False

tagComment :: (str -> Bool) -> Tag str -> Bool
tagComment p (TagComment text) = p text
tagComment _ _ = False


-- | match a opening tag's name literally
tagOpenLit :: Eq str => str -> ([Attribute str] -> Bool) -> Tag str -> Bool
tagOpenLit name = tagOpen (name==)

-- | match a closing tag's name literally
tagCloseLit :: Eq str => str -> Tag str -> Bool
tagCloseLit name = tagClose (name==)

tagOpenAttrLit :: Eq str => str -> Attribute str -> Tag str -> Bool
tagOpenAttrLit name attr =
   tagOpenLit name (anyAttrLit attr)

{- |
Match a tag with given name, that contains an attribute
with given name, that satisfies a predicate.
If an attribute occurs multiple times,
all occurrences are checked.
-}
tagOpenAttrNameLit :: Eq str => str -> str -> (str -> Bool) -> Tag str -> Bool
tagOpenAttrNameLit tagName attrName pAttrValue =
   tagOpenLit tagName
      (anyAttr (\(name,value) -> name==attrName && pAttrValue value))


-- | Check if the 'Tag str' is 'TagOpen' and matches the given name
tagOpenNameLit :: Eq str => str -> Tag str -> Bool
tagOpenNameLit name = tagOpenLit name (const True)

-- | Check if the 'Tag str' is 'TagClose' and matches the given name
tagCloseNameLit :: Eq str => str -> Tag str -> Bool
tagCloseNameLit name = tagCloseLit name




anyAttr :: ((str,str) -> Bool) -> [Attribute str] -> Bool
anyAttr = any

anyAttrName :: (str -> Bool) -> [Attribute str] -> Bool
anyAttrName p = any (p . fst)

anyAttrValue :: (str -> Bool) -> [Attribute str] -> Bool
anyAttrValue p = any (p . snd)


anyAttrLit :: Eq str => (str,str) -> [Attribute str] -> Bool
anyAttrLit attr = anyAttr (attr==)

anyAttrNameLit :: Eq str => str -> [Attribute str] -> Bool
anyAttrNameLit name = anyAttrName (name==)

anyAttrValueLit :: Eq str => str -> [Attribute str] -> Bool
anyAttrValueLit value = anyAttrValue (value==)



getTagContent :: Eq str => str -> ([Attribute str] -> Bool) -> [Tag str] -> [Tag str]
getTagContent name pAttrs =
   takeWhile (not . tagCloseLit name) . drop 1 .
   head . sections (tagOpenLit name pAttrs)
    where sections p = filter (p . head) . init . tails
