module Text.HTML.TagSoup.Match where

import Text.HTML.TagSoup.Type (Tag(..), Attribute)
import Data.List


-- | match an opening tag
tagOpen :: (String -> Bool) -> ([Attribute String] -> Bool) -> Tag String -> Bool
tagOpen pName pAttrs (TagOpen name attrs) =
   pName name && pAttrs attrs
tagOpen _ _ _ = False

-- | match an closing tag
tagClose :: (String -> Bool) -> Tag String -> Bool
tagClose pName (TagClose name) = pName name
tagClose _ _ = False

-- | match a text
tagText :: (String -> Bool) -> Tag String -> Bool
tagText p (TagText text) = p text
tagText _ _ = False

tagComment :: (String -> Bool) -> Tag String -> Bool
tagComment p (TagComment text) = p text
tagComment _ _ = False


-- | match a opening tag's name literally
tagOpenLit :: String -> ([Attribute String] -> Bool) -> Tag String -> Bool
tagOpenLit name = tagOpen (name==)

-- | match a closing tag's name literally
tagCloseLit :: String -> Tag String -> Bool
tagCloseLit name = tagClose (name==)

tagOpenAttrLit :: String -> Attribute String -> Tag String -> Bool
tagOpenAttrLit name attr =
   tagOpenLit name (anyAttrLit attr)

{- |
Match a tag with given name, that contains an attribute
with given name, that satisfies a predicate.
If an attribute occurs multiple times,
all occurrences are checked.
-}
tagOpenAttrNameLit :: String -> String -> (String -> Bool) -> Tag String -> Bool
tagOpenAttrNameLit tagName attrName pAttrValue =
   tagOpenLit tagName
      (anyAttr (\(name,value) -> name==attrName && pAttrValue value))


-- | Check if the 'Tag String' is 'TagOpen' and matches the given name
tagOpenNameLit :: String -> Tag String -> Bool
tagOpenNameLit name = tagOpenLit name (const True)

-- | Check if the 'Tag String' is 'TagClose' and matches the given name
tagCloseNameLit :: String -> Tag String -> Bool
tagCloseNameLit name = tagCloseLit name




anyAttr :: ((String,String) -> Bool) -> [Attribute String] -> Bool
anyAttr = any

anyAttrName :: (String -> Bool) -> [Attribute String] -> Bool
anyAttrName p = any (p . fst)

anyAttrValue :: (String -> Bool) -> [Attribute String] -> Bool
anyAttrValue p = any (p . snd)


anyAttrLit :: (String,String) -> [Attribute String] -> Bool
anyAttrLit attr = anyAttr (attr==)

anyAttrNameLit :: String -> [Attribute String] -> Bool
anyAttrNameLit name = anyAttrName (name==)

anyAttrValueLit :: String -> [Attribute String] -> Bool
anyAttrValueLit value = anyAttrValue (value==)



getTagContent :: String -> ([Attribute String] -> Bool) -> [Tag String] -> [Tag String]
getTagContent name pAttrs =
   takeWhile (not . tagCloseLit name) . drop 1 .
   head . sections (tagOpenLit name pAttrs)
    where sections p = filter (p . head) . init . tails
