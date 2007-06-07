module Text.HTML.TagSoup.Match where

import Text.HTML.TagSoup (Tag(..), Attribute, sections)


ignore :: a -> Bool
ignore _ = True


-- | match an opening tag
tagOpen :: (String -> Bool) -> ([Attribute] -> Bool) -> Tag -> Bool
tagOpen pName pAttrs (TagOpen name attrs) =
   pName name && pAttrs attrs
tagOpen _ _ _ = False

-- | match an closing tag
tagClose :: (String -> Bool) -> Tag -> Bool
tagClose pName (TagClose name) = pName name
tagClose _ _ = False

-- | match a text
tagText :: (String -> Bool) -> Tag -> Bool
tagText p (TagText text) = p text
tagText _ _ = False

tagComment :: (String -> Bool) -> Tag -> Bool
tagComment p (TagComment text) = p text
tagComment _ _ = False

tagSpecial :: (String -> Bool) -> (String -> Bool) -> Tag -> Bool
tagSpecial pType pInfo (TagSpecial typ info) = pType typ && pInfo info
tagSpecial _ _ _ = False


-- | match a opening tag's name literally
tagOpenLit :: String -> ([Attribute] -> Bool) -> Tag -> Bool
tagOpenLit name = tagOpen (name==)

-- | match a closing tag's name literally
tagCloseLit :: String -> Tag -> Bool
tagCloseLit name = tagClose (name==)

tagOpenAttrLit :: String -> Attribute -> Tag -> Bool
tagOpenAttrLit name attr =
   tagOpenLit name (anyAttrLit attr)

{- |
Match a tag with given name, that contains an attribute
with given name, that satisfies a predicate.
If an attribute occurs multiple times,
all occurrences are checked.
-}
tagOpenAttrNameLit :: String -> String -> (String -> Bool) -> Tag -> Bool
tagOpenAttrNameLit tagName attrName pAttrValue =
   tagOpenLit tagName
      (anyAttr (\(name,value) -> name==attrName && pAttrValue value))


-- | Check if the 'Tag' is 'TagOpen' and matches the given name
tagOpenNameLit :: String -> Tag -> Bool
tagOpenNameLit name = tagOpenLit name ignore

-- | Check if the 'Tag' is 'TagClose' and matches the given name
tagCloseNameLit :: String -> Tag -> Bool
tagCloseNameLit name = tagCloseLit name




anyAttr :: ((String,String) -> Bool) -> [Attribute] -> Bool
anyAttr = any

anyAttrName :: (String -> Bool) -> [Attribute] -> Bool
anyAttrName p = any (p . fst)

anyAttrValue :: (String -> Bool) -> [Attribute] -> Bool
anyAttrValue p = any (p . fst)


anyAttrLit :: (String,String) -> [Attribute] -> Bool
anyAttrLit attr = anyAttr (attr==)

anyAttrNameLit :: String -> [Attribute] -> Bool
anyAttrNameLit name = anyAttrName (name==)

anyAttrValueLit :: String -> [Attribute] -> Bool
anyAttrValueLit value = anyAttrValue (value==)



getTagContent :: String -> ([Attribute] -> Bool) -> [Tag] -> [Tag]
getTagContent name pAttrs =
   takeWhile (not . tagCloseLit name) . drop 1 .
   head . sections (tagOpenLit name pAttrs)
