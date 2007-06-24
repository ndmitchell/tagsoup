module Text.HTML.TagSoup.Match where

import Text.HTML.TagSoup (Tag(..), Attribute)
import Data.List


ignore :: a -> Bool
ignore _ = True


-- | match an opening tag
tagOpen :: (String -> Bool) -> ([Attribute char] -> Bool) -> Tag char -> Bool
tagOpen pName pAttrs (TagOpen name attrs) =
   pName name && pAttrs attrs
tagOpen _ _ _ = False

-- | match an closing tag
tagClose :: (String -> Bool) -> Tag char -> Bool
tagClose pName (TagClose name) = pName name
tagClose _ _ = False

-- | match a text
tagText :: ([char] -> Bool) -> Tag char -> Bool
tagText p (TagText text) = p text
tagText _ _ = False

tagComment :: (String -> Bool) -> Tag char -> Bool
tagComment p (TagComment text) = p text
tagComment _ _ = False

tagSpecial :: (String -> Bool) -> (String -> Bool) -> Tag char -> Bool
tagSpecial pType pInfo (TagSpecial typ info) = pType typ && pInfo info
tagSpecial _ _ _ = False


-- | match a opening tag's name literally
tagOpenLit :: String -> ([Attribute char] -> Bool) -> Tag char -> Bool
tagOpenLit name = tagOpen (name==)

-- | match a closing tag's name literally
tagCloseLit :: String -> Tag char -> Bool
tagCloseLit name = tagClose (name==)

tagOpenAttrLit :: (Eq char) =>
   String -> Attribute char -> Tag char -> Bool
tagOpenAttrLit name attr =
   tagOpenLit name (anyAttrLit attr)

{- |
Match a tag with given name, that contains an attribute
with given name, that satisfies a predicate.
If an attribute occurs multiple times,
all occurrences are checked.
-}
tagOpenAttrNameLit :: String -> String -> ([char] -> Bool) -> Tag char -> Bool
tagOpenAttrNameLit tagName attrName pAttrValue =
   tagOpenLit tagName
      (anyAttr (\(name,value) -> name==attrName && pAttrValue value))


-- | Check if the 'Tag' is 'TagOpen' and matches the given name
tagOpenNameLit :: String -> Tag char -> Bool
tagOpenNameLit name = tagOpenLit name ignore

-- | Check if the 'Tag' is 'TagClose' and matches the given name
tagCloseNameLit :: String -> Tag char -> Bool
tagCloseNameLit name = tagCloseLit name




anyAttr :: ((String,[char]) -> Bool) -> [Attribute char] -> Bool
anyAttr = any

anyAttrName :: (String -> Bool) -> [Attribute char] -> Bool
anyAttrName p = any (p . fst)

anyAttrValue :: ([char] -> Bool) -> [Attribute char] -> Bool
anyAttrValue p = any (p . snd)


anyAttrLit :: (Eq char) => (String,[char]) -> [Attribute char] -> Bool
anyAttrLit attr = anyAttr (attr==)

anyAttrNameLit :: String -> [Attribute char] -> Bool
anyAttrNameLit name = anyAttrName (name==)

anyAttrValueLit :: (Eq char) => [char] -> [Attribute char] -> Bool
anyAttrValueLit value = anyAttrValue (value==)



getTagContent :: String -> ([Attribute char] -> Bool) -> [Tag char] -> [Tag char]
getTagContent name pAttrs =
   takeWhile (not . tagCloseLit name) . drop 1 .
   head . sections (tagOpenLit name pAttrs)
    where sections p = filter (p . head) . init . tails
