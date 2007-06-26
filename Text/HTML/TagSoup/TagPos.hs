
module Text.HTML.TagSoup.TagPos(
    TagPos(..), TagType(..)
    ) where


import Text.HTML.TagSoup.Position
import Text.HTML.TagSoup.Type


data TagPos char = TagPos Position (Tag char)
                   deriving Show


class TagType a where
    newTagPos :: Position -> Tag char -> a char
    fromTagPos :: a char -> Tag char


instance TagType Tag where
    newTagPos a b = b
    fromTagPos = id


instance TagType TagPos where
    newTagPos = TagPos
    fromTagPos (TagPos a b) = b
