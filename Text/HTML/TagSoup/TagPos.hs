
module Text.HTML.TagSoup.TagPos(
    TagPos(..), TagType(..)
    ) where


import Text.HTML.TagSoup.Position
import Text.HTML.TagSoup.Type


data TagPos char = TagPos Position (Tag char)
                   deriving Show


class TagType a where
    newTagPos :: Position -> Tag char -> a char
    getTag :: a char -> Tag char
    setTag :: a char -> Tag char -> a char


instance TagType Tag where
    newTagPos _ b = b
    getTag = id
    setTag _ b = b


instance TagType TagPos where
    newTagPos = TagPos
    getTag (TagPos _ b) = b
    setTag (TagPos a _) b = TagPos a b

