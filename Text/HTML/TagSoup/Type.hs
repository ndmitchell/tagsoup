-- | The central type in TagSoup

module Text.HTML.TagSoup.Type(
    -- * Data structures and parsing
    Tag(..), Attribute, HTMLChar(..),

    -- * Tag identification
    isTagOpen, isTagClose, isTagText, isTagWarning,

    -- * Extraction
    fromTagText, fromAttrib,
    maybeTagText, maybeTagWarning,
    innerText,
    ) where


import Data.Maybe


-- | An HTML attribute @id=\"name\"@ generates @(\"id\",\"name\")@
type Attribute char = (String,[char])

-- | An HTML element, a document is @[Tag]@.
--   There is no requirement for 'TagOpen' and 'TagClose' to match
--   The type parameter @char@ let you choose between
--   @Char@ for interpreted HTML entity references and
--   @HTMLChar@ for uninterpreted HTML entity.
--   The first one works only properly for plain ASCII ISO-Latin-1 text
--   (no need for special character decoding),
--   the second one does also not do any decoding for you
--   but it allows you doing it properly yourself.
data Tag char =
     TagOpen String [Attribute char]
                                 -- ^ An open tag with 'Attribute's in their original order.
   | TagClose String             -- ^ A closing tag
   | TagText [char]              -- ^ A text node, guaranteed not to be the empty string
   | TagComment String           -- ^ A comment
   | TagSpecial String String    -- ^ A tag like @\<!DOCTYPE ...\>@
   | TagWarning String           -- ^ Mark a syntax error in the input file
     deriving (Show, Eq, Ord)


data HTMLChar =
     Char Char
   | NumericRef Int
   | NamedRef String
      deriving (Show, Eq)


-- | Test if a 'Tag' is a 'TagOpen'
isTagOpen :: Tag char -> Bool
isTagOpen (TagOpen {})  = True; isTagOpen  _ = False

-- | Test if a 'Tag' is a 'TagClose'
isTagClose :: Tag char -> Bool
isTagClose (TagClose {}) = True; isTagClose _ = False

-- | Test if a 'Tag' is a 'TagText'
isTagText :: Tag char -> Bool
isTagText (TagText {})  = True; isTagText  _ = False

-- | Extract the string from within 'TagText', otherwise 'Nothing'
maybeTagText :: Tag char -> Maybe [char]
maybeTagText (TagText x) = Just x
maybeTagText _ = Nothing

{-# DEPRECATED fromTagText "use innerText instead" #-}
-- | Extract the string from within 'TagText', crashes if not a 'TagText'
fromTagText :: Show char => Tag char -> [char]
fromTagText (TagText x) = x
fromTagText x = error ("(" ++ show x ++ ") is not a TagText")

-- | Extract all text content from tags (similar to Verbatim found in HaXml)
innerText :: [Tag char] -> [char]
innerText = concat . mapMaybe maybeTagText

-- | Test if a 'Tag' is a 'TagWarning'
isTagWarning :: Tag char -> Bool
isTagWarning (TagWarning {})  = True; isTagWarning _ = False

-- | Extract the string from within 'TagWarning', otherwise 'Nothing'
maybeTagWarning :: Tag char -> Maybe String
maybeTagWarning (TagWarning x) = Just x
maybeTagWarning _ = Nothing

-- | Extract an attribute, crashes if not a 'TagOpen'.
--   Returns @\"\"@ if no attribute present.
fromAttrib :: Show char => String -> Tag char -> [char]
fromAttrib att (TagOpen _ atts) = fromMaybe [] $ lookup att atts
fromAttrib _ x = error ("(" ++ show x ++ ") is not a TagOpen")

