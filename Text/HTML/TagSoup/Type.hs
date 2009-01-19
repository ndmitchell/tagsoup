-- | The central type in TagSoup

module Text.HTML.TagSoup.Type(
    -- * Data structures and parsing
    Tag(..), Attribute, Row, Column,
    
    -- * Position manipulation
    Position, tagPosition, nullPosition, positionChar, positionString,

    -- * Tag identification
    isTagOpen, isTagClose, isTagText, isTagWarning,
    isTagOpenName, isTagCloseName,

    -- * Extraction
    fromTagText, fromAttrib,
    maybeTagText, maybeTagWarning,
    innerText,
    ) where


import Data.Char
import Data.List
import Data.Maybe


-- | An HTML attribute @id=\"name\"@ generates @(\"id\",\"name\")@
type Attribute = (String,String)

type Row = Int
type Column = Int


--- All positions are stored as a row and a column, with (1,1) being the
--- top-left position

data Position = Position !Row !Column

nullPosition = (Position 0 0)

positionString :: Position -> String -> Position
positionString = foldl' positionChar

positionChar :: Position -> Char -> Position
positionChar (Position r c) x = case x of
    '\n' -> Position (r+1) c
    '\t' -> Position r (c + 8 - mod (c-1) 8)
    _    -> Position r (c+1)

tagPosition :: Position -> Tag
tagPosition (Position r c) = TagPosition r c


-- | An HTML element, a document is @[Tag]@.
--   There is no requirement for 'TagOpen' and 'TagClose' to match
data Tag =
     TagOpen String [Attribute]  -- ^ An open tag with 'Attribute's in their original order.
   | TagClose String             -- ^ A closing tag
   | TagText String              -- ^ A text node, guaranteed not to be the empty string
   | TagComment String           -- ^ A comment
   | TagWarning String           -- ^ Meta: Mark a syntax error in the input file
   | TagPosition !Row !Column    -- ^ Meta: The position of a parsed element
     deriving (Show, Eq, Ord)


-- | Test if a 'Tag' is a 'TagOpen'
isTagOpen :: Tag -> Bool
isTagOpen (TagOpen {})  = True; isTagOpen  _ = False

-- | Test if a 'Tag' is a 'TagClose'
isTagClose :: Tag -> Bool
isTagClose (TagClose {}) = True; isTagClose _ = False

-- | Test if a 'Tag' is a 'TagText'
isTagText :: Tag -> Bool
isTagText (TagText {})  = True; isTagText  _ = False

-- | Extract the string from within 'TagText', otherwise 'Nothing'
maybeTagText :: Tag -> Maybe String
maybeTagText (TagText x) = Just x
maybeTagText _ = Nothing

-- | Extract the string from within 'TagText', crashes if not a 'TagText'
fromTagText :: Tag -> String
fromTagText (TagText x) = x
fromTagText x = error ("(" ++ show x ++ ") is not a TagText")

-- | Extract all text content from tags (similar to Verbatim found in HaXml)
innerText :: [Tag] -> String
innerText = concat . mapMaybe maybeTagText

-- | Test if a 'Tag' is a 'TagWarning'
isTagWarning :: Tag -> Bool
isTagWarning (TagWarning {})  = True; isTagWarning _ = False

-- | Extract the string from within 'TagWarning', otherwise 'Nothing'
maybeTagWarning :: Tag -> Maybe String
maybeTagWarning (TagWarning x) = Just x
maybeTagWarning _ = Nothing

-- | Extract an attribute, crashes if not a 'TagOpen'.
--   Returns @\"\"@ if no attribute present.
fromAttrib :: String -> Tag -> String
fromAttrib att (TagOpen _ atts) = fromMaybe [] $ lookup att atts
fromAttrib _ x = error ("(" ++ show x ++ ") is not a TagOpen")


-- | Returns True if the 'Tag' is 'TagOpen' and matches the given name
isTagOpenName :: String -> Tag -> Bool
isTagOpenName name (TagOpen n _) = n == name
isTagOpenName _ _ = False

-- | Returns True if the 'Tag' is 'TagClose' and matches the given name
isTagCloseName :: String -> Tag -> Bool
isTagCloseName name (TagClose n) = n == name
isTagCloseName _ _ = False
