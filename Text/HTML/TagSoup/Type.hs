{-# LANGUAGE DeriveDataTypeable #-}
-- | The central type in TagSoup

module Text.HTML.TagSoup.Type(
    -- * Data structures and parsing
    StringLike, Tag(..), Attribute, Row, Column,
    
    -- * Position manipulation
    Position(..), tagPosition, nullPosition, positionChar, positionString,

    -- * Tag identification
    isTagOpen, isTagClose, isTagText, isTagWarning, isTagPosition,
    isTagOpenName, isTagCloseName,

    -- * Extraction
    fromTagText, fromAttrib,
    maybeTagText, maybeTagWarning,
    innerText,
    ) where


import Data.List (foldl')
import Data.Maybe (fromMaybe, mapMaybe)
import Text.StringLike
import Data.Data(Data, Typeable)

-- | An HTML attribute @id=\"name\"@ generates @(\"id\",\"name\")@
type Attribute str = (str,str)

-- | The row/line of a position, starting at 1
type Row = Int

-- | The column of a position, starting at 1
type Column = Int


--- All positions are stored as a row and a column, with (1,1) being the
--- top-left position
data Position = Position !Row !Column deriving (Show,Eq,Ord)

nullPosition :: Position
nullPosition = Position 1 1

positionString :: Position -> String -> Position
positionString = foldl' positionChar

positionChar :: Position -> Char -> Position
positionChar (Position r c) x = case x of
    '\n' -> Position (r+1) 1
    '\t' -> Position r (c + 8 - mod (c-1) 8)
    _    -> Position r (c+1)

tagPosition :: Position -> Tag str
tagPosition (Position r c) = TagPosition r c


-- | A single HTML element. A whole document is represented by a list of @Tag@.
--   There is no requirement for 'TagOpen' and 'TagClose' to match.
data Tag str =
     TagOpen str [Attribute str]  -- ^ An open tag with 'Attribute's in their original order
   | TagClose str                 -- ^ A closing tag
   | TagText str                  -- ^ A text node, guaranteed not to be the empty string
   | TagComment str               -- ^ A comment
   | TagWarning str               -- ^ Meta: A syntax error in the input file
   | TagPosition !Row !Column     -- ^ Meta: The position of a parsed element
     deriving (Show, Eq, Ord, Data, Typeable)

instance Functor Tag where
    fmap f (TagOpen x y) = TagOpen (f x) [(f a, f b) | (a,b) <- y]
    fmap f (TagClose x) = TagClose (f x)
    fmap f (TagText x) = TagText (f x)
    fmap f (TagComment x) = TagComment (f x)
    fmap f (TagWarning x) = TagWarning (f x)
    fmap f (TagPosition x y) = TagPosition x y


-- | Test if a 'Tag' is a 'TagOpen'
isTagOpen :: Tag str -> Bool
isTagOpen (TagOpen {})  = True; isTagOpen  _ = False

-- | Test if a 'Tag' is a 'TagClose'
isTagClose :: Tag str -> Bool
isTagClose (TagClose {}) = True; isTagClose _ = False

-- | Test if a 'Tag' is a 'TagText'
isTagText :: Tag str -> Bool
isTagText (TagText {})  = True; isTagText  _ = False

-- | Extract the string from within 'TagText', otherwise 'Nothing'
maybeTagText :: Tag str -> Maybe str
maybeTagText (TagText x) = Just x
maybeTagText _ = Nothing

-- | Extract the string from within 'TagText', crashes if not a 'TagText'
fromTagText :: Show str => Tag str -> str
fromTagText (TagText x) = x
fromTagText x = error $ "(" ++ show x ++ ") is not a TagText"

-- | Extract all text content from tags (similar to Verbatim found in HaXml)
innerText :: StringLike str => [Tag str] -> str
innerText = strConcat . mapMaybe maybeTagText

-- | Test if a 'Tag' is a 'TagWarning'
isTagWarning :: Tag str -> Bool
isTagWarning (TagWarning {})  = True; isTagWarning _ = False

-- | Extract the string from within 'TagWarning', otherwise 'Nothing'
maybeTagWarning :: Tag str -> Maybe str
maybeTagWarning (TagWarning x) = Just x
maybeTagWarning _ = Nothing

-- | Test if a 'Tag' is a 'TagPosition'
isTagPosition :: Tag str -> Bool
isTagPosition TagPosition{} = True; isTagPosition _ = False

-- | Extract an attribute, crashes if not a 'TagOpen'.
--   Returns @\"\"@ if no attribute present.
fromAttrib :: (Show str, Eq str, StringLike str) => str -> Tag str -> str
fromAttrib att (TagOpen _ atts) = fromMaybe empty $ lookup att atts
fromAttrib _ x = error ("(" ++ show x ++ ") is not a TagOpen")


-- | Returns True if the 'Tag' is 'TagOpen' and matches the given name
isTagOpenName :: Eq str => str -> Tag str -> Bool
isTagOpenName name (TagOpen n _) = n == name
isTagOpenName _ _ = False

-- | Returns True if the 'Tag' is 'TagClose' and matches the given name
isTagCloseName :: Eq str => str -> Tag str -> Bool
isTagCloseName name (TagClose n) = n == name
isTagCloseName _ _ = False
