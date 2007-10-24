-- | The central type in TagSoup

module Text.HTML.TagSoup.Type(
    -- * Data structures and parsing
    Tag(..), Attribute, HTMLChar(..), CharType(..),
    tagToHTMLChar,

    -- * Tag identification
    isTagOpen, isTagClose, isTagText, isTagWarning,
    isTagOpenName, isTagCloseName,

    -- * Extraction
    fromTagText, fromAttrib,
    maybeTagText, maybeTagWarning,
    innerText,
    ) where


import Data.Char
import Data.Maybe
import Text.HTML.TagSoup.Entity


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


class CharType a where
    fromHTMLChar :: HTMLChar -> a
    toHTMLChar :: a -> HTMLChar


instance CharType Char where
    fromHTMLChar (Char c) = c
    fromHTMLChar (NumericRef c) = chr c
    fromHTMLChar (NamedRef c) = maybe (error ("TagSoup: Can't find named entity: "++c)) id $ lookupNamedEntity c

    toHTMLChar x = case escapeXMLChar x of
                        Nothing -> Char x
                        Just y -> NamedRef y


tagToHTMLChar :: CharType char => Tag char -> Tag HTMLChar
tagToHTMLChar (TagOpen a b) = TagOpen a [(c, map toHTMLChar d) | (c,d) <- b]
tagToHTMLChar (TagText s) = TagText (map toHTMLChar s)
tagToHTMLChar (TagClose x) = TagClose x
tagToHTMLChar (TagComment x) = TagComment x
tagToHTMLChar (TagSpecial x y) = TagSpecial x y
tagToHTMLChar (TagWarning x) = TagWarning x




data HTMLChar =
     Char Char
   | NumericRef Int
   | NamedRef String
      deriving Show

instance Eq HTMLChar where
    (Char a) == (Char b) = a == b
    (NumericRef a) == (NumericRef b) = a == b
    (NamedRef a) == (NamedRef b) = a == b
    a == b = a1 == b1
        where
            a1 = fromHTMLChar a :: Char
            b1 = fromHTMLChar b


instance CharType HTMLChar where
    toHTMLChar = id
    fromHTMLChar = id


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


-- | Returns True if the 'Tag' is 'TagOpen' and matches the given name
isTagOpenName :: String -> Tag a -> Bool
isTagOpenName name (TagOpen n _) = n == name
isTagOpenName _ _ = False

-- | Returns True if the 'Tag' is 'TagClose' and matches the given name
isTagCloseName :: String -> Tag a -> Bool
isTagCloseName name (TagClose n) = n == name
isTagCloseName _ _ = False
