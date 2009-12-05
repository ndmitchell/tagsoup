{-# LANGUAGE RecordWildCards, PatternGuards #-}

module Text.HTML.TagSoup.Specification(parse) where

import Text.HTML.TagSoup.Implementation
import Data.Char


white x = x `elem` "\t\n\f "

-- We make some generalisations:
-- <!name is a valid tag start closed by >
-- <?name is a valid tag start closed by ?>
-- </!name> is a valid closing tag
-- </?name> is a valid closing tag
-- <a "foo"> is a valid tag attibute, i.e missing an attribute name
-- We also don't do lowercase conversion
-- Entities are handled without a list of known entity names
-- We don't have RCData, CData or Escape modes (only effects dat and tagOpen)

-- 9.2.4 Tokenization

type Parser = S -> [Out]

parse :: String -> [Out]
parse = dat . state 

-- 9.2.4.1 Data state
dat :: Parser
dat S{..} = pos $ case hd of
    '&' -> charReference tl
    '<' -> tagOpen tl
    _ | eof -> []
    _ -> hd & dat tl


-- 9.2.4.2 Character reference data state
charReference s = charRef dat False Nothing s


-- 9.2.4.3 Tag open state
tagOpen S{..} = case hd of
    '!' -> markupDeclOpen tl
    '/' -> closeTagOpen tl
    _ | isAlpha hd -> Tag & hd & tagName False tl
    '>' -> errSeen "<>" & '<' & '>' & dat tl
    '?' -> neilXmlTagOpen tl -- NEIL
    _ -> errSeen  "<" & '<' & dat s


-- seen "<?", emitted []
neilXmlTagOpen S{..} = pos $ case hd of
    _ | isAlpha hd -> Tag & '?' & hd & tagName True tl
    _ -> errSeen "<?" & '<' & '?' & dat s

-- seen "?", expecting ">"
neilXmlTagClose S{..} = pos $ case hd of
    '>' -> TagEnd & dat tl
    _ -> errSeen "?" & beforeAttName True s


-- just seen ">" at the end, am given tl
neilTagEnd xml S{..}
    | xml = pos $ errWant "?>" & TagEnd & dat s
    | otherwise = pos $ TagEnd & dat s


-- 9.2.4.4 Close tag open state
-- Deviation: We ignore the if CDATA/RCDATA bits and tag matching
-- Deviation: On </> we output </> to the text
-- Deviation: </!name> is a closing tag, not a bogus comment
closeTagOpen S{..} = case hd of
    _ | isAlpha hd || hd `elem` "?!" -> TagShut & hd & tagName False tl
    '>' -> errSeen "</>" & '<' & '/' & '>' & dat tl
    _ | eof -> '<' & '/' & dat s
    _ -> errWant "tag name" & bogusComment s


-- 9.2.4.5 Tag name state
tagName xml S{..} = pos $ case hd of
    _ | white hd -> beforeAttName xml tl
    '/' -> selfClosingStartTag xml tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | isAlpha hd -> hd & tagName xml tl
    _ | eof -> errWant (if xml then "?>" else ">") & dat s
    _ -> hd & tagName xml tl


-- 9.2.4.6 Before attribute name state
beforeAttName xml S{..} = pos $ case hd of
    _ | white hd -> beforeAttName xml tl
    '/' -> selfClosingStartTag xml tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | hd `elem` "\'\"" -> beforeAttValue xml s -- NEIL
    _ | hd `elem` "\"'<=" -> errSeen [hd] & AttName & hd & attName xml tl
    _ | eof -> errWant (if xml then "?>" else ">") & dat s
    _ -> AttName & hd & attName xml tl


-- 9.2.4.7 Attribute name state
attName xml S{..} = pos $ case hd of
    _ | white hd -> afterAttName xml tl
    '/' -> selfClosingStartTag xml tl
    '=' -> beforeAttValue xml tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | hd `elem` "\"'<" -> errSeen [hd] & def
    _ | eof -> errWant (if xml then "?>" else ">") & dat s
    _ -> def
    where def = hd & attName xml tl


-- 9.2.4.8 After attribute name state
afterAttName xml S{..} = pos $ case hd of
    _ | white hd -> afterAttName xml tl
    '/' -> selfClosingStartTag xml tl
    '=' -> beforeAttValue xml tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | hd `elem` "\"'" -> AttVal & beforeAttValue xml s -- NEIL
    _ | hd `elem` "\"'<" -> errSeen [hd] & def
    _ | eof -> errWant (if xml then "?>" else ">") & dat s
    _ -> def
    where def = AttName & hd & attName xml tl

-- 9.2.4.9 Before attribute value state
beforeAttValue xml S{..} = pos $ case hd of
    _ | white hd -> beforeAttValue xml tl
    '\"' -> AttVal & attValueDQuoted xml tl
    '&' -> AttVal & attValueUnquoted xml s
    '\'' -> AttVal & attValueSQuoted xml tl
    '>' -> errSeen "=" & neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | hd `elem` "<=" -> errSeen [hd] & def
    _ | eof -> errWant (if xml then "?>" else ">") & dat s
    _ -> def
    where def = AttVal & hd & attValueUnquoted xml tl


-- 9.2.4.10 Attribute value (double-quoted) state
attValueDQuoted xml S{..} = pos $ case hd of
    '\"' -> afterAttValueQuoted xml tl
    '&' -> charRefAttValue (attValueDQuoted xml) (Just '\"') tl
    _ | eof -> errWant "\"" & dat s
    _ -> hd & attValueDQuoted xml tl


-- 9.2.4.11 Attribute value (single-quoted) state
attValueSQuoted xml S{..} = pos $ case hd of
    '\'' -> afterAttValueQuoted xml tl
    '&' -> charRefAttValue (attValueSQuoted xml) (Just '\'') tl
    _ | eof -> errWant "\'" & dat s
    _ -> hd & attValueSQuoted xml tl


-- 9.2.4.12 Attribute value (unquoted) state
attValueUnquoted xml S{..} = pos $ case hd of
    _ | white hd -> beforeAttName xml tl
    '&' -> charRefAttValue (attValueUnquoted xml) Nothing tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | hd `elem` "\"'<=" -> errSeen [hd] & def
    _ | eof -> errWant (if xml then "?>" else ">") & dat s
    _ -> def
    where def = hd & attValueUnquoted xml tl


-- 9.2.4.13 Character reference in attribute value state
charRefAttValue :: Parser -> Maybe Char -> Parser
charRefAttValue resume c s = charRef resume True c s


-- 9.2.4.14 After attribute value (quoted) state
afterAttValueQuoted xml S{..} = pos $ case hd of
    _ | white hd -> beforeAttName xml tl
    '/' -> selfClosingStartTag xml tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | eof -> dat s
    _ -> errSeen [hd] & beforeAttName xml s


-- 9.2.4.15 Self-closing start tag state
selfClosingStartTag xml S{..} = pos $ case hd of
    _ | xml -> errSeen "/" & beforeAttName xml s
    '>' -> TagEndClose & dat tl
    _ | eof -> errWant ">" & dat s
    _ -> errSeen "/" & beforeAttName xml s


-- 9.2.4.16 Bogus comment state
bogusComment S{..} = Comment & bogusComment1 s
bogusComment1 S{..} = pos $ case hd of
    '>' -> CommentEnd & dat tl
    _ | eof -> CommentEnd & dat s
    _ -> hd & bogusComment1 tl


-- 9.2.4.17 Markup declaration open state
markupDeclOpen S{..} = pos $ case hd of
    _ | Just s <- next "--" -> Comment & commentStart s
    _ | isAlpha hd -> Tag & '!' & hd & tagName False tl -- NEIL
    _ | Just s <- next "[CDATA[" -> cdataSection s
    _ -> errWant "tag name" & bogusComment s


-- 9.2.4.18 Comment start state
commentStart S{..} = pos $ case hd of
    '-' -> commentStartDash tl
    '>' -> errSeen "<!-->" & CommentEnd & dat tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> hd & comment tl


-- 9.2.4.19 Comment start dash state
commentStartDash S{..} = pos $ case hd of
    '-' -> commentEnd tl
    '>' -> errSeen "<!--->" & CommentEnd & dat tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> '-' & hd & comment tl


-- 9.2.4.20 Comment state
comment S{..} = pos $ case hd of
    '-' -> commentEndDash tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> hd & comment tl


-- 9.2.4.21 Comment end dash state
commentEndDash S{..} = pos $ case hd of
    '-' -> commentEnd tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> '-' & hd & comment tl


-- 9.2.4.22 Comment end state
commentEnd S{..} = pos $ case hd of
    '>' -> CommentEnd & dat tl
    '-' -> errWant "-->" & '-' & commentEnd tl
    _ | white hd -> errSeen "--" & '-' & '-' & hd & commentEndSpace tl
    '!' -> errSeen "!" & commentEndBang tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> errSeen "--" & '-' & '-' & hd & comment tl


-- 9.2.4.23 Comment end bang state
commentEndBang S{..} = pos $ case hd of
    '>' -> CommentEnd & dat tl
    '-' -> '-' & '-' & '!' & commentEndDash tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> '-' & '-' & '!' & hd & comment tl


-- 9.2.4.24 Comment end space state
commentEndSpace S{..} = pos $ case hd of
    '>' -> CommentEnd & dat tl
    '-' -> commentEndDash tl
    _ | white hd -> hd & commentEndSpace tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> hd & comment tl


-- 9.2.4.38 CDATA section state
cdataSection S{..} = pos $ case hd of
    _ | Just s <- next "]]>" -> dat s
    _ | eof -> dat s
    _ | otherwise -> hd & cdataSection tl


-- 9.2.4.39 Tokenizing character references
-- Change from spec: this is reponsible for writing '&' if nothing is to be written
charRef :: Parser -> Bool -> Maybe Char -> S -> [Out]
charRef resume att end S{..} = pos $ case hd of
    _ | eof || hd `elem` "\t\n\f <&" || maybe False (== hd) end -> '&' & resume s
    '#' -> charRefNum resume s tl
    _ -> charRefAlpha resume att s

charRefNum resume o S{..} = pos $ case hd of
    _ | hd `elem` "xX" -> charRefNum2 resume o True tl
    _ -> charRefNum2 resume o False s

charRefNum2 resume o hex S{..} = pos $ case hd of
    _ | hexChar hex hd -> (if hex then EntityHex else EntityNum) & hd & charRefNum3 resume hex tl
    _ -> errSeen "&" & '&' & resume o

charRefNum3 resume hex S{..} = pos $ case hd of
    _ | hexChar hex hd -> hd & charRefNum3 resume hex tl
    ';' -> EntityEnd & resume tl
    _ -> errWant ";" & EntityEnd & resume s

charRefAlpha resume att S{..} = pos $ case hd of
    _ | isAlpha hd -> Entity & hd & charRefAlpha2 resume att tl
    _ -> errSeen "&" & '&' & resume s

charRefAlpha2 resume att S{..} = pos $ case hd of
    _ | alphaChar hd -> hd & charRefAlpha2 resume att tl
    ';' -> EntityEnd & resume tl
    _ | att -> EntityEndAtt & resume s
    _ -> errWant ";" & EntityEnd & resume s


alphaChar x = isAlphaNum x || x `elem` ":-_"

hexChar False x = isDigit x
hexChar True  x = isDigit x || (x >= 'a' && x <= 'f') || (x >= 'A' && x <= 'F')
