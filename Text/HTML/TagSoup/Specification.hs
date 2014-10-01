{-# LANGUAGE RecordWildCards, PatternGuards #-}

module Text.HTML.TagSoup.Specification(parse) where

import Text.HTML.TagSoup.Implementation
import Data.Char (isAlpha, isAlphaNum, isDigit, toLower)

-- We make some generalisations:
-- <!name is a valid tag start closed by >
-- <?name is a valid tag start closed by ?>
-- </!name> is a valid closing tag
-- </?name> is a valid closing tag
-- <a "foo"> is a valid tag attibute in ! and ?, i.e missing an attribute name
-- We also don't do lowercase conversion
-- Entities are handled without a list of known entity names
-- We don't have RCData, CData or Escape modes (only effects dat and tagOpen)


data TypeTag = TypeNormal -- <foo
             | TypeXml    -- <?foo
             | TypeDecl   -- <!foo
             | TypeScript -- <script
               deriving Eq


-- 2.4.1 Common parser idioms
white x = x `elem` " \t\n\f\r"


-- 8.2.4 Tokenization

type Parser = S -> [Out]

parse :: String -> [Out]
parse = dat . state 

-- 8.2.4.1 Data state
dat :: Parser
dat S{..} = pos $ case hd of
    '&' -> charReference tl
    '<' -> tagOpen tl
    _ | eof -> []
    _ -> hd & dat tl


-- 8.2.4.2 Character reference data state
charReference s = charRef dat False Nothing s


-- 8.2.4.3 Tag open state
tagOpen S{..} = case hd of
    '!' -> markupDeclOpen tl
    '/' -> closeTagOpen tl
    _ | isAlpha hd -> Tag & hd & tagName (if isScript s then TypeScript else TypeNormal) tl
    '>' -> errSeen "<>" & '<' & '>' & dat tl
    '?' -> neilXmlTagOpen tl -- NEIL
    _ -> errSeen  "<" & '<' & dat s

isScript = f "script"
    where
        f (c:cs) S{..} = toLower hd == c && f cs tl
        f [] S{..} = white hd || hd == '/' || hd == '>' || hd == '?' || eof


-- seen "<?", emitted []
neilXmlTagOpen S{..} = pos $ case hd of
    _ | isAlpha hd -> Tag & '?' & hd & tagName TypeXml tl
    _ -> errSeen "<?" & '<' & '?' & dat s

-- seen "?", expecting ">"
neilXmlTagClose S{..} = pos $ case hd of
    '>' -> TagEnd & dat tl
    _ -> errSeen "?" & beforeAttName TypeXml s


-- just seen ">" at the end, am given tl
neilTagEnd typ S{..}
    | typ == TypeXml = pos $ errWant "?>" & TagEnd & dat s
    | typ == TypeScript = pos $ TagEnd & neilScriptBody s
    | otherwise = pos $ TagEnd & dat s

-- Inside a <script> tag, only break on </script
neilScriptBody o@S{..}
    | hd == '<', S{..} <- tl
    , hd == '/', S{..} <- tl
    , isScript s
    = dat o
    | eof = []
    | otherwise =  pos $ hd & neilScriptBody tl


-- 8.2.4.4 Close tag open state
-- Deviation: We ignore the if CDATA/RCDATA bits and tag matching
-- Deviation: On </> we output </> to the text
-- Deviation: </!name> is a closing tag, not a bogus comment
closeTagOpen S{..} = case hd of
    _ | isAlpha hd || hd `elem` "?!" -> TagShut & hd & tagName TypeNormal tl
    '>' -> errSeen "</>" & '<' & '/' & '>' & dat tl
    _ | eof -> '<' & '/' & dat s
    _ -> errWant "tag name" & bogusComment s


-- 8.2.4.5 Tag name state
tagName typ S{..} = pos $ case hd of
    _ | white hd -> beforeAttName typ tl
    '/' -> selfClosingStartTag typ tl
    '>' -> neilTagEnd typ tl
    '?' | typ == TypeXml -> neilXmlTagClose tl
    _ | isAlpha hd -> hd & tagName typ tl
    _ | eof -> errWant (if typ == TypeXml then "?>" else ">") & dat s
    _ -> hd & tagName typ tl


-- 8.2.4.6 Before attribute name state
beforeAttName typ S{..} = pos $ case hd of
    _ | white hd -> beforeAttName typ tl
    '/' -> selfClosingStartTag typ tl
    '>' -> neilTagEnd typ tl
    '?' | typ == TypeXml -> neilXmlTagClose tl
    _ | typ /= TypeNormal && hd `elem` "\'\"" -> beforeAttValue typ s -- NEIL
    _ | hd `elem` "\"'<=" -> errSeen [hd] & AttName & hd & attName typ tl
    _ | eof -> errWant (if typ == TypeXml then "?>" else ">") & dat s
    _ -> AttName & hd & attName typ tl


-- 8.2.4.7 Attribute name state
attName typ S{..} = pos $ case hd of
    _ | white hd -> afterAttName typ tl
    '/' -> selfClosingStartTag typ tl
    '=' -> beforeAttValue typ tl
    '>' -> neilTagEnd typ tl
    '?' | typ == TypeXml -> neilXmlTagClose tl
    _ | hd `elem` "\"'<" -> errSeen [hd] & def
    _ | eof -> errWant (if typ == TypeXml then "?>" else ">") & dat s
    _ -> def
    where def = hd & attName typ tl


-- 8.2.4.8 After attribute name state
afterAttName typ S{..} = pos $ case hd of
    _ | white hd -> afterAttName typ tl
    '/' -> selfClosingStartTag typ tl
    '=' -> beforeAttValue typ tl
    '>' -> neilTagEnd typ tl
    '?' | typ == TypeXml -> neilXmlTagClose tl
    _ | typ /= TypeNormal && hd `elem` "\"'" -> AttVal & beforeAttValue typ s -- NEIL
    _ | hd `elem` "\"'<" -> errSeen [hd] & def
    _ | eof -> errWant (if typ == TypeXml then "?>" else ">") & dat s
    _ -> def
    where def = AttName & hd & attName typ tl

-- 8.2.4.9 Before attribute value state
beforeAttValue typ S{..} = pos $ case hd of
    _ | white hd -> beforeAttValue typ tl
    '\"' -> AttVal & attValueDQuoted typ tl
    '&' -> AttVal & attValueUnquoted typ s
    '\'' -> AttVal & attValueSQuoted typ tl
    '>' -> errSeen "=" & neilTagEnd typ tl
    '?' | typ == TypeXml -> neilXmlTagClose tl
    _ | hd `elem` "<=" -> errSeen [hd] & def
    _ | eof -> errWant (if typ == TypeXml then "?>" else ">") & dat s
    _ -> def
    where def = AttVal & hd & attValueUnquoted typ tl


-- 8.2.4.10 Attribute value (double-quoted) state
attValueDQuoted typ S{..} = pos $ case hd of
    '\"' -> afterAttValueQuoted typ tl
    '&' -> charRefAttValue (attValueDQuoted typ) (Just '\"') tl
    _ | eof -> errWant "\"" & dat s
    _ -> hd & attValueDQuoted typ tl


-- 8.2.4.11 Attribute value (single-quoted) state
attValueSQuoted typ S{..} = pos $ case hd of
    '\'' -> afterAttValueQuoted typ tl
    '&' -> charRefAttValue (attValueSQuoted typ) (Just '\'') tl
    _ | eof -> errWant "\'" & dat s
    _ -> hd & attValueSQuoted typ tl


-- 8.2.4.12 Attribute value (unquoted) state
attValueUnquoted typ S{..} = pos $ case hd of
    _ | white hd -> beforeAttName typ tl
    '&' -> charRefAttValue (attValueUnquoted typ) Nothing tl
    '>' -> neilTagEnd typ tl
    '?' | typ == TypeXml -> neilXmlTagClose tl
    _ | hd `elem` "\"'<=" -> errSeen [hd] & def
    _ | eof -> errWant (if typ == TypeXml then "?>" else ">") & dat s
    _ -> def
    where def = hd & attValueUnquoted typ tl


-- 8.2.4.13 Character reference in attribute value state
charRefAttValue :: Parser -> Maybe Char -> Parser
charRefAttValue resume c s = charRef resume True c s


-- 8.2.4.14 After attribute value (quoted) state
afterAttValueQuoted typ S{..} = pos $ case hd of
    _ | white hd -> beforeAttName typ tl
    '/' -> selfClosingStartTag typ tl
    '>' -> neilTagEnd typ tl
    '?' | typ == TypeXml -> neilXmlTagClose tl
    _ | eof -> dat s
    _ -> errSeen [hd] & beforeAttName typ s


-- 8.2.4.15 Self-closing start tag state
selfClosingStartTag typ S{..} = pos $ case hd of
    _ | typ == TypeXml -> errSeen "/" & beforeAttName typ s
    '>' -> TagEndClose & dat tl
    _ | eof -> errWant ">" & dat s
    _ -> errSeen "/" & beforeAttName typ s


-- 8.2.4.16 Bogus comment state
bogusComment S{..} = Comment & bogusComment1 s
bogusComment1 S{..} = pos $ case hd of
    '>' -> CommentEnd & dat tl
    _ | eof -> CommentEnd & dat s
    _ -> hd & bogusComment1 tl


-- 8.2.4.17 Markup declaration open state
markupDeclOpen S{..} = pos $ case hd of
    _ | Just s <- next "--" -> Comment & commentStart s
    _ | isAlpha hd -> Tag & '!' & hd & tagName TypeDecl tl -- NEIL
    _ | Just s <- next "[CDATA[" -> cdataSection s
    _ -> errWant "tag name" & bogusComment s


-- 8.2.4.18 Comment start state
commentStart S{..} = pos $ case hd of
    '-' -> commentStartDash tl
    '>' -> errSeen "<!-->" & CommentEnd & dat tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> hd & comment tl


-- 8.2.4.19 Comment start dash state
commentStartDash S{..} = pos $ case hd of
    '-' -> commentEnd tl
    '>' -> errSeen "<!--->" & CommentEnd & dat tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> '-' & hd & comment tl


-- 8.2.4.20 Comment state
comment S{..} = pos $ case hd of
    '-' -> commentEndDash tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> hd & comment tl


-- 8.2.4.21 Comment end dash state
commentEndDash S{..} = pos $ case hd of
    '-' -> commentEnd tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> '-' & hd & comment tl


-- 8.2.4.22 Comment end state
commentEnd S{..} = pos $ case hd of
    '>' -> CommentEnd & dat tl
    '-' -> errWant "-->" & '-' & commentEnd tl
    _ | white hd -> errSeen "--" & '-' & '-' & hd & commentEndSpace tl
    '!' -> errSeen "!" & commentEndBang tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> errSeen "--" & '-' & '-' & hd & comment tl


-- 8.2.4.23 Comment end bang state
commentEndBang S{..} = pos $ case hd of
    '>' -> CommentEnd & dat tl
    '-' -> '-' & '-' & '!' & commentEndDash tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> '-' & '-' & '!' & hd & comment tl


-- 8.2.4.24 Comment end space state
commentEndSpace S{..} = pos $ case hd of
    '>' -> CommentEnd & dat tl
    '-' -> commentEndDash tl
    _ | white hd -> hd & commentEndSpace tl
    _ | eof -> errWant "-->" & CommentEnd & dat s
    _ -> hd & comment tl


-- 8.2.4.38 CDATA section state
cdataSection S{..} = pos $ case hd of
    _ | Just s <- next "]]>" -> dat s
    _ | eof -> dat s
    _ | otherwise -> hd & cdataSection tl


-- 8.2.4.39 Tokenizing character references
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
    ';' -> EntityEnd True & resume tl
    _ -> EntityEnd False & errWant ";" & resume s

charRefAlpha resume att S{..} = pos $ case hd of
    _ | isAlpha hd -> EntityName & hd & charRefAlpha2 resume att tl
    _ -> errSeen "&" & '&' & resume s

charRefAlpha2 resume att S{..} = pos $ case hd of
    _ | alphaChar hd -> hd & charRefAlpha2 resume att tl
    ';' -> EntityEnd True & resume tl
    _ | att -> EntityEnd False & resume s
    _ -> EntityEnd False & errWant ";" & resume s


alphaChar x = isAlphaNum x || x `elem` ":-_"

hexChar False x = isDigit x
hexChar True  x = isDigit x || (x >= 'a' && x <= 'f') || (x >= 'A' && x <= 'F')
