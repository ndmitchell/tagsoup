{-# LANGUAGE RecordWildCards, PatternGuards #-}

module Spec(parse) where

import Implementation
import Data.Char


white x = x `elem` "\t\n\f "

-- We make three generalisations:
--   <!name is a valid tag start closed by >
--   <?name is a valid tag start closed by ?>
--   <a "foo"> is a valid tag attibute, i.e missing an attribute name
-- All these parts are annotated with NEIL


-- 9.2.4 Tokenization

type Parser = S -> [Out]

parse :: String -> [Out]
parse = dat . state 

-- 9.2.4.1 Data state
dat :: Parser
dat S{..} = case hd of
    '&' | pcdata_rcdata, noescape -> charRef tl
    '-' | rcdata_cdata, noescape, before "<--" -> hd & dat (escape_ tl)
    '<' | pcdata -> tagOpen tl
    '<' | rcdata_cdata, noescape -> tagOpen tl
    '>' | rcdata_cdata, escape, before "--" -> hd & dat (noescape_ tl)
    _ | eof -> []
    _ -> hd & dat tl


-- 9.2.4.2 Character reference data state
charRef s = case charReference Nothing s of
    Nothing -> '&' & dat s
    Just (s,x) -> x & dat s


-- 9.2.4.3 Tag open state
tagOpen S{..} = case hd of
    _ | rcdata_cdata ->
        if hd == '/' then closeTagOpen tl else '<' & dat s
    '!' -> markupDeclOpen tl
    '/' -> closeTagOpen tl
    _ | isAlpha hd -> TagOpen & hd & tagName False tl
    '>' -> err & '<' & '>' & dat tl
    '?' -> neilXmlTagOpen tl -- NEIL
    _ -> err & '<' & dat s


-- seen "<?", emitted []
neilXmlTagOpen S{..} = case hd of
    _ | isAlpha hd -> TagOpen & '?' & hd & tagName True tl
    _ -> err & '<' & '?' & dat s

-- seen "?", expecting ">"
neilXmlTagClose S{..} = case hd of
    '>' -> TagEnd & dat tl
    _ -> err & beforeAttName True s


-- just seen ">" at the end, am given tl
neilTagEnd xml S{..}
    | xml = err & TagEnd & dat s
    | otherwise = TagEnd & dat s


-- 9.2.4.4 Close tag open state
-- Deviation: We ignore the if CDATA/RCDATA bits and tag matching
closeTagOpen S{..} = case hd of
    _ | isAlpha hd -> TagShut & hd & tagName False tl
    '>' -> err & dat tl
    _ | eof -> '<' & '/' & dat s
    _ -> err & bogusComment s


-- 9.2.4.5 Tag name state
tagName xml S{..} = case hd of
    _ | white hd -> beforeAttName xml tl
    '/' -> selfClosingStartTag xml tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | isAlpha hd -> hd & tagName xml tl
    _ | eof -> err & dat s
    _ -> hd & tagName xml tl


-- 9.2.4.6 Before attribute name state
beforeAttName xml S{..} = case hd of
    _ | white hd -> beforeAttName xml tl
    '/' -> selfClosingStartTag xml tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | hd `elem` "\'\"" -> beforeAttValue xml s -- NEIL
    _ | hd `elem` "\"'<=" -> err & AttName & hd & attName xml tl
    _ | eof -> err & dat s
    _ -> AttName & hd & attName xml tl


-- 9.2.4.7 Attribute name state
attName xml S{..} = case hd of
    _ | white hd -> afterAttName xml tl
    '/' -> selfClosingStartTag xml tl
    '=' -> beforeAttValue xml tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | hd `elem` "\"'<" -> err & def
    _ | eof -> err & dat s
    _ -> def
    where def = hd & attName xml tl


-- 9.2.4.8 After attribute name state
afterAttName xml S{..} = case hd of
    _ | white hd -> afterAttName xml tl
    '/' -> selfClosingStartTag xml tl
    '=' -> beforeAttValue xml tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | hd `elem` "\"'<" -> err & def
    _ | eof -> err & dat s
    _ -> def
    where def = AttName & hd & attName xml tl

-- 9.2.4.9 Before attribute value state
beforeAttValue xml S{..} = case hd of
    _ | white hd -> beforeAttValue xml tl
    '\"' -> AttVal & attValueDQuoted xml tl
    '&' -> AttVal & attValueUnquoted xml s
    '\'' -> AttVal & attValueSQuoted xml tl
    '>' -> err & neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | hd `elem` "<=" -> err & def
    _ | eof -> err & dat s
    _ -> def
    where def = AttVal & hd & attValueUnquoted xml tl


-- 9.2.4.10 Attribute value (double-quoted) state
attValueDQuoted xml S{..} = case hd of
    '\"' -> afterAttValueQuoted xml tl
    '&' -> charRefAttValue (attValueDQuoted xml) (Just '\"') tl
    _ | eof -> err & dat s
    _ -> hd & attValueDQuoted xml tl


-- 9.2.4.11 Attribute value (single-quoted) state
attValueSQuoted xml S{..} = case hd of
    '\'' -> afterAttValueQuoted xml tl
    '&' -> charRefAttValue (attValueSQuoted xml) (Just '\'') tl
    _ | eof -> err & dat s
    _ -> hd & attValueSQuoted xml tl


-- 9.2.4.12 Attribute value (unquoted) state
attValueUnquoted xml S{..} = case hd of
    _ | white hd -> beforeAttName xml tl
    '&' -> charRefAttValue (attValueUnquoted xml) Nothing tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | hd `elem` "\"'<=" -> err & def
    _ | eof -> err & dat s
    _ -> def
    where def = hd & attValueUnquoted xml tl


-- 9.2.4.13 Character reference in attribute value state
charRefAttValue :: Parser -> Maybe Char -> Parser
charRefAttValue resume c s = case charReference c s of
    Nothing -> '&' & resume s
    Just (s, x) -> x & resume s


-- 9.2.4.14 After attribute value (quoted) state
afterAttValueQuoted xml S{..} = case hd of
    _ | white hd -> beforeAttName xml tl
    '/' -> selfClosingStartTag xml tl
    '>' -> neilTagEnd xml tl
    '?' | xml -> neilXmlTagClose tl
    _ | eof -> dat s
    _ -> err & beforeAttName xml s


-- 9.2.4.15 Self-closing start tag state
selfClosingStartTag xml S{..} = case hd of
    _ | xml -> err & beforeAttName xml s
    '>' -> TagEndClose & dat tl
    _ | eof -> err & dat s
    _ -> err & beforeAttName xml s


-- 9.2.4.16 Bogus comment state
bogusComment S{..} = CommentOpen & bogusComment1 s
bogusComment1 S{..} = case hd of
    '>' -> CommentEnd & dat tl
    _ | eof -> CommentEnd & dat s
    _ -> hd & bogusComment1 tl


-- 9.2.4.17 Markup declaration open state
markupDeclOpen S{..} = case hd of
    _ | after "--" -> CommentOpen & commentStart (drp 2)
    _ | isAlpha hd -> TagOpen & '!' & hd & tagName False tl -- NEIL
    _ | after "[CDATA[" -> cdataSection (drp 7)
    _ -> err & bogusComment s


-- 9.2.4.18 Comment start state
commentStart S{..} = case hd of
    '-' -> commentStartDash tl
    '>' -> err & CommentEnd & dat tl
    _ | eof -> err & CommentEnd & dat s
    _ -> hd & comment tl


-- 9.2.4.19 Comment start dash state
commentStartDash S{..} = case hd of
    '-' -> commentEnd tl
    '>' -> err & CommentEnd & dat tl
    _ | eof -> err & CommentEnd & dat s
    _ -> '-' & hd & comment tl


-- 9.2.4.20 Comment state
comment S{..} = case hd of
    '-' -> commentEndDash tl
    _ | eof -> err & CommentEnd & dat s
    _ -> hd & comment tl


-- 9.2.4.21 Comment end dash state
commentEndDash S{..} = case hd of
    '-' -> commentEnd tl
    _ | eof -> err & CommentEnd & dat s
    _ -> '-' & hd & comment tl


-- 9.2.4.22 Comment end state
commentEnd S{..} = case hd of
    '>' -> CommentEnd & dat tl
    '-' -> err & '-' & commentEnd tl
    _ | white hd -> err & '-' & '-' & hd & commentEnd tl
    '!' -> err & commentEndBang tl
    _ | eof -> err & CommentEnd & dat s
    _ -> err & '-' & '-' & hd & comment tl


-- 9.2.4.23 Comment end bang state
commentEndBang S{..} = case hd of
    '>' -> CommentEnd & dat tl
    '-' -> '-' & '-' & '!' & commentEndDash tl
    _ | eof -> err & CommentEnd & dat s
    _ -> '-' & '-' & '!' & hd & comment tl


-- 9.2.4.24 Comment end space state
commentEndSpace S{..} = case hd of
    '>' -> CommentEnd & dat tl
    '-' -> commentEndDash tl
    _ | white hd -> hd & commentEndSpace tl
    _ | eof -> err & CommentEnd & dat s
    _ -> hd & comment tl


-- 9.2.4.38 CDATA section state
cdataSection S{..} = case hd of
    _ | after "]]>" -> dat (drp 3)
    _ | eof -> dat s
    _ | otherwise -> hd & cdataSection tl


-- 9.2.4.39 Tokenizing character references
charReference :: Maybe Char -> S -> Maybe (S, Out)
charReference = undefined

