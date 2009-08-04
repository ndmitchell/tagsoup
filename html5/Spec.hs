{-# LANGUAGE RecordWildCards, PatternGuards #-}

module Spec where

import Implementation
import Data.Char


white x = x `elem` "\t\n\f "


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
    _ | isAlpha hd -> TagOpen & hd & tagName tl
    '>' -> err & '<' & '>' & dat tl
    '?' -> err & bogusComment tl
    _ -> err & '<' & dat s


-- 9.2.4.4 Close tag open state
-- Deviation: We ignore the if CDATA/RCDATA bits and tag matching
closeTagOpen S{..} = case hd of
    _ | isAlpha hd -> TagShut & hd & tagName tl
    '>' -> err & dat tl
    _ | eof -> '<' & '/' & dat s
    _ -> err & bogusComment tl


-- 9.2.4.5 Tag name state
tagName S{..} = case hd of
    _ | white hd -> TagNameEnd & beforeAttName tl
    '/' -> TagNameEnd & selfClosingStartTag
    '>' -> TagEnd & dat tl
    _ | isAlpha hd -> hd & tagName tl
    _ | eof -> err & dat s
    _ -> hd & tagName tl


-- 9.2.4.6 Before attribute name state
beforeAttName S{..} = case hd of
    _ | white hd -> beforeAttName tl
    '/' -> selfClosingStartTag tl
    '>' -> TagEnd & dat tl
    _ | hd `elem` "\"'<=" -> err & AttName & hd & attName tl
    _ | eof -> err & dat s
    _ -> AttName & hd & attName tl


-- 9.2.4.7 Attribute name state
attName S{..} = case hd of
    _ | white hd -> afterAttName tl
    '/' -> selfClosingStartTag tl
    '=' -> beforeAttValue tl
    '>' -> TagEnd & dat tl
    _ | hd `elem` "\"'<" -> err & def
    _ | eof -> err & dat s
    _ -> def
    where def = hd & attName tl


-- 9.2.4.8 After attribute name state
afterAttName S{..} = case hd of
    _ | white hd -> afterAttName tl
    '/' -> selfClosingStartTag tl
    '=' -> beforeAttValue tl
    '>' -> TagEnd & dat tl
    _ | hd `elem` "\"'<" -> err & def
    _ | eof -> err & dat s
    _ -> def
    where def = AttName & hd & attName tl

-- 9.2.4.9 Before attribute value state
beforeAttValue S{..} = case hd of
    _ | white hd -> beforeAttValue tl
    '\"' -> AttVal & attValueDQuoted tl
    '&' -> AttVal & attValueUnquoted s
    '\'' -> AttVal & attValueSQuoted tl
    '>' -> err & TagEnd & dat tl
    _ | hd `elem` "<=" -> err & def
    _ | eof -> err & dat s
    _ -> def
    where def = AttVal & hd & attValueUnquoted tl


-- 9.2.4.10 Attribute value (double-quoted) state
attValueDQuoted S{..} = case hd of
    '\"' -> afterAttValueQuoted tl
    '&' -> charRefAttValue attValueDQuoted (Just '\"') tl
    _ | eof -> err & dat s
    _ -> hd & attValueDQuoted tl


-- 9.2.4.11 Attribute value (single-quoted) state
attValueSQuoted S{..} = case hd of
    '\'' -> afterAttValueQuoted tl
    '&' -> charRefAttValue attValueSQuoted (Just '\'') tl
    _ | eof -> err & dat s
    _ -> hd & attValueSQuoted tl


-- 9.2.4.12 Attribute value (unquoted) state
attValueUnquoted S{..} = case hd of
    _ | white hd -> beforeAttName tl
    '&' -> charRefAttValue attValueUnquoted Nothing tl
    '>' -> TagEnd & dat tl
    _ | hd `elem` "\"'<=" -> err & def
    _ | eof -> err & dat s
    _ -> def
    where def = hd & attValueUnquoted tl


-- 9.2.4.13 Character reference in attribute value state
charRefAttValue :: Parser -> Maybe Char -> Parser
charRefAttValue resume c s = case charReference c s of
    Nothing -> '&' & resume s
    Just (s, x) -> x & resume s


-- 9.2.4.14 After attribute value (quoted) state
afterAttValueQuoted S{..} = case hd of
    _ | white hd -> beforeAttName tl
    '/' -> selfClosingStartTag tl
    '>' -> TagEnd & dat tl
    _ | eof -> dat s
    _ -> err & beforeAttName s


-- 9.2.4.17 Markup declaration open state
markupDeclOpen = error "markupDeclOpen"

bogusComment = error "bogusComment"

selfClosingStartTag = undefined



-- 9.2.4.37 Bogus DOCTYPE state
bogusDocType S{..} = case hd of
    '>' -> DocTypeEnd & dat tl
    _ | eof -> DocTypeEnd & dat s
    _ -> bogusDocType tl


-- 9.2.4.38 CDATA section state
cdataSection S{..} = case hd of
    _ | after "]]>" -> dat (drp 3)
    _ | eof -> dat s
    _ | otherwise -> hd & cdataSection tl


-- 9.2.4.39 Tokenizing character references
charReference :: Maybe Char -> S -> Maybe (S, Out)
charReference = undefined

