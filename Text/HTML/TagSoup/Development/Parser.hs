{-
The XML spec is done mainly from memory. The only special bits are:

Attributes
----------

"foo" as an attribute comes out as an attribute with a value and no text
to allow DOCTYPE tags to get parsed as normal.

References (aka Character and Entity References)
----------

A character reference is one of:

entity = '&#' [0-9]+ ';' 
       | '&#x' [0-9a-fA-F]+ ';'
       | '&' name ';'

The maximum character reference is 0x10FFFF

name = (letter | '_' | ':') (namechar)*
namechar = letter | digit | '.' | '-' | '_' | ':' | combiningchar | extender
combiningchar and extender are assumed to be empty
letter = isAlpha
digit = isDigit
-}


module Text.HTML.TagSoup.Development.Parser(
    parseTags, parseTagsOptions,
    ParseOptions(..), parseOptions
    ) where

import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Entity
import Text.HTML.TagSoup.Development.ParserUtils

import Data.Char
import Data.List
import Data.Maybe



---------------------------------------------------------------------
-- * ParseOptions

-- important: '"&<>/\ and space must not be in this list
isNameChar x = isAlphaNum x || x `elem` "-_:.!?"

data ParseOptions = ParseOptions
    {optTagPosition :: Bool -- ^ Should 'TagPosition' values be given before every item
    ,optTagWarning :: Bool -- ^ Should 'TagWarning' values be given
    ,optLookupEntity :: String -> [Tag] -- ^ How to lookup an entity
    ,optMaxEntityLength :: Maybe Int -- ^ /DEPRECATED/: Always ignored
    }

{-# DEPRECATED optMaxEntityLength "This option is now always ignored" #-}


-- Default 'ParseOptions' structure
parseOptions :: ParseOptions
parseOptions = ParseOptions False False f (Just 10)
    where
        f x = case lookupEntity x of
                  Nothing -> [TagText $ "&" ++ x ++ ";", TagWarning $ "Unknown entity: &" ++ x ++ ";"]
                  Just x -> [TagText [x]]


parseTags :: String -> [Tag]
parseTags = parseTagsOptions parseOptions

tagWarn :: ParseOptions -> String -> [Tag]
tagWarn opts x = [TagWarning x | optTagWarning opts]


parseTagsOptions :: ParseOptions -> String -> [Tag]
parseTagsOptions o = runParser (tag o)



tag :: ParseOptions -> Parser [Tag]
tag o = pick
    ["<!--" |-> comment   o & \ ~(a,b)   -> TagComment a : b
    ,"</"   |-> close     o & \ ~(a,b)   -> TagClose a : b
    ,"<"    |-> open      o & \ ~(a,b,c) -> TagOpen a b : c
    ,"&"    |-> entityTag o
    ,cons $ text o
    ,nil [] ]


-- the first list of tags may be denormalised, the second is normal
-- combine TagText as necessary, delete TagPos and TagWarn (if necessary)
tagJoin :: ParseOptions -> [Tag] -> [Tag] -> [Tag]
tagJoin o (TagText x:xs) ys = TagText (x++text) : rest
    where (text,rest) = textRest $ tagJoin o xs ys
tagJoin o (TagWarning{}:xs) ys | not $ optTagWarning o = tagJoin o xs ys
tagJoin o (x:xs) ys = x : tagJoin o xs ys
tagJoin o [] ys = ys


textRest :: [Tag] -> (String, [Tag])
textRest (TagText xs : rest) = (xs, rest)
textRest rest = ("", rest)


text :: ParseOptions -> Char -> PState -> [Tag]
text o x s = TagText (x:xs) : rest
    where (xs,rest) = textRest $ tag o s


close :: ParseOptions -> Parser (String,[Tag])
close o s0 = (name, rest)
    where
        (name,s1) = spanS isNameChar s0
        s2 = dropWhileS isSpace s1

        rest = pick [">" |-> tag o
                    ,"" |-> tag o & (tagWarn o msg ++)] s2
        msg = "Failed to find \">\" in closing tag"


comment :: ParseOptions -> Parser (String,[Tag])
comment o s0 = (text, if found then tag o s1 else tagWarn o msg)
    where
        (text, s1, found) = spanEndS "-->" s0
        msg = "Failed to find \"-->\" in comment"


entityTag :: ParseOptions -> Parser [Tag]
entityTag o s = tagJoin o text $ tag o rest
    where (text,rest) = entity o s


entityText :: ParseOptions -> Parser (String,PState)
entityText o s = (concat [s | TagText s <- text], rest)
    where (text,rest) = entity o s


entity :: ParseOptions -> Parser ([Tag],PState)
entity o = pick ["#x" |-> f "#x" isHexDigit
                ,"#"  |-> f "#"  isDigit
                ,""   |-> f ""   isNameChar]
    where
        -- should be "rest" here
        f prefix test s0 = (optLookupEntity o (prefix ++ text) ++ warn, s2)
            where
                (text,s1) = spanS test s0 
                (warn,s2) = pick [";" |-> \s -> ([], s)
                                 ,""  |-> \s -> (tagWarn o msg, s)] s1
        msg = "Failed to find \";\" in entity"


open :: ParseOptions -> Parser (String, [(String,String)], [Tag])
open o s0 = (name, atts, warn ++ rest)
    where
        (name,s1) = spanS isNameChar s0
        (atts, rest) = attribs o name s1
        warn = [] -- if name is null


attribs :: ParseOptions -> String -> Parser ([(String,String)], [Tag])
attribs o name = pick [">" |-> tag o & \a -> ([], a)
                      ,"/>" |-> tag o & \a -> ([], TagClose name : a)
                      ,nil ([], tagWarn o "Failed to find \">\" in open tag")
                      ,"" |-> attrib o & \ ~(a,s) -> let ~(as,b) = attribs o name s in (a:as,b) 
                      ] . dropWhileS isSpace


attrib :: ParseOptions -> Parser ((String,String), PState)
attrib o s0 = ((name,val), s3)
    where
        (name,s1) = spanS isNameChar s0
        s2 = dropWhileS isSpace s1
        (val,s3) = pick ["=" |-> value o . dropWhileS isSpace
                        ,"" |-> \s -> ("",s)] s2


value :: ParseOptions -> Parser (String, PState)
value o = pick ["\"" |-> f (== '\"')
               ,"\'" |-> f (== '\'')
               ,""   |-> f isSpace]
    where
        f end s = pick [nil ("",s)
                       ,"&" |-> entityText o & \ ~(a,b) -> g a b
                       ,cons $ \c s -> if end c then ("",s) else g [c] s] s
            where g str sn = let ~(a,b) = f end sn in (str++a,b)
