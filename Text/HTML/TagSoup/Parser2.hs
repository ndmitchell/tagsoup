{-# OPTIONS_GHC -O2 #-}

module Text.HTML.TagSoup.Parser2(
    parseTags, parseTagsOptions,
    ParseOptions(..), parseOptions
    ) where

import Text.ParserCombinators.LazyParse
import Data.Char
import Control.Monad
import Control.Arrow
import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Options
import Debug.Trace


---------------------------------------------------------------------
-- MAIN RUN FUNCTION

parseTags :: String -> [Tag]
parseTags = parseTagsOptions parseOptions


parseTagsOptions :: ParseOptions -> String -> [Tag]
parseTagsOptions opts x = mergeTexts $ runParser tags $ S x nullPosition [] opts


-- | Combine adjacent text nodes.
--
--   If two text nodes are separated only a position node, delete the position.
--   If two text nodes are separated only by a warning, move the warning afterwards.
--   If a position immediately proceeds a warning, count that into the warning.
--
--   Note: this function leaks stack on Hugs.
mergeTexts :: [Tag] -> [Tag]
mergeTexts (TagText x:xs) = (TagText $ concat $ x:texts) : warns ++ mergeTexts rest
    where
        (texts,warns,rest) = f xs

        f (TagText x:xs) = (x:a,b,c)
            where (a,b,c) = f xs
        f (TagPosition _ _:TagText x:xs) = (x:a,b,c)
            where (a,b,c) = f xs

        f (p@TagPosition{}:TagWarning y:xs) = (a,p:TagWarning y:b,c)
            where (a,b,c) = f xs
        f (TagWarning x:xs) = (a,TagWarning x:b,c)
            where (a,b,c) = f xs

        f xs = ([],[],xs)

mergeTexts (x:xs) = x : mergeTexts xs
mergeTexts [] = []



---------------------------------------------------------------------
-- PARAMETERISATION

data S = S {string :: String, pos :: !Position, warn :: [(Position,String)], opts :: ParseOptions}

instance Parse S where
    un s = case string s of
        [] -> Nothing
        x:xs -> Just (x, s{string=xs, pos=positionChar (pos s) x})


addWarn :: String -> Parser S ()
addWarn msg = modify $ \s -> 
    if optTagWarning (opts s) then error "here" else s -- s{warn=(pos s,msg):warn s} else s

outWarn :: Parser S [Tag] -> Parser S [Tag]
outWarn p = do
    s <- get
    put s{warn=[]}
    res <- p
    return $ concat [position s{pos=p} [TagWarning w] | (p,w) <- warn s] ++ res

position :: S -> [Tag] -> [Tag]
position s xs | optTagPosition $ opts s = tagPosition (pos s) : xs
              | otherwise = xs

tagPosWarnFix :: ParseOptions -> [Tag] -> [Tag]
tagPosWarnFix opts = if optTagWarning opts then id else filter (not . isTagWarning)

debug p = do
    s <- get
    trace (take 50 $ string s) p

---------------------------------------------------------------------
-- USEFUL UTILITIES

isNameChar x = not $ isSpace x || x `elem` "><&'\"="


nowLit :: String -> Parser S ()
nowLit s = do
    r <- lit s
    when (null r) $ addWarn $ "Expected but not found: " ++ s


name :: Parser S String
name = many isNameChar

nameNow = do
    r <- name
    when (null r) $ addWarn $ "Expected but not found: a name"


---------------------------------------------------------------------
-- THE PARSER

tags :: Parser S [Tag]
tags = do
    s <- get
    outWarn $ choice $ do
        eof ==> return []
        def ==> do x<-tag ; xs<-tags ; return $ position s $ x ++ xs

tag = choice $ do
    "<!--" ==> comment
    "&" ==> entity
    "</" ==> close
    "<" ==> open
    def ==> text

comment :: Parser S [Tag]
comment = do res <- takesUntil "-->" ; nowLit "-->" ; return [TagComment res]

entity :: Parser S [Tag]
entity = do
    res <- choice $ do
        "#x" ==> many isHexDigit >>= entityName "#x"
        "#" ==> many isDigit >>= entityName "#"
        def ==> many isAlphaNum >>= entityName ""
    nowLit ";"
    return res
    where
        entityName prefix x = do
            s <- get
            return $ tagPosWarnFix (opts s) $ optLookupEntity (opts s) (prefix ++ x)

close = do spaces ; res<-name ; spaces ; nowLit ">" ; return [TagClose res]

open = do spaces ; b<-one (=='!') ; x<-name ; spaces ; xs<-atts x; return $ TagOpen (b++x) (fst xs) : snd xs

atts :: String -> Parser S ([(String,String)],[Tag])
atts param = choice $ do
    "/>" ==> return ([],[TagClose param])
    ">"  ==> return ([],[])
    "\"" ==> do y<-str "\"" ; spaces ; res<-atts param ; return $ first (("",y):) res
    "\'" ==> do y<-str "\'" ; spaces ; res<-atts param ; return $ first (("",y):) res
    def ==> do
        x<-name
        if not $ null x
            then do spaces ; y<-attEq ; spaces ; res<-atts param; return $ first ((x,y):) res
            else do nowLit ">" ; return ([],[])

-- ="bar"
attEq = choice $ do
    "=" ==> do spaces ; attQuote
    def ==> do spaces ; return ""

-- "bar"
attQuote = choice $ do
    "\"" ==> str "\""
    "'"  ==> str "'"
    def ==> name

-- bar"
str param = choice $ do
    eof ==> do nowLit param ; return []
    param ==> return []
    "&" ==> do x<-entity ; xs<-str param ; return $ innerText x ++ xs
    def ==> do x <- many (`notElem` ("&"++param)) ; xs<-str param ; return $ x++xs

text :: Parser S [Tag]
text = do res <- many (`notElem` "<&") ; return [TagText res]
