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
import qualified Text.StringLike as Str(concat)
import Text.StringLike hiding(concat)
import Debug.Trace


---------------------------------------------------------------------
-- MAIN RUN FUNCTION

parseTags :: String -> [Tag String]
parseTags = parseTagsOptions parseOptions


parseTagsOptions :: ParseOptions String -> String -> [Tag String]
parseTagsOptions opts x = mergeTexts $ runParser tags $ S x nullPosition [] opts


-- | Combine adjacent text nodes.
--
--   If two text nodes are separated only a position node, delete the position.
--   If two text nodes are separated only by a warning, move the warning afterwards.
--   If a position immediately proceeds a warning, count that into the warning.
--
--   Note: this function leaks stack on Hugs.
mergeTexts :: [Tag String] -> [Tag String]
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

data S str = S {string :: str, pos :: !Position, warn :: [(Position,str)], opts :: ParseOptions str}

instance StringLike s => StringLike (S s) where
    uncons s = case uncons (string s) of
        Nothing -> Nothing
        Just (x,xs) -> Just (x, s{string=xs, pos=positionChar (pos s) x})


addWarn :: str -> Parser (S str) ()
addWarn msg = modify $ \s -> 
    if optTagWarning (opts s) then s{warn=(pos s,msg):warn s} else s

outWarn :: Parser (S str) [Tag str] -> Parser (S str) [Tag str]
outWarn p = do
    s <- get
    put s{warn=[]}
    res <- p
    return $ concat [position s{pos=p} [TagWarning w] | (p,w) <- warn s] ++ res

position :: S str -> [Tag str] -> [Tag str]
position s xs | optTagPosition $ opts s = tagPosition (pos s) : xs
              | otherwise = xs

tagPosWarnFix :: ParseOptions str -> [Tag str] -> [Tag str]
tagPosWarnFix opts = if optTagWarning opts then id else filter (not . isTagWarning)

debug p = do
    s <- get
    trace (take 50 $ string s) p

---------------------------------------------------------------------
-- USEFUL UTILITIES

isNameChar x = not $ isSpace x || x `elem` "><&'\"="


nowLit :: StringLike str => String -> Parser (S str) ()
nowLit s = do
    r <- lit s
    when (r == "") $ addWarn $ fromString $ "Expected but not found: " ++ s


name :: StringLike str => Parser (S str) str
name = many isNameChar

nowName :: StringLike str => Parser (S str) str
nowName = do
    r <- name
    when (isEmpty r) $ addWarn $ fromString $ "Expected but not found: a name"
    return r

spaces :: StringLike str => Parser (S str) ()
spaces = do
    x <- many isSpace
    let _ = x :: String
    return ()


---------------------------------------------------------------------
-- THE PARSER

tags :: StringLike str => Parser (S str) [Tag str]
tags = do
    s <- get
    outWarn $ choice $ do
        eof ==> return []
        def ==> do x<-tag ; xs<-tags ; return $ position s $ x ++ xs

tag :: StringLike str => Parser (S str) [Tag str]
tag = choice $ do
    "<!--" ==> comment
    "&" ==> entity
    "</" ==> close
    "<" ==> open
    def ==> text

comment :: StringLike str => Parser (S str) [Tag str]
comment = do res <- takesUntil "-->" ; nowLit "-->" ; return [TagComment res]

entity :: StringLike str => Parser (S str) [Tag str]
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
            return $ tagPosWarnFix (opts s) $ optLookupEntity (opts s) $
                fromString prefix `append` x

close :: StringLike str => Parser (S str) [Tag str]
close = do spaces ; res<-nowName ; spaces ; nowLit ">" ; return [TagClose res]

open :: StringLike str => Parser (S str) [Tag str]
open = do spaces ; x<-nowName ; spaces ; xs<-atts x; return $ TagOpen x (fst xs) : snd xs

atts :: StringLike str => str -> Parser (S str) ([(str,str)],[Tag str])
atts param = choice $ do
    "/>" ==> return ([],[TagClose param])
    ">"  ==> return ([],[])
    "\"" ==> do y<-str "\"" ; spaces ; res<-atts param ; return $ first ((empty,y):) res
    "\'" ==> do y<-str "\'" ; spaces ; res<-atts param ; return $ first ((empty,y):) res
    def ==> do
        x<-name
        if isEmpty x
            then do nowLit ">" ; return ([],[])
            else do spaces ; y<-attEq ; spaces ; res<-atts param; return $ first ((x,y):) res

-- ="bar"
attEq :: StringLike str => Parser (S str) str
attEq = choice $ do
    "=" ==> do spaces ; attQuote
    def ==> do spaces ; return empty

-- "bar"
attQuote :: StringLike str => Parser (S str) str
attQuote = choice $ do
    "\"" ==> str "\""
    "'"  ==> str "'"
    def ==> name

-- bar"
str :: StringLike str => String -> Parser (S str) str
str param = choice $ do
    eof ==> do nowLit param ; return empty
    param ==> return empty
    "&" ==> do x<-entity ; xs<-str param ; return $ innerText x `append` xs
    def ==> do x <- many (`notElem` ("&"++param)) ; xs<-str param ; return $ x `append` xs

text :: StringLike str => Parser (S str) [Tag str]
text = do res <- many (`notElem` "<&") ; return [TagText res]
