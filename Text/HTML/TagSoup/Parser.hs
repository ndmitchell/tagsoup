{-# OPTIONS_GHC -w #-}

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


module Text.HTML.TagSoup.Parser(
    parseTags, parseTagsOptions,
    ParseOptions(..), parseOptions
    ) where

import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Entity
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe


infix 9 ?->

(?->) :: Bool -> [x] -> [x]
(?->) b true = if b then true else []

---------------------------------------------------------------------
-- * ParseOptions

data ParseOptions = ParseOptions
    {optTagPosition :: Bool -- ^ Should 'TagPosition' values be given before every item
    ,optTagWarning :: Bool -- ^ Should 'TagWarning' values be given
    ,optLookupEntity :: String -> [Tag] -- ^ How to lookup an entity
    ,optMaxEntityLength :: Maybe Int -- ^ The maximum length of an entities content
                                     --   (Nothing for no maximum, default to 10)
    }


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

---------------------------------------------------------------------
-- * Positions

-- All positions are stored as a row and a column, with (1,1) being the
-- top-left position

data Position = Position !Row !Column


updateOnString :: Position -> String -> Position
updateOnString = foldl' updateOnChar

updateOnChar   :: Position -> Char -> Position
updateOnChar (Position r c) x = case x of
    '\n' -> Position (r+1) c
    '\t' -> Position r (c + 8 - mod (c-1) 8)
    _    -> Position r (c+1)


tagPos :: ParseOptions -> Position -> [Tag]
tagPos opts (Position r c) = [TagPosition r c | optTagPosition opts]

tagPosWarn :: ParseOptions -> Position -> String -> [Tag]
tagPosWarn opts p x = optTagWarning opts ?-> (tagPos opts p ++ [TagWarning x])

tagPosWarnFix :: ParseOptions -> Position -> [Tag] -> [Tag]
tagPosWarnFix opts p = addPositions . remWarnings
    where
        remWarnings = if optTagWarning opts then id else filter (not . isTagWarning)
        addPositions = concatMap (\x -> tagPos opts p ++ [x])
    

---------------------------------------------------------------------
-- * Driver

parseTagsOptions :: ParseOptions -> String -> [Tag]
parseTagsOptions opts x = mergeTexts $ evalState (parse opts) $ Value x (Position 0 0)


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
-- * Combinators

data Value = Value String !Position

type Parser a = State Value a


isNameCharFirst x = isAlphaNum x || x `elem` "_:"
isNameChar x = isAlphaNum x || x `elem` "-_:."

-- Read and consume n characters from the stream, updating the position.
-- Highly likely to be a potential for space leaks from this, unless @n@ is bounded.
consume :: Int -> Parser ()
consume n = do
    Value s p <- get
    let (a,b) = splitAt n s
    put $ Value b (updateOnString p a)


-- Break once an end string is encountered.
-- Return the string before, and a boolean indicating if the end was matched.
breakOn :: String -> Parser (String,Bool)
breakOn end = do
    Value s p <- get
    if null s then
        return ("",True)
     else if end `isPrefixOf` s then
        consume (length end) >> return ("",False)
     else do
        consume 1
        ~(a,b) <- breakOn end
        return (head s:a,b)

-- Break after an HTML name (entity, attribute or tag name)
-- Note: potential space leak with consume at the end
breakName :: Parser String
breakName = do
    Value s p <- get
    if not (null s) && isNameCharFirst (head s) then do
        let (a,b) = span isNameChar s
        consume (length a)
        return a
     else
        return ""

-- Break after a number has been read
-- Note: potential space leak with consume at the end
breakNumber :: Parser (Maybe Int)
breakNumber = do
    Value s p <- get
    if not (null s) && isDigit (head s) then do
        let (a,b) = span isDigit s
        consume (length a)
        return $ Just $ read a
     else
        return Nothing


-- Drop a number of spaces
-- Note: potential space leak with consume at the end
dropSpaces :: Parser ()
dropSpaces = do
    Value s p <- get
    let n = length $ takeWhile isSpace s
    consume n


---------------------------------------------------------------------
-- * Parser

parse :: ParseOptions -> Parser [Tag]
parse opts = do
    Value s p <- get
    case s of
        '<':'!':'-':'-':_ -> consume 4 >> comment opts p
        '<':'/':_         -> consume 2 >> close opts p
        '<':_             -> consume 1 >> open opts p
        []                -> return []
        '&':_             -> do
            consume 1
            s <- entity opts p
            rest <- parse opts
            return $ s ++ rest
        s:ss              -> do
            consume 1
            rest <- parse opts
            return $ tagPos opts p ++ [TagText [s]] ++ rest

-- have read "<!--"
comment opts p1 = do
    ~(inner,bad) <- breakOn "-->"
    rest <- parse opts
    return $ tagPos opts p1 ++ [TagComment inner] ++
             (bad ?-> tagPosWarn opts p1 "Unexpected end when looking for \"-->\"") ++
             rest


close opts p1 = do
        name <- breakName
        dropSpaces
        ~(Value s p) <- get
        rest <- f s
        return $ tagPos opts p1 ++ [TagClose name] ++
                 (null name ?-> tagPosWarn opts p1 "Empty name in close tag") ++
                 rest
    where
        f ('>':s) = do
            consume 1
            rest <- parse opts
            return rest

        f _ = do
            ~(_,bad) <- breakOn ">"
            rest <- parse opts
            return $ tagPosWarn opts p1 "Junk in closing tag" ++
                     bad ?-> tagPosWarn opts p1 "Unexpected end when looking for \">\"" ++
                     rest


-- an open tag, perhaps <? or <!
open opts p1 = do
    Value s p <- get
    prefix <- if take 1 s `elem` ["!","?"] then consume 1 >> return [head s] else return ""
    name <- liftM (prefix++) breakName
    if null name then do
        rest <- parse opts
        return $ tagPos opts p1 ++ [TagText ('<':prefix)] ++ tagPosWarn opts p1 "Expected name of tag" ++ rest
     else do
        ~(atts,shut,warns) <- attribs opts p1
        rest <- parse opts
        return $ tagPos opts p1 ++ [TagOpen name atts] ++
                 shut ?-> (tagPos opts p1 ++ [TagClose name]) ++
                 warns ++ rest


-- read a list of attributes
-- return (the attributes read, if the tag is self-shutting, any warnings) 
attribs :: ParseOptions -> Position -> Parser ([Attribute],Bool,[Tag])
attribs opts p1 = do
    dropSpaces
    Value s p <- get
    case s of
        '/':'>':_ -> consume 2 >> return ([],True ,[])
        '>':_     -> consume 1 >> return ([],False,[])
        x:xs | x `elem` "'\"" -> do
            ~(val,warns1) <- value opts
            ~(atts,shut,warns2) <- attribs opts p1
            return (("",val):atts,shut,warns1++warns2)
        []        -> return ([],False,tagPosWarn opts p1 "Unexpected end when looking for \">\"")
        _ -> attrib opts p1


-- read a single attribute
-- return (the attributes read, if the tag is self-shutting, any warnings) 
attrib :: ParseOptions -> Position -> Parser ([Attribute],Bool,[Tag])
attrib opts p1 = do
    name <- breakName
    if null name then do
        consume 1
        ~(atts,shut,warns) <- attribs opts p1
        return (atts,shut,tagPosWarn opts p1 "Junk character in tag" ++ warns)
     else do
        ~(Value sold p) <- get
        dropSpaces
        ~(Value s p) <- get
        ~(val,warns1) <- f sold s
        ~(atts,shut,warns2) <- attribs opts p1
        return ((name,val):atts,shut,warns1++warns2)
    where
        f sold ('=':s) = consume 1 >> dropSpaces >> value opts
        f sold s | not $ junk sold = return ([], [])
                 | otherwise = do
                      ~(Value s p) <- get
                      dropJunk
                      return ([], tagPosWarn opts p "Junk character in tag")

        junk ('/':'>':_) = False
        junk ('>':_) = False
        junk (c:cs) | not $ isSpace c = True
        junk _ = False
        
        dropJunk = do
            ~(Value s p) <- get
            when (junk s) $ consume 1 >> dropJunk


-- read a single value
-- return (value,warnings)
value :: ParseOptions -> Parser (String,[Tag])
value opts = do
    Value s p <- get
    case s of
        '\"':_ -> consume 1 >> f p True "\""
        '\'':_ -> consume 1 >> f p True "\'"
        _ -> f p False ">"
    where
        f p1 quote end = do
            Value s p <- get
            case s of
                '&':_ -> do
                    consume 1
                    ~(cs1,warns1) <- entityString opts p
                    ~(cs2,warns2) <- f p1 quote end
                    return (cs1++cs2,warns1++warns2)
                '/':'>':_ | not quote -> do
                    return ([],[])
                c:_ | c `elem` end || (not quote && isSpace c) -> do
                    if quote then consume 1 else return ()
                    return ([],[])
                c:_ -> do
                    consume 1
                    ~(cs,warns) <- f p1 quote end
                    return (c:cs,warns)
                [] -> return ([],tagPosWarn opts p1 "Unexpected end in attibute value")



-- have seen an &, and have consumed it
-- return a [Tag] to go in a tag stream
entity :: ParseOptions -> Position -> Parser [Tag]
entity opts p1 = do
    Value s p <- get
    case s of
        '#':'x':_ -> f "#x" isHexDigit 
        '#':_     -> f "#"  isDigit
        _         -> f ""   isNameChar
    where
        f prefix match = do
            consume (length prefix)
            g match (reverse prefix) (fromMaybe maxBound (optMaxEntityLength opts))
        
        g match buf bound | bound < 0 = return $
            tagPos opts p1 ++ [TagText ('&':reverse buf)] ++
            tagPosWarn opts p1 "Unexpected '&' not in an entity"

        g match buf bound = do
            Value s p <- get
            case s of
                ';':_ -> do
                    consume 1
                    return $ tagPosWarnFix opts p1 $ optLookupEntity opts (reverse buf)
                x:xs | match x -> consume 1 >> g match (x:buf) (bound-1) 
                _ -> g match buf (-1)
            


-- return the tag and some position information
entityString :: ParseOptions -> Position -> Parser (String,[Tag])
entityString opts p = do
    tags <- entity opts p
    let warnings = tagPosWarnFix opts p $ filter isTagWarning tags
    return (innerText tags, warnings)
