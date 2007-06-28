{-# OPTIONS_GHC -w #-}

module Text.HTML.TagSoup.Parser(parseTags, parseTagsGeneric) where

import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.TagPos
import Text.HTML.TagSoup.Position
import Control.Monad.State
import Data.Char
import Data.List


---------------------------------------------------------------------
-- * Driver

parseTags :: String -> [Tag Char]
parseTags = parseTagsGeneric

parseTagsGeneric :: (TagType tag, CharType char) => String -> [tag char]
parseTagsGeneric x = mergeTexts $ evalState parse $ Value x (initialize "")



mergeTexts :: (TagType tag, CharType char) => [TagPos char] -> [tag char]
mergeTexts (TagPos p (TagText x):xs) = newTagPos p (TagText $ concat $ x:texts) : warns ++ mergeTexts rest
    where
        (texts,warns,rest) = f xs

        f (TagPos _ (TagText x):xs) = (x:a,b,c)
            where (a,b,c) = f xs
        f (TagPos p (TagWarning x):xs) = (a,newTagPos p (TagWarning x):b,c)
            where (a,b,c) = f xs
        f xs = ([],[],xs)

mergeTexts (TagPos p x:xs) = newTagPos p x : mergeTexts xs
mergeTexts [] = []


---------------------------------------------------------------------
-- * Combinators

data Value = Value String !Position

type Parser a = State Value a


isNameChar x = isAlphaNum x || x `elem` "-_:"

consume :: Int -> Parser ()
consume n = do
    Value s p <- get
    let (a,b) = splitAt n s
    put $ Value b (updateOnString p a)


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


breakName :: Parser String
breakName = do
    Value s p <- get
    if not (null s) && isAlpha (head s) then do
        let (a,b) = span isNameChar s
        consume (length a)
        return a
     else
        return ""

breakNumber :: Parser (Maybe Int)
breakNumber = do
    Value s p <- get
    if not (null s) && isDigit (head s) then do
        let (a,b) = span isDigit s
        consume (length a)
        return $ Just $ read a
     else
        return Nothing


dropSpaces :: Parser ()
dropSpaces = do
    Value s p <- get
    let n = length $ takeWhile isSpace s
    consume n


tagPos :: (TagType tag, CharType char) => Position -> Tag char -> tag char
tagPos = newTagPos


---------------------------------------------------------------------
-- * Parser

parse :: (TagType tag, CharType char) => Parser [tag char]
parse = do
    Value s p <- get
    case s of
        '<':'!':'-':'-':_ -> consume 4 >> comment p
        '<':'!':_         -> consume 2 >> special p
        '<':'/':_         -> consume 2 >> close p
        '<':_             -> consume 1 >> open p
        []                -> return []
        '&':_             -> do
            consume 1
            ~(s,warn) <- entity p
            rest <- parse
            return $ tagPos p (TagText s) : warn ++ rest
        s:ss              -> do
            consume 1
            rest <- parse
            return $ tagPos p (TagText [fromHTMLChar $ Char s]) : rest


comment p1 = do
    ~(inner,bad) <- breakOn "-->"
    rest <- parse
    return $ tagPos p1 (TagComment inner) :
             [tagPos p1 $ TagWarning "Unexpected end when looking for \"-->\"" | bad] ++
             rest


special p1 = do
    name <- breakName
    dropSpaces
    ~(inner,bad) <- breakOn ">"
    rest <- parse
    return $ tagPos p1 (TagSpecial name inner) :
             [tagPos p1 $ TagWarning "Empty name in special" | null name] ++
             [tagPos p1 $ TagWarning "Unexpected end when looking for \">\"" | bad] ++
             rest


close p1 = do
    name <- breakName
    dropSpaces
    Value s p <- get
    case s of
        '>':s -> do
            consume 1
            rest <- parse
            return $ tagPos p1 (TagClose name) :
                     [tagPos p1 $ TagWarning "Empty name in close tag" | null name] ++
                     rest
        _ -> do
            ~(_,bad) <- breakOn ">"
            rest <- parse
            return $ tagPos p1 (TagClose name) :
                     (tagPos p1 $ TagWarning "Junk in closing tag") :
                     [tagPos p1 $ TagWarning "Unexpected end when looking for \">\"" | bad] ++
                     rest


open p1 = do
    name <- breakName
    if null name then do
        rest <- parse
        return $ tagPos p1 (TagText [fromHTMLChar $ Char '<']) : rest
     else do
        ~(atts,shut,warns) <- attribs p1
        rest <- parse
        return $ tagPos p1 (TagOpen name atts) :
                 [tagPos p1 (TagClose name) | shut] ++
                 warns ++ rest


attribs :: (TagType tag, CharType char) => Position -> Parser ([Attribute char],Bool,[tag char])
attribs p1 = do
    dropSpaces
    Value s p <- get
    case s of
        '/':'>':_ -> consume 2 >> return ([],True ,[])
        '>':_     -> consume 1 >> return ([],False,[])
        []        -> return ([],False,[tagPos p1 $ TagWarning "Unexpected end when looking for \">\""])
        _ -> attrib p1


attrib :: (TagType tag, CharType char) => Position -> Parser ([Attribute char],Bool,[tag char])
attrib p1 = do
    name <- breakName
    if null name then do
        consume 1
        ~(atts,shut,warns) <- attribs p1
        return (atts,shut,tagPos p1 (TagWarning "Junk character in tag") : warns)
     else do
        Value s p <- get
        case s of
            '=':s -> do
                consume 1
                ~(val,warns1) <- value
                ~(atts,shut,warns2) <- attribs p1
                return ((name,val):atts,shut,warns1++warns2)
            _ -> do
                ~(atts,shut,warns) <- attribs p1
                return ((name,[]):atts,shut,warns)


value :: (TagType tag, CharType char) => Parser ([char],[tag char])
value = do
    Value s p <- get
    case s of
        '\"':_ -> consume 1 >> f p True "\""
        '\'':_ -> consume 1 >> f p True "\'"
        _ -> f p False " />"
    where
        f p1 quote end = do
            Value s p <- get
            case s of
                '&':_ -> do
                    consume 1
                    ~(cs1,warns1) <- entity p
                    ~(cs2,warns2) <- f p1 quote end
                    return (cs1++cs2,warns1++warns2)
                c:_ | c `elem` end -> do
                    if quote then consume 1 else return ()
                    return ([],[])
                c:_ -> do
                    consume 1
                    ~(cs,warns) <- f p1 quote end
                    return (fromHTMLChar (Char c):cs,warns)
                [] -> return ([],[tagPos p1 $ TagWarning "Unexpected end in attibute value"])


entity :: (TagType tag, CharType char) => Position -> Parser ([char],[tag char])
entity p1 = do
    Value s _ <- get
    ~(res,bad) <- case s of
        '#':_ -> do
            consume 1
            num <- breakNumber
            case num of
                Nothing -> return ([Char '&',Char '#'],True)
                Just y -> return ([NumericRef y],False)
        _ -> do
            name <- breakName
            if null name then
                return ([Char '&'],True)
             else
                return ([NamedRef name], False)
    if bad then
        return (map fromHTMLChar res,[tagPos p1 $ TagWarning "Unquoted & found"])
     else do
        Value s _ <- get
        case s of
            ';':_ -> consume 1 >> return (map fromHTMLChar res,[])
            _ -> return (map fromHTMLChar res,[tagPos p1 $ TagWarning "Missing closing \";\" in entity"])
