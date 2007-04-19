{-|
    Module      :  Data.Html.TagSoup
    Copyright   :  (c) Neil Mitchell 2006-2007
    License     :  BSD-style

    Maintainer  :  http://www.cs.york.ac.uk/~ndm/
    Stability   :  moving towards stable
    Portability :  portable

    This module is for extracting information out of unstructured HTML code,
    sometimes known as tag-soup. This is for situations where the author of
    the HTML is not cooperating with the person trying to extract the information,
    but is also not trying to hide the information.
    
    The standard practice is to parse a String to 'Tag's using 'parseTags', then
    operate upon it to extract the necessary information.
-}


module Data.Html.TagSoup(
    -- * Data structures and parsing
    Tag(..), Attribute, parseTags,
    module Data.Html.Download,
    
    -- * Tag Combinators
    (~==), (~/=),
    isTagOpen, isTagClose, isTagText,
    fromTagText, fromAttrib,
    isTagOpenName, isTagCloseName,
    sections
    ) where

import Data.Char
import Data.List
import Data.Maybe
import Data.Html.Download


-- | An HTML attribute @id=\"name\"@ generates @(\"id\",\"name\")@
type Attribute = (String,String)

-- | An HTML element, a document is @[Tag]@.
--   There is no requirement for 'TagOpen' and 'TagClose' to match
data Tag = TagOpen String [Attribute]  -- ^ An open tag with 'Attribute's in their original order.
         | TagClose String             -- ^ A closing tag
         | TagText String              -- ^ A text node, guranteed not to be the empty string
           deriving (Show, Eq, Ord)


-- | Parse an HTML document to a list of 'Tag'.
-- Automatically expands out escape characters.
parseTags :: String -> [Tag]
parseTags [] = []

parseTags ('<':'/':xs) = TagClose tag : parseTags trail
    where
        (tag,rest) = span isAlphaNum xs
        trail = drop 1 $ dropWhile (/= '>') rest

parseTags ('<':xs)
        | "/>" `isPrefixOf` rest2 = res : TagClose tag : parseTags (drop 2 rest2)
        | ">" `isPrefixOf` rest2 = res : parseTags (drop 1 rest2)
        | otherwise = res : parseTags (drop 1 $ dropWhile (/= '>') rest2)
    where
        res = TagOpen tag attrs
        (tag,rest) = span isAlphaNum xs
        (attrs,rest2) = parseAttributes rest

parseTags (x:xs) = [TagText $ parseString pre | not $ null pre] ++ parseTags post
    where (pre,post) = break (== '<') (x:xs)


parseAttributes :: String -> ([Attribute], String)
parseAttributes (x:xs) | isSpace x = parseAttributes xs
                       | not $ isAlpha x = ([], x:xs)
                       | otherwise = ((parseString lhs, parseString rhs):attrs, over)
    where
        (attrs,over) = parseAttributes (dropWhile isSpace other)
    
        (lhs,rest) = span isAlphaNum (x:xs)
        rest2 = dropWhile isSpace rest
        (rhs,other) = if "=" `isPrefixOf` rest2 then parseValue (dropWhile isSpace $ tail rest2) else ("", rest2)
        

parseValue :: String -> (String, String)
parseValue ('\"':xs) = (a, drop 1 b)
    where (a,b) = break (== '\"') xs
parseValue x = span isValid x
    where isValid x = isAlphaNum x || x `elem` "_-"



escapes = [("gt",">")
          ,("lt","<")
          ,("amp","&")
          ,("quot","\"")
          ]


parseEscape :: String -> Maybe String
parseEscape ('#':xs) | all isDigit xs = Just [chr $ read xs]
parseEscape xs = lookup xs escapes



parseString :: String -> String
parseString ('&':xs) = case parseEscape a of
                            Nothing -> '&' : parseString xs
                            Just x -> x ++ parseString (drop 1 b)
    where (a,b) = break (== ';') xs
parseString (x:xs) = x : parseString xs
parseString [] = []


-- | Test if a 'Tag' is a 'TagOpen'
isTagOpen :: Tag -> Bool
isTagOpen (TagOpen {})  = True; isTagOpen  _ = False

-- | Test if a 'Tag' is a 'TagClose'
isTagClose :: Tag -> Bool
isTagClose (TagClose {}) = True; isTagClose _ = False

-- | Test if a 'Tag' is a 'TagText'
isTagText :: Tag -> Bool
isTagText (TagText {})  = True; isTagText  _ = False

-- | Extract the string from within 'TagText', crashes if not a 'TagText'
fromTagText :: Tag -> String
fromTagText (TagText x) = x

-- | Extract an attribute, crashes if not a 'TagOpen'.
--   Returns "" if no attribute present.
fromAttrib :: String -> Tag -> String
fromAttrib att (TagOpen _ atts) = fromMaybe "" $ lookup att atts

-- | Returns True if the 'Tag' is 'TagOpen' and matches the given name
isTagOpenName :: String -> Tag -> Bool
isTagOpenName name (TagOpen n _) = n == name
isTagOpenName _ _ = False

-- | Returns True if the 'Tag' is 'TagClose' and matches the given name
isTagCloseName :: String -> Tag -> Bool
isTagCloseName name (TagClose n) = n == name
isTagCloseName _ _ = False


-- | Performs an inexact match, the first item should be the thing to match.
-- If the second item is a blank string, that is considered to match anything.
-- For example:
--
-- > (TagText "test" ~== TagText ""    ) == True
-- > (TagText "test" ~== TagText "test") == True
-- > (TagText "test" ~== TagText "soup") == False
--
-- For 'TagOpen' missing attributes on the right are allowed.
(~==) :: Tag -> Tag -> Bool
(TagText y) ~== (TagText x) = null x || x == y
(TagClose y) ~== (TagClose x) = null x || x == y
(TagOpen y ys) ~== (TagOpen x xs) = (null x || x == y) && all f xs
    where
        f ("",val) = val `elem` map snd ys
        f (name,"") = name `elem` map fst ys
        f nameval = nameval `elem` ys
_ ~== _ = False

-- | Negation of '~=='
(~/=) :: Tag -> Tag -> Bool
(~/=) a b = not (a ~== b)


-- | This function takes a list, and returns all initial lists whose
--   first item matches the function.
sections :: (a -> Bool) -> [a] -> [[a]]
sections f [] = []
sections f (x:xs) = [x:xs | f x] ++ sections f xs
