
module Data.Html.TagSoup(
    Tag(..), Attribute, parseTags,
    module Data.Html.Download,
    
    (~==), (~/=),
    isTagOpen, isTagClose, isTagText, fromTagText,
    isTagOpenName, isTagCloseName,
    sections
    ) where

import Data.Char
import Data.List
import Data.Html.Download


type Attribute = (String,String)

data Tag = TagOpen String [Attribute]
         | TagClose String
         | TagText String
           deriving Show


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
parseValue x = span isAlphaNum x



escapes = [("gt",">")
          ,("lt","<")
          ,("amp","&")
          ,("quot","\"")
          ]


parseString :: String -> String
parseString ('&':xs) = case lookup a escapes of
                            Nothing -> '&' : parseString xs
                            Just x -> x ++ parseString (drop 1 b)
    where (a,b) = break (== ';') xs
parseString (x:xs) = x : parseString xs
parseString [] = []


-- TAG COMBINATORS

isTagOpen, isTagClose, isTagText :: Tag -> Bool
isTagOpen  (TagOpen {})  = True; isTagOpen  _ = False
isTagClose (TagClose {}) = True; isTagClose _ = False
isTagText  (TagText {})  = True; isTagText  _ = False

fromTagText :: Tag -> String
fromTagText (TagText x) = x

isTagOpenName :: Tag -> Bool
isTagOpenName name (TagOpen n _) = n == name
isTagOpenName _ _ = False

isTagCloseName :: Tag -> Bool
isTagCloseName name (TagClose n) = n == name
isTagCloseName _ _ = False


(~==) :: Tag -> Tag -> Bool
(TagText y) ~== (TagText x) = null x || x == y
(TagClose y) ~== (TagClose x) = null x || x == y
(TagOpen y ys) ~== (TagOpen x xs) = (null x || x == y) && all f xs
    where
        f ("",val) = val `elem` map snd ys
        f (name,"") = name `elem` map fst ys
        f nameval = nameval `elem` ys
_ ~== _ = False

(~/=) :: Tag -> Tag -> Bool
(~/=) a b = not (a ~== b)

sections :: (a -> Bool) -> [a] -> [[a]]
sections f [] = []
sections f (x:xs) = [x:xs | f x] ++ sections f xs
