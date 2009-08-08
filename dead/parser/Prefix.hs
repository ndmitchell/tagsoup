
module Text.HTML.TagSoup.Generated.Parser where

import Data.List
import Data.Char
import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Entity

data Tags
     = Comment String
	 | OpenStart String
	 | OpenShut
	 | OpenFinish
	 | OpenAtt String String
	 | Text String
	 | Close String
	 | Warning String
	 | Pos Position
	   deriving Show



endBy opts str (s0,p0,w0)
	| str `isPrefixOf` s0 = ("",(drop (length str) s0,p0,w0))
	| null s0 = ("",(s0,p0,w0))
	| otherwise = (head s0:v,spw)
        where (v,spw) = endBy opts str (tail s0,p0,w0)

literal opts str (s0,p0,w0)
    | str `isPrefixOf` s0 = (str,(drop (length str) s0,p0,w0))
    | otherwise = ("",(s0,p0,w0++ tagPosWarn opts p0 ("Expected " ++ str)))

clearWarn opts (s0,p0,w0) = ("",(s0,p0,[]))


name opts (s0,p0,w0) = (a,(b,p0,w0))
	where (a,b) = span isAlphaNum s0

spaces opts (s0,p0,w0) = (a,(b,p0,w0))
	where (a,b) = span isSpace s0


entityNameHashX x = entityName ("#x" ++ x)
entityNameHash x = entityName ("#" ++ x)
entityName x = [Text $ "&" ++ x ++ ";"]


takeWhileNot opts str (s0,p0,w0) = (a,(b,p0,w0))
	where (a,b) = span (`notElem` str) s0


innerTexts xs = concat [x | Text x <- xs]
    


---------------------------------------------------------------------
-- * ParseOptions

data ParseOptions = ParseOptions
    {optTagPosition :: Bool -- ^ Should 'TagPosition' values be given before every item
    ,optTagWarning :: Bool -- ^ Should 'TagWarning' values be given
    ,optLookupEntity :: String -> [Tags] -- ^ How to lookup an entity
    }


-- Default 'ParseOptions' structure
parseOptions :: ParseOptions
parseOptions = ParseOptions False False f
    where
        f x = case lookupEntity x of
                  Nothing -> [Text $ "&" ++ x ++ ";", Warning $ "Unknown entity: &" ++ x ++ ";"]
                  Just x -> [Text [x]]


parseTags :: String -> [Tags]
parseTags = parseTagsOptions parseOptions

tagWarn :: ParseOptions -> String -> [Tag]
tagWarn opts x = [TagWarning x | optTagWarning opts]

---------------------------------------------------------------------
-- * Positions

tagPos :: ParseOptions -> Position -> [Tags]
tagPos opts p = [Pos p | optTagPosition opts]

tagPosWarn :: ParseOptions -> Position -> String -> [Tags]
tagPosWarn opts p x | optTagWarning opts = tagPos opts p ++ [Warning x]
                    | otherwise = []

tagPosWarnFix :: ParseOptions -> Position -> [Tags] -> [Tags]
tagPosWarnFix opts p = addPositions . remWarnings
    where
        remWarnings = if optTagWarning opts then id else filter (not . isWarning)
        addPositions = concatMap (\x -> tagPos opts p ++ [x])

isWarning Warning{} = True; isWarning _ = False


---------------------------------------------------------------------
-- * Driver

parseTagsOptions :: ParseOptions -> String -> [Tags]
parseTagsOptions opts x = fst $ tags opts (x,nullPosition,[])

{-
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

-}
