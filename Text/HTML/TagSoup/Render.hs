{-|
    This module converts a list of 'Tag' back into a string.
-}

module Text.HTML.TagSoup.Render
    (
    renderTags, renderTagsOptions,
    RenderOptions(..), renderOptions
    ) where

import Data.Char
import qualified Data.IntMap as IntMap
import Text.HTML.TagSoup.Entity
import Text.HTML.TagSoup.Type


data RenderOptions = RenderOptions
    {optEscape :: Char -> String    -- ^ Escape a single text character
    ,optMinimize :: String -> Bool  -- ^ Minimise <b></b> -> <b/>
    }


-- | A configuration which escapes the four characters @&\"\<\>@, and only minimises @\<br\>@ tags.
--   This configuration is chosen to be compatible with Internet Explorer.
renderOptions :: RenderOptions
renderOptions = RenderOptions
        (\x -> IntMap.findWithDefault [x] (ord x) esc)
        (== "br")
    where esc = IntMap.fromList [(b, "&"++a++";") | (a,b) <- htmlEntities]


-- | Show a list of tags, as they might have been parsed. Note that this makes use of
--   'renderOptions'. If you do not desire renderOption's behavior, try instead 'renderTagsOptions'.
renderTags :: [Tag String] -> String
renderTags = renderTagsOptions renderOptions


-- | Show a list of tags as a String. You need to supply a 'RenderOptions' configuration
--   value. One is provided for you as 'renderOptions'; override it as necessary, eg. to avoid
--   escaping any characters one could do:
--
-- > renderTagsOptions (renderOptions{optEscape = (:[])})
renderTagsOptions :: RenderOptions -> [Tag String] -> String
renderTagsOptions opts = tags
    where
        tags (TagOpen name atts:TagClose name2:xs)
            | name == name2 && optMinimize opts name = open name atts " /" ++ tags xs
        tags (x:xs) = tag x ++ tags xs
        tags [] = []

        tag (TagOpen name atts) = open name atts ""
        tag (TagClose name) = "</" ++ name ++ ">"
        tag (TagText text) = txt text
        tag (TagComment text) = "<!--" ++ com text ++ "-->"
        tag _ = ""

        txt = concatMap (optEscape opts)
        open name atts shut = "<" ++ name ++ concatMap att atts ++ shut ++ ">"
        att (x,"") = " " ++ x
        att ("",y) = " " ++ "\"" ++ txt y ++ "\""
        att (x,y) = " " ++ x ++ "=\"" ++ txt y ++ "\""

        com ('-':'-':'>':xs) = "-- >" ++ com xs
        com (x:xs) = x : com xs
        com [] = []
