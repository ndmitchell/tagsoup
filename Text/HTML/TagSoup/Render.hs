{-|
    This module is preliminary and may change at a future date.
    If you wish to use its features, please email me and I will
    help evolve an API that suits you.
-}

module Text.HTML.TagSoup.Render
    {-# DEPRECATED "Not quite ready for use yet, email me if it looks useful to you" #-}
    (
    renderTags, renderTagsOptions,
    RenderOptions(..), renderOptions
    ) where

import Data.Char
import qualified Data.IntMap as IntMap
import Text.HTML.TagSoup.Entity
import Text.HTML.TagSoup.Type


data RenderOptions = RenderOptions
    {optEscape :: Char -> String    -- ^ Escape a single character
    ,optMinimize :: String -> Bool  -- ^ Minimise <b></b> -> <b/>, defaults to only for @br@
    }

renderOptions :: RenderOptions
renderOptions = RenderOptions
        (\x -> IntMap.findWithDefault [x] (ord x) esc)
        (== "br")
    where esc = IntMap.fromList [(b, "&"++a++";") | (a,b) <- htmlEntities]


-- | Show a list of tags, as they might have been parsed
renderTags :: [Tag] -> String
renderTags = renderTagsOptions renderOptions


renderTagsOptions :: RenderOptions -> [Tag] -> String
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
