
module Text.HTML.TagSoup.Render(
    renderTags, renderTagsOptions,
    RenderOptions(..), renderOptions
    ) where

import Text.HTML.TagSoup.Type


data RenderOptions = RenderOptions
    {optEscape :: Char -> String
    }

renderOptions :: RenderOptions
renderOptions = RenderOptions (:[])


-- | Show a list of tags, as they might have been parsed
renderTags :: [Tag] -> String
renderTags = renderTagsOptions renderOptions


renderTagsOptions :: RenderOptions -> [Tag] -> String
renderTagsOptions opts xs = tags xs
    where
        tags (TagOpen name atts:TagClose name2:xs)
            | name == name2 = open name atts " /" ++ tags xs
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
