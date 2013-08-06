{-# LANGUAGE PatternGuards #-}
{-|
    This module converts a list of 'Tag' back into a string.
-}

module Text.HTML.TagSoup.Render
    (
    renderTags, renderTagsOptions, escapeHTML,
    RenderOptions(..), renderOptions
    ) where

import Text.HTML.TagSoup.Entity
import Text.HTML.TagSoup.Type
import Text.StringLike


-- | These options control how 'renderTags' works.
--
--   The strange quirk of only minimizing @\<br\>@ tags is due to Internet Explorer treating
--   @\<br\>\<\/br\>@ as @\<br\>\<br\>@.
data RenderOptions str = RenderOptions
    {optEscape :: str -> str        -- ^ Escape a piece of text (default = escape the four characters @&\"\<\>@)
    ,optMinimize :: str -> Bool     -- ^ Minimise \<b\>\<\/b\> -> \<b/\> (default = minimise only @\<br\>@ tags)
    ,optRawTag :: str -> Bool      -- ^ Should a tag be output with no escaping (default = true only for @script@)
    }


-- | Replace the four characters @&\"\<\>@ with their HTML entities ('escapeXML' lifted to 'StringLike').
escapeHTML :: StringLike str => str -> str
escapeHTML = fromString . escapeXML . toString


-- | The default render options value, described in 'RenderOptions'.
renderOptions :: StringLike str => RenderOptions str
renderOptions = RenderOptions escapeHTML (\x -> toString x == "br") (\x -> toString x == "script")


-- | Show a list of tags, as they might have been parsed, using the default settings given in
--   'RenderOptions'.
--
-- > renderTags [TagOpen "hello" [],TagText "my&",TagClose "world"] == "<hello>my&amp;</world>"
renderTags :: StringLike str => [Tag str] -> str
renderTags = renderTagsOptions renderOptions


-- | Show a list of tags using settings supplied by the 'RenderOptions' parameter,
--   eg. to avoid escaping any characters one could do:
--
-- > renderTagsOptions renderOptions{optEscape = id} [TagText "my&"] == "my&"
renderTagsOptions :: StringLike str => RenderOptions str -> [Tag str] -> str
renderTagsOptions opts = strConcat . tags
    where
        s = fromString
        ss x = [s x]
    
        tags (TagOpen name atts:TagClose name2:xs)
            | name == name2 && optMinimize opts name = open name atts (s " /") ++ tags xs
        tags (TagOpen name atts:xs)
            | Just ('?',_) <- uncons name = open name atts (s " ?") ++ tags xs
            | optRawTag opts name =
                let (a,b) = break (== TagClose name) (TagOpen name atts:xs)
                in concatMap (\x -> case x of TagText s -> [s]; _ -> tag x) a ++ tags b
        tags (x:xs) = tag x ++ tags xs
        tags [] = []

        tag (TagOpen name atts) = open name atts (s "")
        tag (TagClose name) = [s "</", name, s ">"]
        tag (TagText text) = [txt text]
        tag (TagComment text) = ss "<!--" ++ com text ++ ss "-->"
        tag _ = ss ""

        txt = optEscape opts
        open name atts shut = [s "<",name] ++ concatMap att atts ++ [shut,s ">"]
        att (x,y) | xnull && ynull = [s " \"\""]
                  | ynull = [s " ", x]
                  | xnull = [s " \"",txt y,s "\""]
                  | otherwise = [s " ",x,s "=\"",txt y,s "\""]
            where (xnull, ynull) = (strNull x, strNull y)

        com xs | Just ('-',xs) <- uncons xs, Just ('-',xs) <- uncons xs, Just ('>',xs) <- uncons xs = s "-- >" : com xs
        com xs = case uncons xs of
            Nothing -> []
            Just (x,xs) -> fromChar x : com xs
