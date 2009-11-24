{-# LANGUAGE PatternGuards #-}
{-|
    This module converts a list of 'Tag' back into a string.
-}

module Text.HTML.TagSoup.Render
    (
    renderTags, renderTagsOptions,
    RenderOptions(..), renderOptions, fmapRenderOptions
    ) where

import Data.Char
import qualified Data.IntMap as IntMap
import Text.HTML.TagSoup.Entity
import Text.HTML.TagSoup.Type
import Text.StringLike


data RenderOptions str = RenderOptions
    {optEscape :: str -> str        -- ^ Escape a piece of text
    ,optMinimize :: str -> Bool     -- ^ Minimise <b></b> -> <b/>
    }


-- | A configuration which escapes the four characters @&\"\<\>@, and only minimises @\<br\>@ tags.
--   This configuration is chosen to be compatible with Internet Explorer.
renderOptions :: StringLike str => RenderOptions str
renderOptions = RenderOptions
        (\x -> fromString $ concatMap esc1 $ toString x)
        (\x -> toString x == "br")
    where esc = IntMap.fromList [(b, "&"++a++";") | (a,b) <- htmlEntities]
          esc1 x = IntMap.findWithDefault [x] (ord x) esc


fmapRenderOptions :: (StringLike a, StringLike b) => RenderOptions a -> RenderOptions b
fmapRenderOptions (RenderOptions x y) = RenderOptions (fromString . toString . x . fromString . toString) (y . fromString . toString)


-- | Show a list of tags, as they might have been parsed. Note that this makes use of
--   'renderOptions'. If you do not desire renderOption's behavior, try instead 'renderTagsOptions'.
renderTags :: StringLike str => [Tag str] -> str
renderTags = renderTagsOptions renderOptions


-- | Show a list of tags as a String. You need to supply a 'RenderOptions' configuration
--   value. One is provided for you as 'renderOptions'; override it as necessary, eg. to avoid
--   escaping any characters one could do:
--
-- > renderTagsOptions (renderOptions{optEscape = (:[])})
renderTagsOptions :: StringLike str => RenderOptions str -> [Tag str] -> str
renderTagsOptions opts = strConcat . tags
    where
        s = fromString
        ss x = [s x]
    
        tags (TagOpen name atts:TagClose name2:xs)
            | name == name2 && optMinimize opts name = open name atts (s " /") ++ tags xs
        tags (x:xs) = tag x ++ tags xs
        tags [] = []

        tag (TagOpen name atts) = open name atts (s "")
        tag (TagClose name) = [s "</", name, s ">"]
        tag (TagText text) = [txt text]
        tag (TagComment text) = ss "<!--" ++ com text ++ ss "-->"
        tag _ = ss ""

        txt = optEscape opts
        open name atts shut = [s "<",name] ++ concatMap att atts ++ [shut,s ">"]
        att (x,y) | strNull y = [s " ", x]
                  | strNull x = [s " \"",txt y,s "\""]
                  | otherwise = [s " ",x,s "=\"",txt y,s "\""]

        com xs | Just ('-',xs) <- uncons xs, Just ('-',xs) <- uncons xs, Just ('>',xs) <- uncons xs = s "-- >" : com xs
        com xs = case uncons xs of
            Nothing -> []
            Just (x,xs) -> fromString1 x : com xs
