{-# LANGUAGE PatternGuards #-}
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
import Text.StringLike


-- | These options control how 'renderTags' works.
--
--   The strange quirk of only minimizing @\<br\>@ tags is due to Internet Explorer treating
--   @\<br\>\<\/br\>@ as @\<br\>\<br\>@.
data RenderOptions str = RenderOptions
    {optEscape :: str -> str        -- ^ Escape a piece of text (default = escape the four characters @&\"\<\>@)
    ,optMinimize :: str -> Bool     -- ^ Minimise \<b\>\<\/b\> -> \<b/\> (default = minimise only @\<br\>@ tags)
    }


-- | The default render options value, described in 'RenderOptions'.
renderOptions :: StringLike str => RenderOptions str
renderOptions = RenderOptions
        (\x -> fromString $ concatMap esc1 $ toString x)
        (\x -> toString x == "br")
    where esc = IntMap.fromList [(b, "&"++a++";") | (a,b) <- htmlEntities]
          esc1 x = IntMap.findWithDefault [x] (ord x) esc


fmapRenderOptions :: (StringLike a, StringLike b) => RenderOptions a -> RenderOptions b
fmapRenderOptions (RenderOptions x y) = RenderOptions (castString . x . castString) (y . castString)


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
        tags (TagOpen name atts:xs) | Just ('?',_) <- uncons name = open name atts (s " ?") ++ tags xs
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
