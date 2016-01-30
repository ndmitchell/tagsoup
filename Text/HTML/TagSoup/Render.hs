{-# LANGUAGE PatternGuards, OverloadedStrings #-}
{-|
    This module converts a list of 'Tag' back into a string.
-}

module Text.HTML.TagSoup.Render
    (
    renderTags, renderTagsOptions, escapeHTML,
    RenderOptions(..), renderOptions
    ) where

import Data.Maybe

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
renderOptions = RenderOptions escapeHTML (== "br") (== "script")


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
renderTagsOptions opts tags = strConcat $ foldr tagAcc (\_ _ -> []) tags Nothing Nothing
    where
        tagAcc (TagClose name) tags (Just (name',atts)) raw
            | name == name' = open name (isJust raw) atts " /" ++ tags Nothing raw
        tagAcc t tags (Just (name,atts)) raw = open name (isJust raw) atts "" ++ tagAcc' t tags raw
        tagAcc t tags Nothing raw = tagAcc' t tags raw

        tagAcc' (TagOpen name atts) tags raw
            | optMinimize opts name = tags (Just (name, atts)) raw
            | Just ('?',_) <- uncons name = open name (isJust raw) atts " ?" ++ tags Nothing raw
            | optRawTag opts name = open name True atts "" ++ tags Nothing (Just $ fromMaybe name raw)
        tagAcc' t tags raw = tag t (isJust raw) ++ tags Nothing raw

        tag (TagOpen name atts) isRaw = open name isRaw atts ""
        tag (TagClose name) _ = ["</", name, ">"]
        tag (TagText text) True = [text]
        tag (TagText text) False = [optEscape opts text]
        tag (TagComment text) _ = ["<!--"] ++ com text ++ ["-->"]
        tag _ _ = [""]

        open name israw atts shut = ["<",name] ++ concatMap (att israw) atts ++ [shut,">"]
        att _ ("","") = [" \"\""]
        att _ (x ,"") = [" ", x]
        att isRaw ("", y) = [" \"",(if isRaw then y else optEscape opts y),"\""]
        att isRaw (x , y) = [" ",x,"=\"",(if isRaw then y else optEscape opts y),"\""]

        -- com makes sure that comments close correctly by transforming "-->" into "-- >"
        com xs | Just ('-',xs) <- uncons xs, Just ('-',xs) <- uncons xs, Just ('>',xs) <- uncons xs = "-- >" : com xs
        com xs = case uncons xs of
            Nothing -> []
            Just (x,xs) -> fromChar x : com xs
