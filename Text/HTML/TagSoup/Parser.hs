
module Text.HTML.TagSoup.Parser(parseTags, parseTagsOptions, module Text.HTML.TagSoup.Options) where

import Text.HTML.TagSoup.Specification
import Text.HTML.TagSoup.Implementation
import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Options
import Text.StringLike


parseTags :: StringLike str => str -> [Tag str]
parseTags = parseTagsOptions parseOptions


parseTagsOptions :: StringLike str => ParseOptions str -> str -> [Tag str]
parseTagsOptions opts = map (fmap fromString) . result opts2 . output opts2 . parse . toString
    where opts2 = fmapParseOptions opts
