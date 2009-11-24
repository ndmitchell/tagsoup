
module Text.HTML.TagSoup.Parser(parseTags, parseTagsOptions, module Text.HTML.TagSoup.Options) where

import Text.HTML.TagSoup.Specification
import Text.HTML.TagSoup.Implementation
import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Options
import Text.StringLike


parseTags :: StringLike str => str -> [Tag str]
parseTags = parseTagsOptions parseOptions


parseTagsOptions :: StringLike str => ParseOptions str -> str -> [Tag str]
parseTagsOptions opts = result opts . output opts . parse . toString

