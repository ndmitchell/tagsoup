
module Text.HTML.TagSoup.Parser(parseTags, parseTagsOptions, module Text.HTML.TagSoup.Options) where

import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Options
import Text.StringLike
import Text.HTML.TagSoup.Generated


parseTags :: StringLike str => str -> [Tag str]
parseTags = parseTagsOptions parseOptions

