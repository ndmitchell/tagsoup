
module Text.HTML.TagSoup.Manual(parseTagsOptions) where

import Text.HTML.TagSoup.Specification
import Text.HTML.TagSoup.Implementation
import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Options
import Text.StringLike


parseTagsOptions :: StringLike str => ParseOptions str -> str -> [Tag str]
parseTagsOptions opts = output opts . parse . toString

