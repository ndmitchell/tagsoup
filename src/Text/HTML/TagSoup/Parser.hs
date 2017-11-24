
module Text.HTML.TagSoup.Parser(
    parseTags, parseTagsOptions,
    ParseOptions(..), parseOptions, parseOptionsFast, parseOptionsEntities
    ) where

import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Options
import qualified Text.HTML.TagSoup.Generated as Gen


-- | Parse a string to a list of tags, using an HTML 5 compliant parser.
--
-- > parseTags "<hello>my&amp;</world>" == [TagOpen "hello" [],TagText "my&",TagClose "world"]
parseTags :: StringLike str => str -> [Tag str]
parseTags = parseTagsOptions parseOptions


-- | Parse a string to a list of tags, using settings supplied by the 'ParseOptions' parameter,
--   eg. to output position information:
--
-- > parseTagsOptions parseOptions{optTagPosition = True} "<hello>my&amp;</world>" ==
-- >    [TagPosition 1 1,TagOpen "hello" [],TagPosition 1 8,TagText "my&",TagPosition 1 15,TagClose "world"]
parseTagsOptions :: StringLike str => ParseOptions str -> str -> [Tag str]
parseTagsOptions = Gen.parseTagsOptions
