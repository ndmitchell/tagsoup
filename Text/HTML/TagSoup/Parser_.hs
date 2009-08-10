
module Text.HTML.TagSoup.Parser_(TagSoup(..), parseTags) where

import Text.HTML.TagSoup.Specification
import Text.HTML.TagSoup.Implementation
import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Options_
import Text.StringLike


test x = parseTagsOptions parseOptions{optTagWarning=True,optTagPosition=True} $ Spec x


parseTags :: TagSoup str => str -> [Tag str]
parseTags = parseTagsOptions parseOptions


class StringLike str => TagSoup str where
    parseTagsOptions :: ParseOptions str -> str -> [Tag str]


newtype Spec = Spec {fromSpec :: String}

instance Show Spec where
    show (Spec x) = show x


instance StringLike Spec where
    toString = fromSpec
    fromString = Spec

instance TagSoup Spec where
    parseTagsOptions opts (Spec x) = map (fmap Spec) $ result opts2 $ output opts2 $ parse x
        where opts2 = parseOptionsRetype opts
