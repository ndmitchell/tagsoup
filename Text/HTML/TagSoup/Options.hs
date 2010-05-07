{-# LANGUAGE DeriveDataTypeable #-}

module Text.HTML.TagSoup.Options where

import Data.Typeable
import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Entity
import Text.StringLike


-- | These options control how 'parseTags' works.
data ParseOptions str = ParseOptions
    {optTagPosition :: Bool -- ^ Should 'TagPosition' values be given before some items (default=False,fast=False)
    ,optTagWarning :: Bool  -- ^ Should 'TagWarning' values be given (default=False,fast=False)
    ,optEntityData :: (str,Bool) -> [Tag str] -- ^ How to lookup an entity (Bool = has ending @';'@)
    ,optEntityAttrib :: (str,Bool) -> (str,[Tag str]) -- ^ How to lookup an entity in an attribute (Bool = has ending @';'@?)
    ,optTagTextMerge :: Bool -- ^ Require no adjacent 'TagText' values (default=True,fast=False)
    }
    deriving Typeable


-- | The default parse options value, described in 'ParseOptions'.
parseOptions :: StringLike str => ParseOptions str
parseOptions = ParseOptions False False entityData entityAttrib True
    where
        entityData x = TagText a : b
            where (a,b) = entityAttrib x

        entityAttrib ~(x,b) = case lookupEntity y of
            Just y -> (fromChar y, [])
            Nothing -> (fromString $ "&" ++ y ++ [';'|b], [TagWarning $ fromString $ "Unknown entity: " ++ y])
            where y = toString x


-- | A 'ParseOptions' structure optimised for speed, following the fast options.
parseOptionsFast :: StringLike str => ParseOptions str
parseOptionsFast = parseOptions{optTagTextMerge=False}


-- | Change the underlying string type of a 'ParseOptions' value.
fmapParseOptions :: (StringLike from, StringLike to) => ParseOptions from -> ParseOptions to
fmapParseOptions (ParseOptions a b c d e) = ParseOptions a b c2 d2 e
    where
        c2 ~(x,y) = map (fmap castString) $ c (castString x, y)
        d2 ~(x,y) = (castString r, map (fmap castString) s)
            where (r,s) = d (castString x, y)

