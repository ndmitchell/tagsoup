
module Text.HTML.TagSoup.Options where

import Text.StringLike
import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Entity


data ParseOptions str = ParseOptions
    {optTagPosition :: Bool -- ^ Should 'TagPosition' values be given before every item
    ,optTagWarning :: Bool -- ^ Should 'TagWarning' values be given
    ,optLookupEntity :: str -> [Tag str] -- ^ How to lookup an entity
    }


-- Default 'ParseOptions' structure
parseOptions :: StringLike str => ParseOptions str
parseOptions = ParseOptions False False (f . toString)
    where
        f x = case lookupEntity x of
                  Nothing -> [TagText $ fromString $ "&" ++ x ++ [';'| x/=""]
                             ,TagWarning $ fromString $ "Unknown entity: &" ++ x ++ ";"]
                  Just y -> [TagText $ fromString [y]]
