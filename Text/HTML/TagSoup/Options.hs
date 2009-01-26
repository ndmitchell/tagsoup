
module Text.HTML.TagSoup.Options where

import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Entity


data ParseOptions = ParseOptions
    {optTagPosition :: Bool -- ^ Should 'TagPosition' values be given before every item
    ,optTagWarning :: Bool -- ^ Should 'TagWarning' values be given
    ,optLookupEntity :: String -> [Tag] -- ^ How to lookup an entity
    }


-- Default 'ParseOptions' structure
parseOptions :: ParseOptions
parseOptions = ParseOptions False False f
    where
        f x = case lookupEntity x of
                  Nothing -> [TagText $ "&" ++ x ++ [';'|x/=""], TagWarning $ "Unknown entity: &" ++ x ++ ";"]
                  Just x -> [TagText [x]]
