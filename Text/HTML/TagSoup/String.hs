
module Text.HTML.TagSoup.String where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

class AsChar a where asChar :: a -> Char; fromChar :: Char -> a
instance AsChar Char where asChar = id; fromChar = id


class (Eq a, Ord a, Show a) => AsString a where
    uncons :: a -> Maybe (Char, a)
    toString :: a -> String
    fromString :: String -> a
    concat :: [a] -> a
    empty :: a

instance (Eq a, Ord a, Show a, AsChar a) => AsString [a] where
    uncons [] = Nothing
    uncons (x:xs) = Just (asChar x, xs)
    toString = map asChar
    fromString = map fromChar
    concat = Prelude.concat
    empty = []

instance AsString BS.ByteString where
    uncons = BS.uncons
    toString = BS.unpack
    fromString = BS.pack
    concat = BS.concat
    empty = BS.empty

instance AsString LBS.ByteString where
    uncons = LBS.uncons
    toString = LBS.unpack
    fromString = LBS.pack
    concat = LBS.concat
    empty = LBS.empty
