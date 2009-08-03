
module Text.StringLike where

import Data.Maybe

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List


class CharLike a where
    toChar :: a -> Char
    fromChar :: Char -> a

instance CharLike Char where
    toChar = id
    fromChar = id


class StringLike a where
    -- Primitive operations
    empty :: a
    cons :: Char -> a -> a
    uncons :: a -> Maybe (Char, a)

    toString :: a -> String
    fromString :: String -> a
    concat :: [a] -> a
    isEmpty :: a -> Bool
    append :: a -> a -> a
    
    toString = unfoldr uncons
    fromString = foldr cons empty
    concat = foldr append empty
    isEmpty = isNothing . uncons
    append x y = fromString $ toString x ++ toString y

instance CharLike a => StringLike [a] where
    uncons [] = Nothing
    uncons (x:xs) = Just (toChar x, xs)
    toString = map toChar
    fromString = map fromChar
    concat = Prelude.concat
    empty = []
    isEmpty = null
    cons c = (:) (fromChar c)
    append = (++)

instance StringLike BS.ByteString where
    uncons = BS.uncons
    toString = BS.unpack
    fromString = BS.pack
    concat = BS.concat
    empty = BS.empty
    isEmpty = BS.null
    cons = BS.cons
    append = BS.append

instance StringLike LBS.ByteString where
    uncons = LBS.uncons
    toString = LBS.unpack
    fromString = LBS.pack
    concat = LBS.concat
    empty = LBS.empty
    isEmpty = LBS.null
    cons = LBS.cons
    append = LBS.append
