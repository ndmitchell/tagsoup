
module Text.StringLike where

import Data.Maybe

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS


class CharLike a where
    toChar :: a -> Char
    fromChar :: Char -> a

instance CharLike Char where
    toChar = id
    fromChar = id


class StringLike a where
    uncons :: a -> Maybe (Char, a)
    toString :: a -> String
    fromString :: String -> a
    concat :: [a] -> a
    empty :: a
    isEmpty :: a -> Bool
    cons :: Char -> a -> a
    append :: a -> a -> a
    
    isEmpty = isNothing . uncons 

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
