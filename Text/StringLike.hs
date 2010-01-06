{-# LANGUAGE TypeSynonymInstances #-}

-- | This module is /not/ intended for use outside the TagSoup library.
module Text.StringLike where

import Data.List
import Data.Maybe
import Data.Typeable

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS


-- | A class to generalise TagSoup parsing over many types of string-like types.
class (Typeable a, Eq a) => StringLike a where
    -- Primitive operations
    empty :: a
    cons :: Char -> a -> a
    uncons :: a -> Maybe (Char, a)

    toString :: a -> String
    fromString :: String -> a
    fromChar :: Char -> a
    strConcat :: [a] -> a
    strNull :: a -> Bool
    append :: a -> a -> a


-- | Convert a String from one type to another.
castString :: (StringLike a, StringLike b) => a -> b
castString = fromString . toString


instance StringLike String where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
    toString = id
    fromString = id
    fromChar = (:[])
    strConcat = concat
    empty = []
    strNull = null
    cons c = (c:)
    append = (++)

instance StringLike BS.ByteString where
    uncons = BS.uncons
    toString = BS.unpack
    fromString = BS.pack
    fromChar = BS.singleton
    strConcat = BS.concat
    empty = BS.empty
    strNull = BS.null
    cons = BS.cons
    append = BS.append

instance StringLike LBS.ByteString where
    uncons = LBS.uncons
    toString = LBS.unpack
    fromString = LBS.pack
    fromChar = LBS.singleton
    strConcat = LBS.concat
    empty = LBS.empty
    strNull = LBS.null
    cons = LBS.cons
    append = LBS.append
