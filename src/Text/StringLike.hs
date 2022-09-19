{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

-- | /WARNING/: This module is /not/ intended for use outside the TagSoup library.
--
--   This module provides an abstraction for String's as used inside TagSoup. It allows
--   TagSoup to work with String (list of Char), ByteString.Char8, ByteString.Lazy.Char8,
--   Data.Text and Data.Text.Lazy.
module Text.StringLike (StringLike(..), fromString, castString) where

import Data.String
import Data.Typeable

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT


-- | A class to generalise TagSoup parsing over many types of string-like types.
--   Examples are given for the String type.
--
--   Note that for types which don't represent the full range of characters (e.g. ByteString)
--   entities like @"&#128512;"@ will result in incorrect output.
class (Typeable a, Eq a, IsString a) => StringLike a where
    -- | > empty = ""
    empty :: a
    -- | > cons = (:)
    cons :: Char -> a -> a
    -- | > uncons []     = Nothing
    --   > uncons (x:xs) = Just (x, xs)
    uncons :: a -> Maybe (Char, a)

    -- | > toString = id
    toString :: a -> String
    -- | > fromChar = return
    fromChar :: Char -> a
    -- | > strConcat = concat
    strConcat :: [a] -> a
    -- | > strNull = null
    strNull :: a -> Bool
    -- | > append = (++)
    append :: a -> a -> a
    -- | > strMap = map
    strMap :: (Char -> Char) -> a -> a


-- | Convert a String from one type to another.
castString :: (StringLike a, StringLike b) => a -> b
castString = fromString . toString


instance StringLike String where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
    toString = id
    fromChar = (:[])
    strConcat = concat
    empty = []
    strNull = null
    cons c = (c:)
    append = (++)
    strMap = fmap

instance StringLike BS.ByteString where
    uncons = BS.uncons
    toString = BS.unpack
    fromChar = BS.singleton
    strConcat = BS.concat
    empty = BS.empty
    strNull = BS.null
    cons = BS.cons
    append = BS.append
    strMap = BS.map

instance StringLike LBS.ByteString where
    uncons = LBS.uncons
    toString = LBS.unpack
    fromChar = LBS.singleton
    strConcat = LBS.concat
    empty = LBS.empty
    strNull = LBS.null
    cons = LBS.cons
    append = LBS.append
    strMap = LBS.map

instance StringLike T.Text where
    uncons = T.uncons
    toString = T.unpack
    fromChar = T.singleton
    strConcat = T.concat
    empty = T.empty
    strNull = T.null
    cons = T.cons
    append = T.append
    strMap = T.map

instance StringLike LT.Text where
    uncons = LT.uncons
    toString = LT.unpack
    fromChar = LT.singleton
    strConcat = LT.concat
    empty = LT.empty
    strNull = LT.null
    cons = LT.cons
    append = LT.append
    strMap = LT.map
