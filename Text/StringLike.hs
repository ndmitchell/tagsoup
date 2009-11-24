{-# LANGUAGE FlexibleInstances #-}

module Text.StringLike where

import Data.Maybe

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.List


class Eq a => StringLike a where
    -- Primitive operations
    empty :: a
    cons :: Char -> a -> a
    uncons :: a -> Maybe (Char, a)

    toString :: a -> String
    fromString :: String -> a
    fromString1 :: Char -> a
    strConcat :: [a] -> a
    strNull :: a -> Bool
    append :: a -> a -> a
    
    toString = unfoldr uncons
    fromString1 x = cons x empty
    fromString = foldr cons empty
    strConcat = foldr append empty
    strNull = isNothing . uncons
    append x y = fromString $ toString x ++ toString y

    empty = fromString ""
    cons x y = fromString $ x : toString y
    uncons x = case toString x of
        [] -> Nothing
        x:xs -> Just (x, fromString xs)


instance StringLike [Char] where
    uncons [] = Nothing
    uncons (x:xs) = Just (x, xs)
    toString = id
    fromString = id
    strConcat = concat
    empty = []
    strNull = null
    cons c = (c:)
    append = (++)

instance StringLike BS.ByteString where
    uncons = BS.uncons
    toString = BS.unpack
    fromString = BS.pack
    strConcat = BS.concat
    empty = BS.empty
    strNull = BS.null
    cons = BS.cons
    append = BS.append

instance StringLike LBS.ByteString where
    uncons = LBS.uncons
    toString = LBS.unpack
    fromString = LBS.pack
    strConcat = LBS.concat
    empty = LBS.empty
    strNull = LBS.null
    cons = LBS.cons
    append = LBS.append
