{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Text.HasCase ( HasCase(..) ) where

import Data.Char (toLower)
import qualified Data.List as L

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT


-- | A class that allows case-insensitive comparison.
class HasCase a where
    caselessEq :: a -> a -> Bool

instance HasCase Char where
    caselessEq a b
        | a == b = True
        | toLower a == b = True
        | a == toLower b = True
        | otherwise = False

instance HasCase String where
    caselessEq a b = L.all (uncurry caselessEq) $ L.zip a b

instance HasCase BS.ByteString where
    caselessEq a b = L.all (uncurry caselessEq) $ BS.zip a b

instance HasCase LBS.ByteString where
    caselessEq a b = L.all (uncurry caselessEq) $ LBS.zip a b

instance HasCase T.Text where
    caselessEq a b = L.all (uncurry caselessEq) $ T.zip a b

instance HasCase LT.Text where
    caselessEq a b = L.all (uncurry caselessEq) $ LT.zip a b

