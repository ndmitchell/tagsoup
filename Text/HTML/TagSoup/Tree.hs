{-|
    This module is preliminary and may change at a future date.
    If you wish to use its features, please email me and I will
    help evolve an API that suits you.
-}

module Text.HTML.TagSoup.Tree
    {-# DEPRECATED "Not quite ready for use yet, email me if it looks useful to you" #-}
    (
    TagTree(..), tagTree,
    flattenTree, transformTree, universeTree
    ) where

import Text.HTML.TagSoup.Type
import Control.Arrow


data TagTree str = TagBranch str [Attribute str] [TagTree str]
                 | TagLeaf (Tag str)
                   deriving Show

instance Functor TagTree where
    fmap f (TagBranch x y z) = TagBranch (f x) (map (f***f) y) (map (fmap f) z)
    fmap f (TagLeaf x) = TagLeaf (fmap f x)


-- | Convert a list of tags into a tree. This version is not lazy at
--   all, that is saved for version 2.
tagTree :: Eq str => [Tag str] -> [TagTree str]
tagTree = g
    where
        g :: Eq str => [Tag str] -> [TagTree str]
        g [] = []
        g xs = a ++ map TagLeaf (take 1 b) ++ g (drop 1 b)
            where (a,b) = f xs

        -- the second tuple is either null or starts with a close
        f :: Eq str => [Tag str] -> ([TagTree str],[Tag str])
        f (TagOpen name atts:rest) =
            case f rest of
                (inner,[]) -> (TagLeaf (TagOpen name atts):inner, [])
                (inner,TagClose x:xs)
                    | x == name -> let (a,b) = f xs in (TagBranch name atts inner:a, b)
                    | otherwise -> (TagLeaf (TagOpen name atts):inner, TagClose x:xs)
                _ -> error "TagSoup.Tree.tagTree: safe as - forall x . isTagClose (snd (f x))"

        f (TagClose x:xs) = ([], TagClose x:xs)
        f (x:xs) = (TagLeaf x:a,b)
            where (a,b) = f xs
        f [] = ([], [])


flattenTree :: [TagTree str] -> [Tag str]
flattenTree xs = concatMap f xs
    where
        f (TagBranch name atts inner) =
            TagOpen name atts : flattenTree inner ++ [TagClose name]
        f (TagLeaf x) = [x]


universeTree :: [TagTree str] -> [TagTree str]
universeTree = concatMap f
    where
        f t@(TagBranch _ _ inner) = t : universeTree inner
        f x = [x]


transformTree :: (TagTree str -> [TagTree str]) -> [TagTree str] -> [TagTree str]
transformTree act = concatMap f
    where
        f (TagBranch a b inner) = act $ TagBranch a b (transformTree act inner)
        f x = act x
