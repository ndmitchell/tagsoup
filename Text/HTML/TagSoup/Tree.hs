{-|
    /NOTE/: This module is preliminary and may change at a future date.

    This module is intended to help converting a list of tags into a
    tree of tags.
-}

module Text.HTML.TagSoup.Tree
    (
    TagTree(..), tagTree,
    flattenTree, transformTree, universeTree
    ) where

import Text.HTML.TagSoup.Type
import Control.Arrow


data TagTree str = TagBranch str [Attribute str] [TagTree str]
                 | TagLeaf (Tag str)
                   deriving (Eq,Ord,Show)

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


-- | This operation is based on the Uniplate @universe@ function. Given a
--   list of trees, it returns those trees, and all the children trees at
--   any level. For example:
--
-- > universeTree
-- >    [TagBranch "a" [("href","url")] [TagBranch "b" [] [TagLeaf (TagText "text")]]]
-- > == [TagBranch "a" [("href","url")] [TagBranch "b" [] [TagLeaf (TagText "text")]]]
-- >    ,TagBranch "b" [] [TagLeaf (TagText "text")]]
--
--   This operation is particularly useful for queries. To collect all @\"a\"@
--   tags in a tree, simply do:
--
-- > [x | x@(TagBranch "a" _ _) <- universeTree tree]
universeTree :: [TagTree str] -> [TagTree str]
universeTree = concatMap f
    where
        f t@(TagBranch _ _ inner) = t : universeTree inner
        f x = [x]


-- | This operation is based on the Uniplate @transform@ function. Given a
--   list of trees, it applies the function to every tree in a bottom-up
--   manner. This operation is useful for manipulating a tree - for example
--   to make all tag names upper case:
--
-- > upperCase = transformTree f
-- >   where f (TagBranch name atts inner) = [TagBranch (map toUpper name) atts inner]
-- >         f x = [x]
transformTree :: (TagTree str -> [TagTree str]) -> [TagTree str] -> [TagTree str]
transformTree act = concatMap f
    where
        f (TagBranch a b inner) = act $ TagBranch a b (transformTree act inner)
        f x = act x
