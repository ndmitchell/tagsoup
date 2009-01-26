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


data TagTree = TagBranch String [Attribute String] [TagTree]
             | TagLeaf (Tag String)
             deriving Show



-- | Convert a list of tags into a tree. This version is not lazy at
--   all, that is saved for version 2.
tagTree :: [Tag String] -> [TagTree]
tagTree = g
    where
        g :: [Tag String] -> [TagTree]
        g [] = []
        g xs = a ++ map TagLeaf (take 1 b) ++ g (drop 1 b)
            where (a,b) = f xs

        -- the second tuple is either null or starts with a close
        f :: [Tag String] -> ([TagTree],[Tag String])
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


flattenTree :: [TagTree] -> [Tag String]
flattenTree xs = concatMap f xs
    where
        f (TagBranch name atts inner) =
            TagOpen name atts : flattenTree inner ++ [TagClose name]
        f (TagLeaf x) = [x]


universeTree :: [TagTree] -> [TagTree]
universeTree = concatMap f
    where
        f t@(TagBranch _ _ inner) = t : universeTree inner
        f x = [x]


transformTree :: (TagTree -> [TagTree]) -> [TagTree] -> [TagTree]
transformTree act = concatMap f
    where
        f (TagBranch a b inner) = act $ TagBranch a b (transformTree act inner)
        f x = act x
