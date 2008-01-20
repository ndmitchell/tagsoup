
module Text.HTML.TagSoup.Tree (
    TagTree(..), tagTree
    ) where

import Text.HTML.TagSoup.Type


-- | Invariants: there will be no TagOpen inside a TagLeaf
data TagTree = TagBranch String [Attribute] Bool [TagTree]
             | TagLeaf Tag
             deriving Show



-- | Convert a list of tags into a tree. This version is not lazy at
--   all, that is saved for version 2.
tagTree :: [Tag] -> [TagTree]
tagTree = g
    where
        g :: [Tag] -> [TagTree]
        g [] = []
        g xs = a ++ map TagLeaf (take 1 b) ++ g (drop 1 b)
            where (a,b) = f xs

        -- the second tuple is either null or starts with a close
        f :: [Tag] -> ([TagTree],[Tag])
        f (TagOpen name atts:xs) =
            case f xs of
                (inner,[]) -> (TagBranch name atts False inner:[], [])
                (inner,TagClose x:xs)
                    | x == name -> let (a,b) = f xs in (TagBranch name atts True inner:a, b)
                    | otherwise -> (TagBranch name atts False []:inner, TagClose x:xs)

        f (TagClose x:xs) = ([], TagClose x:xs)
        f (x:xs) = (TagLeaf x:a,b)
            where (a,b) = f xs
        f [] = ([], [])
