module Text.HTML.TagSoup.Tree where
    import Text.HTML.TagSoup.Type
    -- | A tree of 'Tag' values.
    data TagTree str
        = -- | A 'TagOpen'/'TagClose' pair with the 'Tag' values in between.
          TagBranch str [Attribute str] [TagTree str]
        | -- | Any leaf node
          TagLeaf (Tag str)
    tagTree :: Eq str => [Tag str] -> [TagTree str]
