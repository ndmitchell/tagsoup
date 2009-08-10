
module TagSoup.Generate.All(generate) where

import Language.Haskell.Exts



generate :: IO ()
generate = do
    let parse x = fromParseResult `fmap` parseFileWithExts [] x
    spec <- parse "Text/HTML/TagSoup/Specification.hs"
    impl <- parse "Text/HTML/TagSoup/Implementation.hs"
    writeFile "Text/HTML/TagSoup/Parser_.hs" $ unlines $ gen spec impl


-- Strict/Lazy, Char/Word8
data Typ = Str | SW | SC | LW | LC
           deriving (Eq,Enum,Bounded,Show)

typs = [Str .. LC]

typeName Str = "String"
typeName x = show x ++ ".ByteString"


gen :: Module -> Module -> [String]
gen spec impl =
    ["module Text.HTML.TagSoup.Generated(TagSoup(..), parseTags) where"
    ,"import qualified Data.ByteString as SW"
    ,"import qualified Data.ByteString.Char8 as SC"
    ,"import qualified Data.ByteString.Lazy as LW"
    ,"import qualified Data.ByteString.Lazy.Char8 as LC"
    ,""
    ,"parseTags :: TagSoup str => str -> [Tag str]"
    ,"parseTags = parseTagsOptions parseOptions"
    ,""
    ,"class ParseOptions str where"
    ,"    parseTagsOptions :: ParseOptions str -> str -> [Tag str]"
    ,"    parseOptions :: ParseOptions str"
    ,"    parseOptionsFast :: ParseOptions str"
    ,""] ++
    concat [
        ["instance ParseOpts " ++ typeName x ++ " where"
        ,"    parseTagsOptions opts = case (optWarning opts, optPosition opts) of"] ++
        ["    (" ++ show warn ++ ", " ++ show pos ++ ") -> parseTags" ++ show x ++ concat (["Warn"|warn] ++ ["Pos"|pos])
            | warn <- [False,True], pos <- [False,True]] ++
        [""
        ,"    parseOptions = undefined"
        ,"    parseOptionsFast = undefined"
        ,""]
        | x <- typs] ++
    concat [genFunc typ warn pos
        | typ <- typs, warn <- [False,True], pos <- [False,True]]


genFunc :: Typ -> Bool -> Bool -> [String]
genFunc typ warn pos =
    [name ++ " :: ParseOptions " ++ ty ++ " -> " ++ ty ++ " -> [Tag " ++ ty ++ "]"
    ,name ++ " = error \"Todo\""]
    where
        name = "parseTags" ++ show typ ++ concat (["Warn"|warn] ++ ["Pos"|pos])
        ty = typeName typ
