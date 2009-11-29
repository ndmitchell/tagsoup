
module TagSoup.Generate.All(generate) where

import Language.Haskell.Exts
import Control.Monad



generate :: IO ()
generate = do
    let parse x = fromParseResult `fmap` parseFileWithExts [] x
    spec <- parse "Text/HTML/TagSoup/Specification.hs"
    impl <- parse "Text/HTML/TagSoup/Implementation.hs"
    putStr "Optimising... "
    writeFile "Text/HTML/TagSoup/Generated.hs" $ optimise $ mergeModules spec impl
    putStrLn "done"


mergeModules (Module x1 x2 x3 x4 x5 x6 x7) (Module y1 y2 y3 y4 y5 y6 y7) =
    Module x1 x2 (x3++y3) Nothing Nothing (x6++y6) (x7++y7)


types = words "String LBS BS"
modes = replicateM 3 [False,True]

showMode [pos,warn,merge] = f pos "Pos" ++ f warn "Warn" ++ f merge "Merge"
    where f b s = if b then s else ""


optimise :: Module -> String
optimise (Module x1 x2 x3 x4 x5 x6 x7) = prettyPrint m2 ++ "\n\n\n" ++ unlines fastParse
    where
        m2 = Module x1 (ModuleName "Text.HTML.TagSoup.Generated") x3 x4
                       (Just [EVar $ UnQual $ Ident "parseTagsOptions"])
                       (imports ++ filter ((/=) (ModuleName "Text.HTML.TagSoup.Implementation") . importModule) x6) x7
        imports = [ImportDecl x1 (ModuleName "Text.HTML.TagSoup.Manual") True False Nothing (Just $ ModuleName "Manual") Nothing
                  ,ImportDecl x1 (ModuleName "Data.ByteString.Char8") True False Nothing (Just $ ModuleName "BS") Nothing
                  ,ImportDecl x1 (ModuleName "Data.ByteString.Lazy.Char8") True False Nothing (Just $ ModuleName "LBS") Nothing
                  ,ImportDecl x1 (ModuleName "Data.Typeable") False False Nothing Nothing Nothing
                  ,ImportDecl x1 (ModuleName "Data.Maybe") False False Nothing Nothing Nothing]

        fastParse =
            ["type LBS = LBS.ByteString"
            ,"type BS = BS.ByteString"
            ,"type EntData str = str -> [Tag str]"
            ,"type EntAttrib str = (str,Bool) -> (str,[Tag str])"
            ,""
            ,"parseTagsOptions :: StringLike str => ParseOptions str -> str -> [Tag str]"] ++
            concat [
                ["parseTagsOptions opts x | Just (ParseOptions pos warn entData entAttrib merge) <- cast (opts,x) ="
                ,"    fromJust $ cast $ case (pos,warn,merge) of"] ++
                ["        ("++pb++","++wb++","++mb++") -> fp"++t++ showMode m ++ " entData entAttrib" | m <- modes, let [pb,wb,mb] = map show m]
                | t <- types] ++
            ["parseTagsOptions opts x = Manual.parseTagsOptions opts x"] ++
            concat [
                [""
                ,"fp"++t++showMode m++ " :: EntData " ++ t ++ " -> EntAttrib " ++ t ++ " -> " ++ t ++ " -> [Tag " ++ t ++ "]"
                ,"fp"++t++showMode m++ " entData entAttrib x = output (ParseOptions "++pb++" "++wb++" entData entAttrib "++mb ++ ") $ parse $ toString x"]
                | t <- types, m <- modes, let [pb,wb,mb] = map show m]



{-





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
-}
