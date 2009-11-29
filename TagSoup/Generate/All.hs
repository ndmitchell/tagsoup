
module TagSoup.Generate.All(generate) where

import Control.Monad
import Data.List

import TagSoup.Generate.HSE
import TagSoup.Generate.Type
import TagSoup.Generate.Convert
import TagSoup.Generate.Desugar
import TagSoup.Generate.Supercompile


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
optimise (Module x1 x2 x3 x4 x5 x6 x7) = unlines $
    ["{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables #-}"
    ,"module Text.HTML.TagSoup.Generated(parseTagsOptions) where"
    ,"import qualified Text.HTML.TagSoup.Manual as Manual"
    ,"import Data.Typeable"
    ,"import Data.Maybe"
    ,"import qualified Data.ByteString.Char8 as BS"
    ,"import qualified Data.ByteString.Lazy.Char8 as LBS"] ++
    map prettyPrint (nub $ filter ((/=) (ModuleName "Text.HTML.TagSoup.Implementation") . importModule) x6) ++
    ["type LBS = LBS.ByteString"
    ,"type BS = BS.ByteString"
    ,"type EntData str = str -> [Tag str]"
    ,"type EntAttrib str = (str,Bool) -> (str,[Tag str])"] ++
    map prettyPrint typedefs ++
    ["{-# NOINLINE parseTagsOptions #-}"
    ,"parseTagsOptions :: StringLike str => ParseOptions str -> str -> [Tag str]"] ++
    concat [
        ["parseTagsOptions opts x | Just (ParseOptions pos warn entData entAttrib merge) <- cast (opts,x) ="
        ,"    fromJust $ cast $ case (pos,warn,merge) of"] ++
        ["        ("++pb++","++wb++","++mb++") -> fp"++t++ showMode m ++ " entData entAttrib" | m <- modes, let [pb,wb,mb] = map show m]
        | t <- types] ++
    ["parseTagsOptions opts x = Manual.parseTagsOptions opts x"] ++
    concat [
        [""
        ,"{-# NOINLINE fp"++t++showMode m++ " #-}"
        ,"fp"++t++showMode m++ " :: EntData " ++ t ++ " -> EntAttrib " ++ t ++ " -> " ++ t ++ " -> [Tag " ++ t ++ "]"
        ,"fp"++t++showMode m++ " entData entAttrib x = main x"
        ,"    where"] ++
        map ("    "++) (concatMap (lines . prettyPrint) $ {- output $ supercompile $ input $ -} mainFunc t m :decls)
        | t <- types, m <- modes, let [pb,wb,mb] = map show m]
    where
        (decls,typedefs) = partition isDecl $ desugar x7


mainFunc t [a,b,c] = FunBind [Match sl (Ident "main") [PVar $ Ident "x"] Nothing (UnGuardedRhs bod) (BDecls [])]
    where
        bod = App (App (vr "output") popts) $ Paren $ App (vr "parse") $ Paren $ App (vr "toString") $ vr "x"
        popts = Paren $ apps (vr "ParseOptions") [vb a,vb b,vr "entData",vr "entAttrib",vb c]

        sl = SrcLoc "" 0 0
        vr = Var . UnQual . Ident
        vb = Con . UnQual . Ident . show

isDecl PatBind{} = True
isDecl FunBind{} = True
isDecl TypeSig{} = True
isDecl InfixDecl{} = True
isDecl _ = False


desugar :: [Decl] -> [Decl]
desugar = drop (length recordTypes) . desugarRecords . (recordTypes++)


recordTypes = [typeTag, typeParseOptions]

typeTag = DataDecl sl DataType [] (Ident "Tag") [] (map (QualConDecl sl [] []) cons) []
    where cons = [ConDecl (Ident x) (replicate n $ UnBangedTy $ TyVar $ Ident "a") | (x,n) <- zip names cs]
          names = words "TagOpen TagClose TagText TagComment TagWarning TagPosition"
          cs = [2,1,1,1,1,2]

typeParseOptions = DataDecl sl DataType [] nam [] [QualConDecl sl [] [] con] []
    where con = RecDecl nam [([Ident x], UnBangedTy $ TyVar $ Ident "a") | x <- flds]
          nam = Ident "ParseOptions"
          flds = words "optTagPosition optTagWarning optEntityData optEntityAttrib optTagTextMerge"
