
module Main(main) where

import Control.Monad
import Data.List
import Data.Generics.PlateData

import HSE
import Type
import Convert
import qualified Desugar
import Supercompile


main :: IO ()
main = do
    let parse x = fmap fromParseResult $ parseFileWithMode mode x
        mode = defaultParseMode{fixities=infixr_ 5 ["&"] ++ baseFixities}
    spec <- parse "Text/HTML/TagSoup/Specification.hs"
    impl <- parse "Text/HTML/TagSoup/Implementation.hs"
    putStrLn "Optimising... "
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
    ,"type EntAttrib str = (str,Bool) -> (str,[Tag str])"
    ,"patternMatchFail = error \"Pattern match fail\""
    ,"primEq x y = x == y"
    ,"primGeq x y = x >= y"
    ,"primLeq x y = x <= y"] ++
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
        map ("    "++) (concatMap (lines . prettyPrint) $ output $ supercompiler $ input $ mainFunc t m : decls)
        | t <- types, m <- modes, let [pb,wb,mb] = map show m]
    where
        (decls,typedefs) = partition isDecl $ desugar $ prelude ++ x7


mainFunc t [a,b,c] = fromParseResult $ parse $
    "main = \\x -> output (ParseOptions " ++ show a ++ " " ++ show b ++ " entData entAttrib " ++ show c ++ ") (parse (toString x))"

prelude = map (fromParseResult . parse)
    ["id x = x"
    ,"seq x y = y"
    ,"null x = case x of z:zs -> False ; _ -> True"
    ,"not x = case x of True -> False; _ -> True"
    ,"head x = case x of z:zs -> z ; _ -> error \"head on empty\""
    ,"tail x = case x of z:zs -> zs; _ -> error \"tail on empty\""
    ,"dol x y = x y"
    ,"dot f g x = f (g x)"
    ,"opOr x y = if x then True else y"
    ,"opAnd x y = if x then y else False"
    ,"opEq x y = primEq x y"
    ,"opGeq x y = primGeq x y"
    ,"opLeq x y = primLeq x y"
    ,"opPlusPlus x y = case x of [] -> y ; z:zs -> z : opPlusPlus zs y"
    ,"opStarStarStar f g x = (f (fst x), g (snd x))"
    -- specific to String!
    ,"toString x = x"
    ]


isDecl PatBind{} = True
isDecl FunBind{} = True
isDecl TypeSig{} = True
isDecl InfixDecl{} = True
isDecl _ = False


---------------------------------------------------------------------
-- APP SPECIFIC DESUGAR

desugar :: [Decl] -> [Decl]
desugar =
    Desugar.singleCase ["S","(,)","(,,)","(,,,)"] .
    Desugar.core2 . Desugar.irrefutable . Desugar.untyped . expandOps . expandAmp . expandS .
    drop (length recordTypes) . Desugar.records . (recordTypes++)


recordTypes = map (fromParseResult . parse)
    ["data Tag a = TagOpen a a | TagClose a | TagText a | TagComment a | TagWarning a | TagPosition a a"
    ,"data ParseOptions a = ParseOptions{optTagPosition,optTagWarning,optEntityData,optEntityAttrib,optTagTextMerge::a}"]


-- 'c' & _ ==> ampChar
-- v & _ ==> ampChar
-- x@C & y ==> ampOut x y
-- plus remove &/Outputable
expandAmp :: [Decl] -> [Decl]
expandAmp = transformBi f . filter (not . isAmp)
    where
        isAmp InfixDecl{} = True
        isAmp ClassDecl{} = True
        isAmp InstDecl{} = True
        isAmp _ = False

        f (InfixApp x op y) | prettyPrint op == "&" = Paren $ apps (var $ g x) [Paren x, Paren y]
        f x = x

        g Var{} = "ampChar"
        g Lit{} = "ampChar"
        g _ = "ampOut"


-- For patterns (S v ...) => v@(S ...)
-- For the type, loose the first field
-- For construction, drop the first field
expandS :: [Decl] -> [Decl]
expandS = transformBi fRec . transformBi fPat . descendBi fExp
    where
        fExp (App (Con s) x) | prettyPrint s == "S" = Con s
        fExp x = descend fExp x
        fPat (PApp s (PVar x:xs)) | prettyPrint s == "S" = PAsPat x $ PParen $ PApp s xs
        fPat x = x
        fRec (RecDecl s (x:xs)) | prettyPrint s == "S" = RecDecl s xs
        fRec x = x


expandOps :: [Decl] -> [Decl]
expandOps = transformBi f
    where
        f (Symbol "$") = Ident "dol"
        f (Symbol ".") = Ident "dot"
        f (Symbol "||") = Ident "opOr"
        f (Symbol "&&") = Ident "opAnd"
        f (Symbol "==") = Ident "opEq"
        f (Symbol ">=") = Ident "opGeq"
        f (Symbol "<=") = Ident "opLeq"
        f (Symbol "++") = Ident "opPlusPlus"
        f (Symbol "***") = Ident "opStarStarStar"
        f (Symbol x) = error $ "Unknown symbol: " ++ x
        f x = x
