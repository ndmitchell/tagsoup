{-# LANGUAGE PatternGuards, RecordWildCards, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module TagSoup.Generate.Supercompile(supercompile) where

import TagSoup.Generate.Type
import TagSoup.Generate.Simplify
import TagSoup.Generate.Convert
import Data.Maybe

import qualified Data.Map as Map
import System.IO.Unsafe


type Env = Var -> Func


supercompile :: Prog -> Prog
supercompile = {- supero . -} simplifyProg . reduceArity . forwarding



supero :: Prog -> Prog
supero prog = unsafePerformIO $ do
    writeFile "test.log" $ showExpr $ funcBody $ func "main"
    error "done"
    where
        func = getFunc prog

        e = head . evaluate func
        f x | dull x = x
            | null xs = x
            | otherwise = f $ head xs
            where xs = evaluate func x



---------------------------------------------------------------------
-- EVALUATE
-- what are the possible 1-step unfoldings, always in simplest form
evaluate :: Env -> Expr -> [Expr]
evaluate func = map simplifyExpr . f
    where
        f (fromEApps -> (EFun y, zs)) | Func{..} <- func y, length zs >= length funcArgs =
            let (zs1,zs2) = splitAt (length funcArgs) zs
            in [eApps (eLet (zip funcArgs zs1) funcBody) zs2]

        f (ECase _ on alts) = map (`eCase` alts) $ evaluate func on

        f _ = []


---------------------------------------------------------------------
-- DULL
-- is this expression dull
dull :: Expr -> Bool
dull (ECase _ (EVar x) _) = True
dull (fromEApps -> (EVar x, _)) = True
dull _ = False


---------------------------------------------------------------------
-- RESIDATE
-- possibly with respect to the function who stopped you last time
residuate :: Maybe Expr -> Expr -> ([Expr], [Var] -> Expr)
residuate = undefined

