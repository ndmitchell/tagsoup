{-# LANGUAGE PatternGuards, RecordWildCards, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module TagSoup.Generate.Supercompile(supercompile) where

import TagSoup.Generate.Type
import TagSoup.Generate.Simplify
import TagSoup.Generate.Convert
import Data.Maybe

import qualified Data.Map as Map
import Control.Monad
import System.IO.Unsafe
import Data.List


type Env = Var -> Func
type UExpr = Unique Expr


supercompile :: Prog -> Prog
supercompile = supero . simplifyProg . map (unique . normalise) . reduceArity . forwarding



supero :: Prog -> Prog
supero prog = unsafePerformIO $ do
    writeFile "test.log" $ show $ unique $ s $ s $ s $ s $ s $ s $ toSem $ func "main"
    error "done"
    where
        s :: Unique Sem -> Unique Sem
        s x = do x <- x ; step func x
    
        func = getFunc prog
{-
        f :: Func -> Func
        f x | dull $ funcBody x = x
            | null xs = x
            | otherwise = f $ head xs
            where
                xs = evaluate func x
-}

newtype Sem = Sem [(Var,Expr)]

instance Show Sem where
    show (Sem xs) = unlines [x ++ " := " ++ showExpr y | (x,y) <- xs]



toSem :: Func -> Unique Sem
toSem x = do
    x <- normalise x
    return $ Sem [("root",funcBody x)]


step :: Env -> Sem -> Unique Sem
step func (Sem ((root, fromEApps -> (EFun y, zs)) : rest)) | fun@Func{..} <- func y, length zs == length funcArgs = do
    Func{..} <- normalise fun
    let (zs1,zs2) = splitAt (length funcArgs) zs
    return $ Sem $ (root,funcBody) : zip funcArgs zs ++ rest

step func (Sem ((root, ECase _ (EVar x) alts) : rest)) | Just (fromEApps -> (ECon y, zs)) <- lookup x rest = do
    let (p,x) = head $ filter (f y . fst) alts ++ error "no matching case in step"
    return $ Sem $ (root,x) : zip (pattVars p) zs ++ rest
    where
        f y (Patt c _) = c == y
        f y PattAny = True
        f y _ = False

step func (Sem rest@((root, fromEApps -> (EVar y, zs)) : _)) = do
    let (a,b) = partition ((==) y . fst) rest
    return $ Sem $ a ++ b

step func (Sem (x:xs)) = error $ "Don't know what to do:\n" ++ show (snd x)
{-
---------------------------------------------------------------------
-- EVALUATE
-- what are the possible 1-step unfoldings, always in simplest form
evaluate :: Env -> Func -> [Func]
evaluate func x = map g $ f $ funcBody x
    where
        g y = x{funcBody = unique $ simplifyExpr . funcBody =<< normalise x{funcBody=y}}

        f (fromEApps -> (EFun y, zs)) | Func{..} <- func y, length zs >= length funcArgs = (:[]) $
            let (zs1,zs2) = splitAt (length funcArgs) zs
            in eApps (eLet (zip funcArgs zs1) funcBody) zs2

        f (ECase _ on alts) = map (`eCase` alts) $ f on

        f (ELet _ xy z) = map (eLet xy) (f z)

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

-}

