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
import System.IO
import Data.List
import Debug.Trace
import Data.Generics.PlateData

type Env = Var -> Func
type UExpr = Unique Expr


supercompile :: Prog -> Prog
supercompile = {- supero . -} simplifyProg . map (unique . normalise) . reduceArity . forwarding



supero :: Prog -> Prog
supero prog = unsafePerformIO $ do
    writeFile "test.log" $ show $ unique $ ss 150 $ toSem $ func "main"
    error "done"
    where
        ss 0 x = do x <- x; return [x]
        ss n x = do
            x <- x
            case step func x of
                Just x -> do x <- x; xs <- ss (n-1) (return x) ; return $ x:xs
                Nothing -> return [x]
    
        func = getFunc prog
{-
        f :: Func -> Func
        f x | dull $ funcBody x = x
            | null xs = x
            | otherwise = f $ head xs
            where
                xs = evaluate func x
-}

data Sem = Sem [Var] (Map.Map Var Expr)

instance Show Sem where
    show (Sem vs mp) = unlines $ map (f "*") vs ++ map (f "") (Map.keys mp \\ vs)
        where f str v = str ++ v ++ " := " ++ showExpr (fromJust $ Map.lookup v mp)

    showList xs = showString $ f Map.empty xs
        where
            f seen [x] = show x
            f seen [] = ""
            f seen (Sem vs mp:rest) = show (Sem vs $ Map.mapWithKey (\k x -> if Map.lookup k seen == Just x then EVar "..." else x) mp) ++
                "\n\n\n" ++ f mp rest


sem vs o = Sem vs $ fix $ Map.filterWithKey (\v _ -> v `elem` vs) o
    where
        fix mp = if Map.size mp == Map.size mp2 then mp else fix mp2
            where now = [v | EVar v <- universeBi $ Map.elems mp]
                  mp2 = Map.filterWithKey (\v _ -> Map.member v mp || v `elem` now) o


toSem :: Func -> Unique Sem
toSem x = do
    x <- normalise x
    return $ Sem ["root"] (Map.singleton "root" (funcBody x))


step :: Env -> Sem -> Maybe (Unique Sem)
step func (Sem (v:vs) mp) = f $ grab v
    where
        grab x = Map.findWithDefault (error "eek, can't find") x mp
        add x = Map.fromList x `Map.union` mp

        isWhnf (fromEApps -> (ECon _, _)) = True
        isWhnf (ELam _ _ _) = True
        isWhnf _ = False

        whnf :: Expr -> (Expr -> Expr) -> (Expr -> Unique Sem) -> Maybe (Unique Sem)
        whnf (EVar w) rep op = case Map.lookup w mp of
            Nothing -> Nothing
            Just e | isWhnf e -> Just $ op e
            Just (EVar w) -> whnf (EVar w) rep op
            Just _ -> Just $ return $ sem (w:v:vs) $ add [(v, rep $ EVar w)]
        whnf e rep op | isWhnf e = Just $ op e
        whnf e rep op = Just $ do w <- fresh ; return $ sem (w:v:vs) $ add [(w,e), (v, rep $ EVar w)]

        f x | isWhnf x = if null vs then Nothing else Just $ return $ sem vs mp

        f (EFun x) = Just $ do
            Func{..} <- normalise $ func x
            return $ sem (v:vs) $ add [(v,eLams funcArgs funcBody)]

        f (EVar x) = case Map.lookup x mp of
            Nothing -> Nothing
            Just e -> Just $ return $ sem (v:vs) $ add [(v,e),(x,EVar v)]

        f (ECase _ on alts) = whnf on (`eCase` alts) $ \e -> do
                let (ECon c, xs) = fromEApps e
                    (pattVars -> ps, x) = head $ filter (g c . fst) alts ++ error "no matching case in step"
                return $ sem (v:vs) $ add $ (v,x) : zip ps xs
            where
                g y (Patt c _) = c == y
                g y PattAny = True
                g y _ = False

        f (fromEApps -> (x,xs)) | xs /= [] = whnf x (`eApps` xs) $ \x -> do
            case fromEApps x of
                (ELam{},_) -> do
                    (xv,xx) <- fmap fromELams $ normaliseExpr x
                    let n = min (length xs) (length xv)
                    w <- fresh
                    return $ sem (v:vs) $ add $ if null $ drop n xs
                        then (v,eLams (drop n xv) xx) : zip xv xs
                        else (v,eApps (EVar w) (drop n xs)) : (w,eLams (drop n xv) xx) : zip xv xs
                (ECon c,ys) -> do
                    xs2 <- freshN $ length ys
                    ys2 <- freshN $ length ys
                    -- NOTE: Only time we ever create an entirely new expression not there before
                    return $ sem vs $ add $ (v,eApps (ECon c) (map EVar $ xs2++ys2)) : zip xs2 xs ++ zip ys2 ys


        f x = error $ "Don't know what to do:\n" ++ showExpr x



{-

| fun@Func{..} <- func y, length zs == length funcArgs = do
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
-}
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


