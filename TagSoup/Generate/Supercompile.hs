{-# LANGUAGE PatternGuards, RecordWildCards, ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module Supercompile(supercompiler) where

import Type
import Simplify
import Convert
import Data.Maybe

import qualified Data.Map as Map
import Control.Monad
import System.IO.Unsafe
import System.IO
import System.Directory
import Control.Arrow
import Data.List
import Debug.Trace
import Data.Generics.PlateData
import Control.Monad.State


supercompiler :: Prog -> Prog
supercompiler = supero . simplifyProg . map (unique . normalise) . reduceArity . forwarding


supero :: Prog -> Prog
supero prog = runResult $ supercompile func newTerm "supermain" $ toSem $ func "main"
    where func = getFunc prog


---------------------------------------------------------------------
-- LOGGING

type Log = String -> ()

logFile :: FilePath -> (Log -> a) -> a
logFile file op = unsafePerformIO $ do
    createDirectoryIfMissing True "log"
    let f h str = unsafePerformIO $ hPutStrLn h str >> hFlush h
    h <- openFile ("log/" ++ file ++ ".log") WriteMode
    return $! op (f h)

dump :: String -> a -> a
dump x y = logFile "dump" $ \f -> f x `seq` y


---------------------------------------------------------------------
-- TYPE

type Env = Var -> Func

data Sem = Sem
    {semFreeVars :: [Var] -- free variables in this expression (lambda arguments when residuated)
    ,semUnique :: Int -- unique index for creating fresh variables
    ,semStack :: [Var] -- stack of expressions to evaluate
    ,semBind :: Map.Map Var Redex -- bindings
    }

instance Show Sem where
    show (Sem free _ vs mp) = unlines $ unwords ("with":free) : map (f "*") vs ++ map (f "") (Map.keys mp \\ vs)
        where f str v = str ++ v ++ " := " ++ showExpr (fromRedex $ fromJust $ Map.lookup v mp)

    showList xs = showString $ f Map.empty xs
        where
            f seen [x] = show x
            f seen [] = ""
            f seen (Sem vars i vs mp:rest) = show (Sem vars i vs $ Map.mapWithKey (\k x -> if Map.lookup k seen == Just x then rVar "..." else x) mp) ++
                "\n\n\n" ++ f mp rest


sem vs o = Sem [] minBound vs $ fix $ Map.filterWithKey (\v _ -> v `elem` vs) o
    where
        fix mp = if Map.size mp == Map.size mp2 then mp else fix mp2
            where now = concatMap freeVarsR $ Map.elems mp
                  mp2 = Map.filterWithKey (\v _ -> Map.member v mp || v `elem` now) o


toSem :: Func -> Sem
toSem x = Sem args i ["root"] (Map.singleton "root" bod)
    where (i,(args,bod)) = fromUnique $ fmap fromRLam $ toRedex =<< normalise x

apply :: Sem -> [Fun] -> Expr
apply (Sem free i vs binds) vars = eLet (zip free (map eFun vars) ++ map (second fromRedex) (Map.toList binds)) (eVar $ last vs)

runSem :: Sem -> Unique Sem -> Sem
runSem (Sem free i _ _) x = Sem free i2 vs mp
    where (Sem _ _ vs mp, i2) = runUnique x i

semMinFree = id

semAddFree xs sem = sem{semFreeVars = xs ++ semFreeVars sem}

---------------------------------------------------------------------
-- MANAGER

supercompile :: Env -> Term -> Fun -> Sem -> Result ()
supercompile env global name x = logFile ("func_" ++ name) $ \out -> f out newTerm x
    where
        f out local x | () <- out ("before step\n" ++ show x), False = error "impossible"
                      | Just e <- terminate local x = g out (Just e) [] x
                      | Just x2 <- step env x = f out (local+=x) x2
                      | otherwise = g out Nothing [] x

        g out evidence vars x = do
            () <- return $ out ("before split\n" ++ show x)
            r <- seen x
            case r of
                Just y -> addResult name (EFun y)
                Nothing -> do
                    addSeen name x
                    let (y,ys) = (id *** (++ vars)) $ split evidence x
                    case terminate global y of
                        Just e -> do
                            g out (Just e) ys y
                        Nothing -> do
                            () <- return $ out ("after split\n" ++ show y)
                            addSeen name y
                            vs <- freshNames $ length ys
                            addResult name $ apply y vs
                            () <- return $ out ("answer\n" ++ showExpr (apply y vs))
                            zipWithM_ (supercompile env (global+=y)) vs ys


---------------------------------------------------------------------
-- RESULTS

type Result a = State ResultState a

data ResultState = ResultState {resultSeen :: [(Sem,Fun)], resultFresh :: !Int, resultProg :: Prog, resultLog :: Log}


runResult :: Result a -> Prog
runResult x = logFile "result" $ \out -> resultProg $ execState x $ ResultState [] 1 [] out

addSeen :: Fun -> Sem -> Result ()
addSeen fun sem = modify $ \r -> r{resultSeen = (sem,fun) : resultSeen r}

addResult :: Fun -> Expr -> Result ()
addResult fun expr = modify $ \r -> let x = Func fun [] expr in
    resultLog r (showFunc x) `seq` r{resultProg = x : resultProg r}

freshNames :: Int -> Result [Fun]
freshNames n = do
    r <- get
    put r{resultFresh = resultFresh r + n}
    return $ map ((:) 'f' . show) $ take n [resultFresh r ..]

seen :: Sem -> Result (Maybe Fun)
seen sem = return Nothing -- FIXME

---------------------------------------------------------------------
-- TERMINATION

data Term = Term
data Evidence = Evidence

newTerm :: Term
newTerm = Term

terminate :: Term -> Sem -> Maybe Evidence
terminate _ _ = Nothing

(+=) :: Term -> Sem -> Term
(+=) x _ = x


---------------------------------------------------------------------
-- SPLIT


split :: Maybe Evidence -> Sem -> (Sem, [Sem])

split _ x = error $ "todo, split: " ++ show x

{-
split Nothing o@(Sem free i (v:vs) mp)
    | Just (ECase _ (EVar on) alts) <- Map.lookup v mp = (root, as)
    where
        Just (ECase _ (EVar on) alts) = Map.lookup v mp 
    
        as = [semMinFree $ semAddFree (pattVars p) $ runSem o $ return $ sem (v:vs) $
              Map.fromList ((v,x) : [(on,eApps (ECon c) $ map eVar vs) | Patt c vs <- [p]]) `Map.union` mp | (p,x) <- alts]

        root = semAddFree new $ runSem o{semUnique=i + length alts} $ return $ sem [v] $ Map.singleton v $ eCase (EVar on)
               [(p,eApps (eVar n) (map eVar $ semFreeVars a)) | (n,a,(p,x)) <- zip3 new as alts]
        new = take (length alts) $ map ((:) 'v' . show) [i..]

split Nothing o@(Sem free i (v:vs) mp)
    | Just (fromEApps -> (EVar x, xs)) <- Map.lookup v mp
    , not $ Map.member x mp
    = (Sem free i vs $ Map.delete v mp, []) -- FIXME: Entirely wrong
    where
     --   root = Sem free i [v] $ Map.singleton v $ eApps (EVar x) xs
     --   rest = semMinFree $ semAddFree v $ runSem o{semUnique=i + length xs} $ return $ sem 
    
split Nothing o@(Sem free i [v] mp)
    | Just (fromEApps -> (ECon x, xs)) <- Map.lookup v mp = (root, as)
    where
        Just (fromEApps -> (ECon x, xs)) = Map.lookup v mp
        as = [semMinFree $ semAddFree (delete v $ Map.keys mp) $ sem [v] $ Map.singleton v x | x <- xs]
        root = semAddFree new $ runSem o{semUnique=i + length xs} $ return $ sem [v] $ Map.singleton v $ eApps (ECon x)
               [eApps (eVar n) $ map eVar $ semFreeVars a | (n,a) <- zip new as]
        new = take (length xs) $ map ((:) 'v' . show) [i..]


split _ sem = dump (show sem) $ error "todo: split"
-}

{-
split Nothing sem@(Sem vars i (v:vs) mp)
    | Just (ECase _ (EVar on) alts) <- Map.lookup v mp = (Sem vars i2 v 

    where
        i2 = i + length alts
        let i = 
    
    
    error $ "split\n" ++ show sem


split _ _ = error "todo: split"
-}


---------------------------------------------------------------------
-- STEP SEMANTICS

step :: Env -> Sem -> Maybe Sem
step func o@(Sem vars i (v:vs) mp) = case f $ grab v of
        Nothing -> Nothing
        Just y -> Just $ runSem o y
    where
        grab x = Map.findWithDefault (error "eek, can't find") x mp
        add x = Map.fromList x `Map.union` mp

        isWhnf RCon{} = True
        isWhnf RLam{} = True
        isWhnf RLit{} = True
        isWhnf _ = False

        whnf :: Var -> (Redex -> Unique Sem) -> Maybe (Unique Sem)
        whnf w op = case Map.lookup w mp of
            Nothing -> Nothing
            Just e | isWhnf e -> Just $ op e
            Just (RApp w []) -> whnf w op
            Just _ -> Just $ return $ sem (w:v:vs) $ add []

        f x | isWhnf x = if null vs then Nothing else Just $ return $ sem vs mp

        f (RFun x) = Just $ do
            y <- toRedex $ func x
            return $ sem (v:vs) $ add [(v,y)]

        f (RLet bind x) = Just $ return $ sem (v:vs) $ add $ (v,x) : bind

        f (RApp x []) = case Map.lookup x mp of
            Nothing -> Nothing {- case vs of
                [] -> Nothing
                v2:vs2 -> Just $ return $ sem vs $ add [(v2, repVar v x $ grab v2)]  -}
            Just e -> Just $ return $ sem (v:vs) $ add [(v,e),(x,rVar v)]

        f (RCase on alts) = whnf on $ \(RCon c xs) -> do
                let (pattVars -> ps, x) = head $ filter (g c . fst) alts ++ error "no matching case in step"
                return $ sem (v:vs) $ add $ (v,x) : zip ps (map rVar xs)
            where
                g y (Patt c _) = c == y
                g y PattAny = True
                g y _ = False

        f (RApp x xs) | xs /= [] = whnf x $ \x -> do
            case x of
                RLam{}-> do
                    RLam xv xx <- normaliseRedex x
                    let n = min (length xs) (length xv)
                    w <- fresh
                    return $ sem (v:vs) $ add $ if null $ drop n xs
                        then (v,rLam (drop n xv) xx) : zip xv (map rVar xs)
                        else (v,RApp w (drop n xs)) : (w,rLam (drop n xv) xx) : zip xv (map rVar xs)
                RCon c ys -> do
                    return $ sem vs $ add [(v, RCon c (ys++xs))]


        f x = dump (showExpr $ fromRedex x) $ error $ "Don't know what to do on expression"


repVar :: Var -> Var -> Redex -> Redex
repVar from to = transform f
    where -- f (EVar x) = EVar $ if x == from then to else x
          f x = error "todo, repVar on Redex"



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


