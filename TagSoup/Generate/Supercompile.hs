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
import Data.IORef
import System.Directory
import Control.Arrow
import Data.List
import Debug.Trace
import Data.Generics.PlateData
import Control.Monad.State
import Control.Exception


supercompiler :: Prog -> Prog
supercompiler = supero . simplifyProg . map (unique . normalise) . reduceArity . forwarding


supero :: Prog -> Prog
supero prog = closeLogFile $ runResult $ supercompile func newTerm "supermain" $ toSem $ func "main"
    where func = getFunc prog


---------------------------------------------------------------------
-- LOGGING


{-# NOINLINE logState #-}
logState :: IORef (Maybe Handle)
logState = unsafePerformIO $ newIORef Nothing


openLogFile :: String -> ()
openLogFile file = unsafePerformIO $ do
    h <- readIORef logState
    case h of
        Just h -> hClose h
        Nothing -> createDirectoryIfMissing True "log"
    h <- openFile ("log/" ++ file ++ ".log") WriteMode
    writeIORef logState $ Just h


appendLogFile :: String -> ()
appendLogFile msg = unsafePerformIO $ do
    Just h <- readIORef logState
    hPutStrLn h msg


closeLogFile :: a -> a
closeLogFile x = unsafePerformIO $ finally (return $! x) $ do
    h <- readIORef logState
    when (isJust h) $ hClose $ fromJust h

---------------------------------------------------------------------
-- TYPE


type Env = Var -> Func
type Split = ([Sem], [Fun] -> Expr)

data Sem = Sem [Var] Var (Map.Map Var Redex)

instance Show Sem where
    show (Sem free v mp) = unlines $ line1 : map f (Map.toList mp)
        where line1 = "\\" ++ unwords free ++ " -> " ++ v ++ " where"
              f (k,v) = "  " ++ k ++ " = " ++ showExpr (fromRedexShort v)


data SemS = SemS {semOld :: Map.Map Var (Maybe Redex, Maybe Var), semNew :: Map.Map Var Redex, semVars :: [Var]}

sem :: [Var] -> Var -> Map.Map Var Redex -> Sem
sem free v mp = relabel $ simplify $ relabel $ Sem free v mp



-- all the bound redexes must be fully simple
-- none of the redexes may have a root let statement

data SSimplify = SSimplify {simplifyNew :: Map.Map Var Redex}

simplify :: Sem -> Sem
simplify (Sem free root mp) = flip evalState (SSimplify Map.empty) $ do
    mapM_ var $ Map.keys mp
    fmap (Sem free root) $ gets simplifyNew
    where
        simp v x = do
            x <- redex x
            modify $ \s -> s{simplifyNew = Map.insert v x $ simplifyNew s}
            return x

        var v = do
            new <- gets simplifyNew
            case (Map.lookup v new, Map.lookup v mp) of
                (Just y, _) -> return y
                (_, Nothing) -> return $ RVar v
                (_, Just x) -> simp v x

        redex (RVar v) = var v
        redex (RLet vs x) = mapM_ (uncurry simp) vs >> redex x
        redex o@(RApp v y) = do
            x <- var v
            case x of
                RCon c xs -> return $ RCon c (xs ++ [y])
                _ -> return o
        redex o@(RCase v alts) = do
            x <- var v
            case x of
                RCon{} -> redex $ head $ concatMap (f x) alts
                RLit{} -> redex $ head $ concatMap (f x) alts
                _ -> return o
            where f (RCon c vs) (Patt c2 vs2, x) | c == c2 = [RLet (zip vs2 $ map RVar vs) x]
                  f (RLit c) (PattLit c2, x) | c == c2 = [x]
                  f _ (PattAny, x) = [x]
                  f _ _ = []
        redex x = return x


-- do a GC, variable normalisation, variable flattening

data SRelabel = SRelabel {relabelOld :: Map.Map Var Var, relabelNew :: Map.Map Var Redex, relabelVars :: [Var]}

relabel :: Sem -> Sem
relabel (Sem free root mp) = flip evalState s0 $ do
    free2 <- freshN (length free)
    zipWithM_ rename free free2
    root2 <- move root
    fmap (Sem free2 root2) $ gets relabelNew
    where
        s0 = SRelabel Map.empty Map.empty ['x':show i | i <- [1..]]
        fresh = do v:vs <- gets relabelVars ; modify $ \s -> s{relabelVars=vs} ; return v
        freshN n = replicateM n fresh

        rename x y = modify $ \s -> s{relabelOld = Map.insert x y $ relabelOld s}
        record x y = modify $ \s -> s{relabelNew = Map.insert x y $ relabelNew s}

        move v = do
            old <- gets relabelOld
            case (Map.lookup v old, Map.lookup v mp) of
                (Just y, _) -> return y
                (_, Nothing) -> return v
                (_, Just (RVar y)) -> move y
                (_, Just x) -> do
                    y <- fresh
                    rename v y
                    x <- f Map.empty x
                    record y x
                    return y

        f mp (RApp x y) = liftM2 RApp (var mp x) (var mp y)
        f mp (RCase x xs) = liftM2 RCase (var mp x) (mapM (alt mp) xs)
        f mp (RFun x) = return $ RFun x
        f mp (RLit x) = return $ RLit x
        f mp (RCon c xs) = liftM (RCon c) $ mapM (var mp) xs
        f mp (RVar x) = liftM RVar (var mp x)
        f mp (RLet vxs x) = do
            let (vs,xs) = unzip vxs
            vs2 <- replicateM (length vs) fresh
            xs2 <- mapM (f mp) xs
            x2 <- f (Map.fromList (zip vs vs2) `Map.union` mp) x
            return $ RLet (zip vs2 xs2) x2

        alt mp (Patt c vs, x) = do
            vs2 <- replicateM (length vs) fresh
            x2 <- f (Map.fromList (zip vs vs2) `Map.union` mp) x
            return (Patt c vs2, x2)
        alt mp (v, x) = fmap ((,) v) $ f mp x

        var mp v = case Map.lookup v mp of
            Nothing -> move v
            Just y -> return y


toSem :: Func -> Sem
toSem x = sem args "?v" (Map.singleton "?v" bod)
    where (args,bod) = unique $ toRedex x


---------------------------------------------------------------------
-- MANAGER

supercompile :: Env -> Term -> Fun -> Sem -> Result ()
supercompile env t name x | terminate t x = f t name (stop t x)
                          | otherwise = do
        () <- return $ openLogFile name
        let (x',t') = reduce env x
        res <- seen x' ; case res of
            Just y -> addSeen y x >> addResult name (forward y)
            _ -> addSeen name x' >> f (t += x') name t'
    where
        f t name (xs',gen) = do
            (names,acts) <- mapAndUnzipM (g t) xs'
            () <- addResult name (gen names)
            () <- return $ appendLogFile $ showExpr $ gen names
            sequence_ acts

        g t x = do
            res <- seen x ; case res of
                Just y -> return (y, return ())
                Nothing -> do
                    [y] <- freshNames 1
                    addSeen y x
                    return (y, supercompile env t y x)


-- return the Sem, and the split version you'd like to use
-- both are equivalent
reduce :: Env -> Sem -> (Sem, Split)
reduce env = f newTerm
    where
        f t x | () <- appendLogFile $ show x, False = undefined
              | terminate t x = (x, stop t x)
              | Just x' <- step env x = f (t += x) x'
              | otherwise = (x, split x)


forward :: Fun -> Expr
forward = EFun

{-


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

-}

---------------------------------------------------------------------
-- RESULTS

type Result a = State ResultState a

data ResultState = ResultState {resultSeen :: [(Sem,Fun)], resultFresh :: !Int, resultProg :: Prog}


runResult :: Result a -> Prog
runResult x = unsafePerformIO $ do
    writeFile "log/result.log" ""
    return $ resultProg $ execState x $ ResultState [] 1 []

addSeen :: Fun -> Sem -> Result ()
addSeen fun sem = modify $ \r -> r{resultSeen = (sem,fun) : resultSeen r}

addResult :: Fun -> Expr -> Result ()
addResult fun expr = unsafePerformIO $ do
    let x = Func fun [] expr
    appendFile "log/result.log" (showFunc x ++ "\n\n")
    return $ modify $ \r -> r{resultProg = x : resultProg r}

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

newTerm :: Term
newTerm = Term

terminate :: Term -> Sem -> Bool
terminate _ _ = False

(+=) :: Term -> Sem -> Term
(+=) x _ = x



stop :: Term -> Sem -> Split
stop = error "stop"


---------------------------------------------------------------------
-- SPLIT


split :: Sem -> Split
split (Sem free root mp) = f [] root
    where
        f args v = case Map.lookup v mp of
            Just o@(RCase w alt) | w `Map.member` mp -> f [] w
                                 | otherwise -> splitCase v w alt
            Just RCon{} -> ([], \_ -> eFun "TODO! Outer constructor")
            Just (RApp x y) -> f (y:args) x
            Nothing -> ([], \_ -> eFun $ "TODO! unresolved variable " ++ show v ++ " @ " ++ show args)
            x -> error $ "how do i split on " ++ show x ++ " @ " ++ show args




        -- type Split = ([Sem], [Fun] -> Expr)
        splitCase v on alt = (map semMin ss,
                \names -> eLams free $ eCase (eVar on) $ zip (map fst alt) (zipWith3 g alt names ss))
            where
                ss = map f alt
                f (Patt c vs, x) = sem (vs ++ delete on free) root $ Map.insert on (RCon c vs) mp
                f (PattLit c, x) = sem (delete on free) root $ Map.insert on (RLit c) mp
                f (PattAny, x) = sem free root $ Map.insert v x mp
                g (p, _) name s = eApps (eFun name) (map eVar $ semNeed s $ pattVars p ++ if isPattAny p then free else delete on free)

                semMin (Sem free root mp) = Sem (filter (/= "_") free) root mp
                semNeed (Sem free _ _) vs = [v | (f,v) <- zip free vs, f /= "_"]


---------------------------------------------------------------------
-- STEP


step :: Env -> Sem -> Maybe Sem
step e (Sem free root mp) = f [] root
    where
        f app v = case Map.lookup v mp of
            Nothing -> Nothing
            Just (RCase on _) -> f [] on
            Just (RApp x y) -> f ((v,y):app) x
            Just (RFun x) | length args == 0 -> Just $ sem free root $ Map.insert v bod mp
                          | length args <= length app -> Just $ sem free root $ Map.insert (fst $ last_ "step" app2) new mp
                where new = RLet (zip args $ map (RVar . snd) app2) bod
                      app2 = take (length args) app
                      (args,bod) = unique $ toRedex $ e x
            _ -> Nothing


last_ msg [] = error $ "last with " ++ msg
last_ msg x = last x
