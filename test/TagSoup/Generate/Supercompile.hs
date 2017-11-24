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
type Tag = String
type Split = ([Sem], [Fun] -> Expr)

data Sem = Sem [Var] Var (Map.Map Var (Tag,Redex)) deriving (Eq,Ord)

instance Show Sem where
    show (Sem free v mp) = unlines $ line1 : map f (Map.toList mp)
        where line1 = "\\" ++ unwords free ++ " -> " ++ v ++ " where"
              f (k,(s,v)) = "  " ++ k ++ " " ++ show s ++ " = " ++ showExpr (fromRedexShort v)


sem :: [Var] -> Var -> Map.Map Var (Tag,Redex) -> Sem
sem free v mp = relabel $ simplify $ relabel $ Sem free v mp



-- all the bound redexes must be fully simple
-- none of the redexes may have a root let statement

data SSimplify = SSimplify {simplifyNew :: Map.Map Var (Tag,Redex)}

simplify :: Sem -> Sem
simplify (Sem free root mp) = flip evalState (SSimplify Map.empty) $ do
    mapM_ var $ Map.keys mp
    fmap (Sem free root) $ gets simplifyNew
    where
        simp :: Var -> Tag -> Redex -> State SSimplify (Tag, Redex)
        simp v tag x = do
            (tag,x) <- redex tag x
            modify $ \s -> s{simplifyNew = Map.insert v (tag,x) $ simplifyNew s}
            return (tag,x)

        var :: Var -> State SSimplify (Tag, Redex)
        var v = do
            new <- gets simplifyNew
            case (Map.lookup v new, Map.lookup v mp) of
                (Just y, _) -> return y
                (_, Nothing) -> return $ ("unbound" ++ v,RVar v)
                (_, Just (tag,x)) -> simp v tag x

        redex :: Tag -> Redex -> State SSimplify (Tag, Redex)
        redex tag (RVar v) = var v
        redex tag (RLet vs x) = do
            sequence_ [simp v (tag ++ "_" ++ show i) x | (i,(v,x)) <- zip [1..] vs]
            redex (tag ++ "_0") x
        redex tag o@(RApp v y) = do
            x <- var v
            case x of
                (tag,RCon c xs) -> return (tag ++ "_", RCon c (xs ++ [y]))
                _ -> return (tag,o)
        redex tag o@(RCase v alts) = do
            (_,x) <- var v
            case x of
                RCon{} -> redex (tag++"_") $ head $ concatMap (f x) alts
                RLit{} -> redex (tag++"_") $ head $ concatMap (f x) alts
                _ -> return (tag,o)
            where f (RCon c vs) (Patt c2 vs2, x) | c == c2 = [RLet (zip vs2 $ map RVar vs) x]
                  f (RLit c) (PattLit c2, x) | c == c2 = [x]
                  f _ (PattAny, x) = [x]
                  f _ _ = []
        redex tag x = return (tag,x)


-- do a GC, variable normalisation, variable flattening

data SRelabel = SRelabel {relabelOld :: Map.Map Var Var, relabelNew :: Map.Map Var (Tag,Redex), relabelVars :: [Var]}

relabel :: Sem -> Sem
relabel (Sem free root mp) = flip evalState s0 $ do
    root2 <- move root
    old <- gets relabelOld
    free2 <- return [Map.findWithDefault "_" x old | x <- free]
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
                (_, Nothing) | v `elem` free -> do y <- fresh; rename v y; return y
                             | otherwise -> return v
                (_, Just (_, RVar y)) -> move y
                (_, Just (tag,x)) -> do
                    y <- fresh
                    rename v y
                    x <- f Map.empty x
                    record y (tag,x)
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
toSem x = sem args "?v" (Map.singleton "?v" ("main",bod))
    where (args,bod) = unique $ toRedex x


---------------------------------------------------------------------
-- MANAGER

supercompile :: Env -> Term -> Fun -> Sem -> Result ()
supercompile env t name x | terminate t x = f t name (stop t x)
                          | otherwise = do
        () <- return $ openLogFile name
        () <- return $ appendLogFile $ show t
        let (x',t') = reduce env x
        res <- seen x' ; case res of
            Just y -> addSeen y x >> addResult name (forward y)
            _ -> addSeen name x' >> f ((t += x') += x) name t'
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
                    -- addSeen y x
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
seen sem = fmap (lookup sem) $ gets resultSeen


---------------------------------------------------------------------
-- TERMINATION

data Term = Term [Past] deriving Show

type Past = [(String,Int)]

past (Sem _ _ mp) = map (head &&& length) $ sort $ group $ sort $ map fst $ Map.elems mp

-- return True if new is greater than old in every way
bad ((x1,x2):xs) ((y1,y2):ys) = case compare x1 y1 of
    LT -> bad xs ((y1,y2):ys)
    GT -> False
    EQ -> x2 >= y2 && bad xs ys
bad _ [] = True
bad [] _ = False


newTerm :: Term
newTerm = Term []

terminate :: Term -> Sem -> Bool
terminate (Term xs) sem | any (bad $ past sem) xs = True -- error $ show (sem, xs)
                        | otherwise = False

(+=) :: Term -> Sem -> Term
(+=) (Term xs) sem = Term $ past sem : xs



stop :: Term -> Sem -> Split
stop term (Sem free root mp) = trace "stop" $ (map snd ss, \names -> eLams free $ eLet (zip keep $ zipWith g names ss) (eVar root))
    where
        keep = fixEq op $ Map.keys mp
        ss = map (f keep) keep
        f keep w = semTop (delete w $ free ++ keep) w (Map.filterWithKey (\k _ -> k `notElem` keep || k == w) mp)
        g name (vs,_) = eApps (eFun name) (map eVar vs)

        op xs = h xs poss
            where
                poss = filter (\x -> (<=1) $ length $ filter (elem x . nub . childrenBi . flip Map.lookup mp) $ delete x xs) $ delete root xs
                h xs [] = xs
                h xs (p:oss) | all (not . terminate term . snd . f k2) k2 = h k2 oss
                             | otherwise = h xs oss
                    where k2 = delete p xs


---------------------------------------------------------------------
-- SPLIT


split :: Sem -> Split
split (Sem free root mp) = (a, eLams free . b)
    where
        (a,b) = f root
    
        f v = case fmap snd $ Map.lookup v mp of
            Just (RCase w alt) | w `Map.member` mp -> f w
                               | otherwise -> splitCase v w alt
            Just (RCon c xs) -> splitApp v (eCon c) xs
            Just (RApp x y) | x `Map.member` mp -> f x
                            | otherwise -> splitApp v (eFun x) [y]
            Just (RFun v) -> splitFun
            Just (RLit x) -> ([], \_ -> eLit x)
            Nothing -> ([], \_ -> eVar v)
            x -> ([], const $ eFun $ "how do i split on " ++ show x)


        splitFun = ([b], \[name] -> eLam "newArg" $ eApps (eFun name) $ map eVar a)
            where (a,b) = semTop (free ++ ["newArg"]) "newRoot" $ Map.insert "newRoot" ("newRoot",RApp root "newArg") mp


        splitApp v gen xs = (map snd ss, \names -> eLet ((v, eApps gen (map eVar xs)) :  zip (delete v keep) (zipWith g names ss)) (eVar root))
            where
                keep = share (Sem free root mp) $ root:v:xs
                ss = map f $ delete v keep
                f w = semTop (delete w $ free ++ keep) w (Map.filterWithKey (\k _ -> k `notElem` keep || k == w) mp)
                g name (vs,_) = eApps (eFun name) (map eVar vs)


        -- type Split = ([Sem], [Fun] -> Expr)
        splitCase v on alt = (map snd ss,
                \names -> eCase (eVar on) $ zipWith3 g names ss alt)
            where
                ss = map f alt
                f (Patt c vs, x) = semTop (vs ++ delete on free) root $ Map.insert on ("con",RCon c vs) mp
                f (PattLit c, x) = semTop (delete on free) root $ Map.insert on ("lit",RLit c) mp
                f (PattAny, x) = semTop free root $ Map.insert v ("case",x) mp
                g name (vs,_) (pat,_) = (pat, eApps (eFun name) (map eVar vs))


-- top level sem, which can throw away free vars
semTop :: [Var] -> Var -> Map.Map Var (Tag,Redex) -> ([Var], Sem)
semTop free root mp = ([v | (v,w) <- zip free free2, w /= "_"], Sem (filter (/= "_") free2) root2 mp2)
    where Sem free2 root2 mp2 = sem free root mp


-- given a Sem, I need these variables at the top level, which other ones to keep sharing? only those from mp
-- at worst, will be all the bound variables (never puts free vars in)
-- at best, will be just those in the input set that are bound in mp
share :: Sem -> [Var] -> [Var]
share (Sem free root mp) top = fixEq f (top \\ free)
    where
        f xs = filter (`Map.member` mp) $ sort $ nub $ xs ++ new
            where new = map head $ filter ((>1) . length) $ group $ sort $ concatMap follow xs
        
        follow x = case Map.lookup x mp of
            Nothing -> []
            Just r -> childrenBi r :: [String]


fixEq f x = if x == x2 then x2 else fixEq f x2
    where x2 = f x


---------------------------------------------------------------------
-- STEP


step :: Env -> Sem -> Maybe Sem
step e (Sem free root mp) = f [] root
    where
        f app v = case fmap snd $ Map.lookup v mp of
            Nothing -> Nothing
            Just (RCase on _) -> f [] on
            Just (RApp x y) -> f ((v,y):app) x
            Just (RFun x) | length args == 0 -> Just $ sem free root $ Map.insert v (x,bod) mp
                          | length args <= length app -> Just $ sem free root $ Map.insert (fst $ last_ "step" app2) (x,new) mp
                where new = RLet (zip args $ map (RVar . snd) app2) bod
                      app2 = take (length args) app
                      (args,bod) = unique $ toRedex $ e x
            _ -> Nothing


step e sem@(Sem free root mp) = f $ flatten sem
    where
        f ((v, Just (_, RFun name)):rest) = g v [] rest
            where
                (args,bod) = unique $ toRedex $ e name

                g v seen _ | length seen == length args = Just $ sem free root $ Map.insert v (name,bod2) mp
                    where bod2 = RLet (zip args $ map RVar seen) bod
                g _ seen ((v, Just (_, RApp _ w)):rest) = g fun v (w:seen) 

        g (args,bod) v seen _ | length seen == length args = Just $ sem free root $ Map.insert v (v,bod2) mp
        
        RLet replace sem val
        g fun _ seen ((v, Just (_, RApp _ w)):rest) = g fun v (w:seen) rest
        g _ _ _ _ = Nothing

            where
                (args,bod) = unique $ toRedex $ e x
                (rep,app) = g v [] rest

                g v seen _ | length seen == length args = (v, seen)
                g v seen (v, Just 

    
    
        f app v = case fmap snd $ Map.lookup v mp of
            Nothing -> Nothing
            Just (RCase on _) -> f [] on
            Just (RApp x y) -> f ((v,y):app) x
            Just (RFun x) | length args == 0 -> Just $ sem free root $ Map.insert v (x,bod) mp
                          | length args <= length app -> Just $ sem free root $ Map.insert (fst $ last_ "step" app2) (x,new) mp
                where new = RLet (zip args $ map (RVar . snd) app2) bod
                      app2 = take (length args) app
                      (args,bod) = unique $ toRedex $ e x
            _ -> Nothing




last_ msg [] = error $ "last with " ++ msg
last_ msg x = last x



-- the first element may be a nothing, all the others must be a Just
flatten :: Sem -> [(Var, Maybe (String,Redex))]
flatten (Sem free root mp) = reverse $ f root
    where
        f v = case Map.lookup v mp of
            Nothing -> [(v, Nothing)]
            Just (s,x) -> (v, Just (s,x)) : case force x of
                Nothing -> []
                Just v -> f v


-- if you force this redex, which var will you force next
force :: Redex -> Maybe Var
force (RVar v) = Just v
force (RApp v _) = Just v
force (RCase v _) = Just v
force _ = Nothing
