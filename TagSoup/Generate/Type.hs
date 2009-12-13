{-# LANGUAGE DeriveDataTypeable #-}

module TagSoup.Generate.Type(module TagSoup.Generate.Type, Literal(..)) where

import Language.Haskell.Exts(Literal(..))

import Data.Data
import Data.Maybe
import Data.List
import Data.Generics.PlateData
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Map as Map

type Var = String
type Con = String
type Fun = String


type Prog = [Func]

data Func = Func {funcName :: Fun, funcArgs :: [Var], funcBody :: Expr}
            deriving (Data,Typeable,Show)

data Expr = ECon  Con
          | EFun  Fun
          | ELit  Literal
          | EVar  Var
          | EApp  I Expr Expr
          | ECase I  Expr [(Patt,Expr)]
          | ELet  I [(Var,Expr)] Expr
          | ELam  I Var Expr -- not supported in many places!
            deriving (Data,Typeable,Show,Eq)


data Patt = Patt Con [Var]
          | PattAny
          | PattLit Literal
            deriving (Data,Typeable,Show,Eq)

pattVars (Patt _ vs) = vs
pattVars _ = []


getFunc :: Prog -> (Var -> Func)
getFunc prog = \name -> fromMaybe (error $ "Couldn't find function " ++ name) $ func name
    where func = getFuncMaybe prog

getFuncMaybe :: Prog -> (Var -> Maybe Func)
getFuncMaybe prog = \name -> Map.lookup name mp
    where mp = Map.fromList $ map (funcName &&& id) prog


-- the Expr is varNames is always undefined, just for a fast subst test
type I = Map.Map Var Int

getI :: Expr -> Map.Map Var Int
getI (ECon _) = Map.empty
getI (ELit _) = Map.empty
getI (EFun _) = Map.empty
getI (EVar x) = Map.singleton x 1
getI (EApp i _ _) = i
getI (ECase i _ _) = i
getI (ELet i _ _) = i
getI (ELam i _ _) = i

unionI = Map.unionWith (+)
unionsI = Map.unionsWith (+)
minusI x [] = x
minusI x (y:ys) = minusI (Map.delete y x) ys

eCon x = ECon x
eFun x = EFun x
eLit x = ELit x
eVar x = EVar x
eApp x y = EApp (getI x `unionI` getI y) x y
eCase x y = ECase (unionI (getI x) alts) x y
    where alts = Map.unionsWith max [minusI (getI x) (pattVars p) | (p,x) <- y] 
eLet [] y = y
eLet x y = ELet (unionI binds body) x y
    where
        binds = unionsI $ map (getI . snd) x
        body = minusI (getI y) $ map fst x

eLam v x = ELam (minusI (getI x) [v]) v x

eApps x [] = x
eApps x y = eApp (eApps x (init y)) (last y)

eLams [] x = x
eLams (v:vs) x = eLam v (eLams vs x)

fromELams (ELam _ v x) = (v:a, b)
    where (a,b) = fromELams x
fromELams x = ([], x)

fromEApps (EApp _ x y) = (a, b ++ [y])
    where (a,b) = fromEApps x
fromEApps x = (x, [])


subst :: Var -> Expr -> Expr -> Expr
subst v b = runIdentity . substsWith return (Map.singleton v b)


substs :: [(Var,Expr)] -> Expr -> Expr
substs bs = runIdentity . substsWith return (Map.fromList bs)

substsWith :: Monad m => (Expr -> m Expr) -> Map.Map Var Expr -> Expr -> m Expr
substsWith op mp x | Map.null mp2 = return x
                   | otherwise = op =<< case x of
        EVar v -> return $ Map.findWithDefault (error "substs logic error") v mp2 -- must be there, or would have gone the empty route
        EApp _ x y -> liftM2 eApp (f mp2 x) (f mp2 y)
        ECase _ x y -> liftM2 eCase (f mp2 x) $ sequence [liftM ((,) a) $ f (minusI mp2 $ pattVars a) b | (a,b) <- y]
        ELet _ xy z -> liftM2 eLet (mapM (\(x,y) -> liftM ((,) x) $ f mp2 y) xy) (f (minusI mp2 $ map fst xy) z)
        _ -> error "substs logic error (2)"
    where mp2 = mp `Map.intersection` getI x
          f = substsWith op


-- is the variable used at most once down any evaluation path
linear :: Var -> Expr -> Bool
linear v x = Map.findWithDefault 0 v (getI x) <= 1


freeVars :: Expr -> [Var]
freeVars = Map.keys . getI


disjoint x y = null $ x `intersect` y


-- normalise bound variables and make them all unique
normalise :: Func -> Unique Func
normalise (Func name args bod) = do
    vs <- freshN (length args)
    fmap (Func name vs) $ f (Map.fromList $ zip args vs) bod
    where
        f mp (EVar v) = return $ eVar $ Map.findWithDefault v v mp
        f mp (EApp _ x y) = liftM2 eApp (f mp x) (f mp y)
        f mp (ECase _ x y) = do
            x <- f mp x
            y <- mapM (g mp) y
            return $ eCase x y
        f mp (ELet _ xy z) = do
            let (xs,ys) = unzip xy
            vs <- freshN (length xy)
            xy <- fmap (zip vs) $ mapM (f mp) ys
            z <- f (Map.fromList (zip xs vs) `Map.union` mp) z
            return $ eLet xy z
        f mp (ELam _ x y) = do
            v <- fresh
            y <- f (Map.singleton x v `Map.union` mp) y
            return $ eLam v y
        f mp x = return x

        g mp (Patt c vs, x) = do
            vs2 <- freshN (length vs)
            fmap ((,) (Patt c vs2)) $ f (Map.fromList (zip vs vs2) `Map.union` mp) x
        g mp (p,x) = fmap ((,) p) $ f mp x


normaliseExpr = fmap funcBody . normalise . Func "" []


type Unique a = State Int a


runUnique :: Unique a -> Int -> (a, Int)
runUnique = runState

unique :: Unique a -> a
unique = flip evalState 1

fresh :: Unique String
fresh = do
    i <- get
    put $ i+1
    return $ "v" ++ show i

freshN :: Int -> Unique [String]
freshN n = replicateM n fresh

fromUnique :: Unique a -> (Int,a)
fromUnique = (\(x,y) -> (y,x)) . flip runState 1

toUnique :: Int -> Unique a -> Unique a
toUnique i x = do
    put i
    x
