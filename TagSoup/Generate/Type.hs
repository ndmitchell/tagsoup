{-# LANGUAGE DeriveDataTypeable #-}

module TagSoup.Generate.Type(module TagSoup.Generate.Type, Literal(..)) where

import Language.Haskell.Exts(Literal(..))

import Data.Data
import Data.Maybe
import Data.List
import Data.Generics.PlateData
import Control.Arrow
import qualified Data.Map as Map

type Var = String
type Con = String


type Prog = [Func]

data Func = Func {funcName :: Var, funcArgs :: [Var], funcBody :: Expr}
            deriving (Data,Typeable,Show)

data Expr = ECon  Con
          | ELit  Literal
          | EVar  Var
          | EApp  I Expr Expr
          | ECase I  Expr [(Patt,Expr)]
          | ELet  I [(Var,Expr)] Expr
            deriving (Data,Typeable,Show,Eq)


data Patt = Patt Con [Var]
          | PattAny
          | PattLit Literal
            deriving (Data,Typeable,Show,Eq)

pattVars (Patt _ vs) = vs
pattVars _ = []


-- the Expr is varNames is always undefined, just for a fast subst test
newtype I = I {varCounts :: Map.Map Var Int}
            deriving (Data,Typeable,Show,Eq)

getI :: Expr -> Map.Map Var Int
getI (ECon _) = Map.empty
getI (ELit _) = Map.empty
getI (EVar x) = Map.singleton x 1
getI (EApp i _ _) = varCounts i
getI (ECase i _ _) = varCounts i
getI (ELet i _ _) = varCounts i

unionI = Map.unionWith (+)
unionsI = Map.unionsWith (+)
minusI x [] = x
minusI x (y:ys) = minusI (Map.delete y x) ys

eCon x = ECon x
eLit x = ELit x
eVar x = EVar x
eApp x y = EApp (I $ getI x `unionI` getI y) x y
eCase x y = ECase (I $ unionI (getI x) alts) x y
    where alts = Map.unionsWith max [minusI (getI x) (pattVars p) | (p,x) <- y] 
eLet x y = ELet (I $ unionI binds body) x y
    where
        binds = unionsI $ map (getI . snd) x
        body = minusI (getI y) $ map fst x


subst :: Var -> Expr -> Expr -> Expr
subst v b = substsWith id $ Map.singleton v b


substs :: [(Var,Expr)] -> Expr -> Expr
substs bs = substsWith id (Map.fromList bs)

substsWith :: (Expr -> Expr) -> Map.Map Var Expr -> Expr -> Expr
substsWith op mp x | Map.null mp2 = x
                   | otherwise = case x of
        EVar v -> op $ Map.findWithDefault (error "substs logic error") v mp2 -- must be there, or would have gone the empty route
        EApp _ x y -> op $ eApp (f mp2 x) (f mp2 y)
        ECase _ x y -> op $ eCase (f mp2 x) [(a, f (minusI mp2 $ pattVars a) b) | (a,b) <- y]
        ELet _ xy z -> op $ eLet (map (second $ f mp2) xy) (f (minusI mp2 $ map fst xy) z)
        _ -> error "substs logic error (2)"
    where mp2 = mp `Map.intersection` getI x
          f = substsWith op


-- is the variable used at most once down any evaluation path
linear :: Var -> Expr -> Bool
linear v x = Map.findWithDefault 0 v (getI x) <= 1


freeVars :: Expr -> [Var]
freeVars = Map.keys . getI


disjoint x y = null $ x `intersect` y
