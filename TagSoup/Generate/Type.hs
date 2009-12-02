{-# LANGUAGE DeriveDataTypeable #-}

module TagSoup.Generate.Type(module TagSoup.Generate.Type, Literal(..)) where

import Language.Haskell.Exts(Literal(..))

import Data.Data
import Data.Maybe
import Data.Generics.PlateData

type Var = String
type Con = String

data Func = Func {funcName :: Var, funcArgs :: [Var], funcBody :: Expr}
            deriving (Data,Typeable,Show)


data Expr = EApp Expr Expr
          | ELet [(Var,Expr)] Expr
          | ECase Expr [(Patt,Expr)]
          | EVar Var
          | ECon Con
          | ELit Literal
            deriving (Data,Typeable,Show,Eq)

data Patt = Patt Con [Var]
          | PattAny
          | PattLit Literal
            deriving (Data,Typeable,Show,Eq)

eLet [] x = x
eLet x y = ELet x y



subst :: (String,Expr) -> Expr -> Expr
subst vb = substs [vb]


substs :: [(String,Expr)] -> Expr -> Expr
substs [] x = x
substs vb x = case x of
    EVar x -> fromMaybe (EVar x) $ lookup x vb
    ELet xy z -> ELet [(x, substs vb y) | (x,y) <- xy] $ substsWithout (map fst xy) z
    ECase x alts -> ECase (substs vb x) (map f alts)
    EApp x y -> EApp (substs vb x) (substs vb y)
    x -> x
    where
        f (Patt c vs, x) = (Patt c vs, substsWithout vs x)
        f (p, x) = (p, substs vb x)
        
        substsWithout vs = substs $ filter (flip notElem vs . fst) vb


-- is the variable used once or fewer times
once :: String -> Expr -> Bool
once v x = length (filter (==EVar "") $ universe $ subst (v,EVar "") x) <= 1
