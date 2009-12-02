
module TagSoup.Generate.Type(module TagSoup.Generate.Type, Literal(..)) where

import Language.Haskell.Exts(Literal(..))

type Var = String
type Con = String

data Func = Func Var [Var] Expr


data Expr = EApp Expr Expr
          | ELet [(Var,Expr)] Expr
          | ECase Expr [(Patt,Expr)]
          | EVar Var
          | ECon Con
          | ELit Literal

data Patt = Patt Con [Var]
          | PattAny
          | PattLit Literal
