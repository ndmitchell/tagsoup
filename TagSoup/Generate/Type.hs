
module TagSoup.Generate.Type where

type Var = String
type Con = String

data Func = Func Var [Var] Expr

data Expr = EApp Expr Expr
          | ELet [(Var,Expr)] Expr
          | ECase Var [((Con, [Var]),Expr)]
          | EVar Var
          | ECon Con
