
module HSE(
    module Language.Haskell.Exts,
    module HSE) where

import Language.Haskell.Exts hiding (app, alt, paren, var, pvar, tuple, binds, EVar)


apps = foldl App
var = Var . UnQual . Ident
pvar = PVar . Ident
con = Con . UnQual . Ident
sl = SrcLoc "" 0 0

tuple [x] = x
tuple xs = Tuple xs

ptuple [x] = x
ptuple xs = PTuple xs

fromPParen (PParen x) = fromPParen x
fromPParen x = x

fromParen (Paren x) = fromParen x
fromParen x = x

moduleDecls (Module _ _ _ _ _ _ x) = x

opToExp (QVarOp op) = Var op
opToExp (QConOp op) = Con op



