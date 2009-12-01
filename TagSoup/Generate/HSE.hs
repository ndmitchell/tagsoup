
module TagSoup.Generate.HSE(
    module Language.Haskell.Exts,
    module TagSoup.Generate.HSE) where

import Language.Haskell.Exts hiding (app, alt, paren, var, pvar, tuple)


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

