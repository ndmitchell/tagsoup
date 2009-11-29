
module TagSoup.Generate.HSE(
    module Language.Haskell.Exts,
    module TagSoup.Generate.HSE) where

import Language.Haskell.Exts hiding (var)


apps = foldl App
var = Var . UnQual . Ident
con = Con . UnQual . Ident
sl = SrcLoc "" 0 0
