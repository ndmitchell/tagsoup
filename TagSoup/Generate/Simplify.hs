
module TagSoup.Generate.Simplify(simplify) where

import TagSoup.Generate.Type
import Data.Generics.PlateData
import Data.List


-- perform some simplification cleanups
-- run both before and after supercompilation

-- global:
-- foo x y z = bar x y z, replace all foo with bar, transitively
-- foo x = .... ..., where x is never used. Eliminate foo the argument where possible

-- let x = y in z, where x is used once = z[x/y]


simplify :: [Func] -> [Func]
simplify = descendBi simplifyExpr



simplifyExpr = transform f
    where
        f (ELet xy z) = eLet keep $ substs sub z
            where (sub,keep) = partition (flip once z . fst) xy
        f x = x
