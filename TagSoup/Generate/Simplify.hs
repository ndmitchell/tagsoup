{-# LANGUAGE PatternGuards #-}

module TagSoup.Generate.Simplify(simplifyProg, simplifyExpr) where

import TagSoup.Generate.Type
import Data.Generics.PlateData
import Data.List
import qualified Data.Map as Map


-- perform some simplification cleanups
-- run both before and after supercompilation

-- global:
-- foo x y z = bar x y z, replace all foo with bar, transitively
-- foo x = .... ..., where x is never used. Eliminate foo the argument where possible

-- let x = y in z, where x is used once = z[x/y]


simplifyProg :: Prog -> Prog
simplifyProg = descendBi simplifyExpr


simplifyExpr :: Expr -> Expr
simplifyExpr = transform f
    where
        f (ELet _ xy z) | not $ null sub = f $ eLet keep $ rep sub z
            where (sub,keep) = partition (flip linear z . fst) xy

        f (ECase _ on alts) | (PattAny,ECase _ on2 alts2) <- last alts, on == on2 = f $ eCase on (init alts ++ alts2)

        f (ECase _ (EVar on) alts) | any fst alts2 = eCase (EVar on) $ map snd alts2
            where alts2 = map g alts
                  g (Patt c vs, x) = let b = any (`Map.member` getI x) vs in (b, (Patt c vs, rep [(on,eApps (eCon c) (map eVar vs)) | b] x))
                  g x = (False, x)

        f (ECase _ on alts) | (ECon c, vs) <- fromEApps on = head $ concatMap (g c vs) alts ++ [error "simplifyExpr bougus case"]
            where g c vs (PattAny,x) = [x]
                  g c vs (Patt c2 vs2, x) = [rep (zip vs2 vs) x | c == c2]
                  g c vs _ = []

        f (EApp _ (EApp _ (EVar "($)") x) y) = f $ eApp x y

        f x = x


        rep [] z = z
        rep xy z = substsWith f (Map.fromList xy) z

{-
transform f
    where
        f (ELet xy z) = eLet keep $ substs sub z
            where (sub,keep) = partition (flip linear z . fst) xy
        f (ECase (EVar x) ys) = ECase (EVar x) $ map g ys
            where g o@(Patt c vs, b) = if x `elem` vs then o else (Patt c vs, subst x (eApps (ECon c) $ map EVar vs) b)
                  g x = x
        f x = x
-}

