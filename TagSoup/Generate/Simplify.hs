{-# LANGUAGE PatternGuards #-}

module TagSoup.Generate.Simplify(reduceArity, simplifyProg, simplifyExpr) where

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



reduceArity :: Prog -> Prog
reduceArity prog = deleteArgs (Map.keys $ reduce mp) prog
    where
        mn = minArity prog
        mp = Map.fromList [((a,i), nub v) | (a,b) <- Map.toList mn, i <- [0..b-1], let fun = getFunc prog a,
                           i < length (funcArgs fun), Just v <- [callers (funcArgs fun !! i) (funcBody fun)]]

        reduce x = if Map.size x == Map.size x2 then x else reduce x2
            where x2 = Map.filter (\xs -> all (`Map.member` x) xs) x


deleteArgs :: [(Var,Int)] -> Prog -> Prog
deleteArgs xs = map f
    where
        mp = Map.unionsWith (++) [Map.singleton a [b] | (a,b) <- xs]

        f (Func name args bod) = Func name (del name args) $ g bod

        g x | (EVar y, z) <- fromEApps x = eApps (EVar y) (del y $ map (descend g) z)
        g x = descend g x

        del name xs = [x | (i,x) <- zip [0..] xs, i `notElem` ys]
            where ys = Map.findWithDefault [] name mp


-- what is the minimum calling arity of each function
minArity :: Prog -> Map.Map Var Int
minArity prog = Map.filterWithKey (\k v -> v /= 0 && k `elem` map funcName prog) $ Map.unionsWith min $ map f $ childrenBi prog
    where
        f x = Map.unionsWith min $ case fromEApps x of
            (EVar a, b) -> Map.singleton a (length b) : map f (concatMap children b)
            _ -> map f $ children x


-- which functions get given this argument, Nothing if it's used directly
callers :: Var -> Expr -> Maybe [(Var,Int)]
callers v x | (EVar y, z) <- fromEApps x = comb $
    (if v == y then Nothing else Just []) :
    (Just $ map ((,) y) $ findIndices (== EVar v) z) :
    map (callers v) (concatMap children z)

callers v (ELet _ xy z) = comb $
    (if v `elem` map fst xy then Just [] else callers v z) :
    (map (callers v . snd) xy)

callers v (ECase _ on alts) = comb $
    (callers v on) :
    [if v `elem` pattVars a then Just [] else callers v b | (a,b) <- alts]

callers v _ = Just []

comb [] = Just []
comb (x:xs) = both x $ comb xs

both (Just a) (Just b) = Just $ a ++ b
both _ _ = Nothing
