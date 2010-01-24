{-# LANGUAGE PatternGuards #-}

module Simplify(reduceArity, simplifyProg, simplifyExpr, forwarding) where

import Type
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
simplifyProg = descendBi (unique . simplifyExpr)


simplifyExpr :: Expr -> Unique Expr
simplifyExpr = transformM f
    where
        f (ELet _ xy z) | not $ null sub = f . eLet keep =<< rep sub z
            where (sub,keep) = partition (\(x,y) -> linear x z || simple y) xy

        -- only safe because variables are unique
        f (ECase _ (ELet _ xy z) alts) = f . eLet xy =<< f (eCase z alts)

        f (ECase _ on alts) | (PattAny,ECase _ on2 alts2) <- last alts, on == on2 = f $ eCase on (init alts ++ alts2)

        f (ECase _ (EVar on) alts) | any fst alts2 = fmap (eCase (EVar on)) $ sequence $ map snd alts2
            where alts2 = map g alts
                  g (Patt c vs, x) = let b = any (`Map.member` getI x) vs in (b, fmap ((,) (Patt c vs)) $ rep [(on,eApps (eCon c) (map eVar vs)) | b] x)
                  g x = (False, return x)

        f (ECase _ on alts) | (ECon c, vs) <- fromEApps on = head $ concatMap (g c vs) alts ++ [error "simplifyExpr bougus case"]
            where g c vs (PattAny,x) = [return x]
                  g c vs (Patt c2 vs2, x) = [rep (zip vs2 vs) x | c == c2]
                  g c vs _ = []

        f (ECase _ on [(Patt "(:)" vs,x), (PattAny,y)]) = f $ eCase on [(Patt "(:)" vs,x), (Patt "[]" [],y)]

        f x = return x


        rep [] z = return z
        rep xy z = substsWith f (Map.fromList xy) z


simple EVar{} = True
simple EFun{} = True
simple ECon{} = True
simple _ = False


reduceArity :: Prog -> Prog
reduceArity prog = deleteArgs (Map.keys $ reduce mp) prog
    where
        mn = minArity prog
        mp = Map.fromList [((a,i), nub v) | (a,b) <- Map.toList mn, i <- [0..b-1], let fun = getFunc prog a,
                           i < length (funcArgs fun), Just v <- [callers (funcArgs fun !! i) (funcBody fun)]]

        reduce x = if Map.size x == Map.size x2 then x else reduce x2
            where x2 = Map.filter (\xs -> all (`Map.member` x) xs) x


deleteArgs :: [(Fun,Int)] -> Prog -> Prog
deleteArgs xs = map f
    where
        mp = Map.unionsWith (++) [Map.singleton a [b] | (a,b) <- xs]

        f (Func name args bod) = Func name (del name args) $ g bod

        g x | (EFun y, z) <- fromEApps x = eApps (EFun y) (del y $ map (descend g) z)
        g x = descend g x

        del name xs = [x | (i,x) <- zip [0..] xs, i `notElem` ys]
            where ys = Map.findWithDefault [] name mp


-- what is the minimum calling arity of each function
minArity :: Prog -> Map.Map Fun Int
minArity prog = Map.filterWithKey (\k v -> v /= 0 && k `elem` map funcName prog) $ Map.unionsWith min $ map f $ childrenBi prog
    where
        f x = Map.unionsWith min $ case fromEApps x of
            (EFun a, b) -> Map.singleton a (length b) : map f (concatMap children b)
            _ -> map f $ children x


-- which functions get given this argument, Nothing if it's used directly
callers :: Var -> Expr -> Maybe [(Fun,Int)]
callers v x | (EFun y, z) <- fromEApps x = comb $
    (Just $ map ((,) y) $ findIndices (== EVar v) z) :
    map (callers v) (concatMap children z)

callers v (EVar y) = if v == y then Nothing else Just []

callers v (ELet _ xy z) = comb $
    (if v `elem` map fst xy then Just [] else callers v z) :
    (map (callers v . snd) xy)

callers v (ECase _ on alts) = comb $
    (callers v on) :
    [if v `elem` pattVars a then Just [] else callers v b | (a,b) <- alts]

callers v x = comb $ map (callers v) $ children x

comb [] = Just []
comb (x:xs) = both x $ comb xs

both (Just a) (Just b) = Just $ a ++ b
both _ _ = Nothing



-- if a is just a synonym for b, replace every occurence of a with b
-- doesn't work if multiple functions are just _|_
forwarding  :: Prog -> Prog
forwarding prog = filter (not . flip Map.member fwd . funcName) $ transformBi f prog
    where
        f (EFun x) = case Map.lookup x fwd of
                         Nothing -> EFun x
                         Just y -> f $ EFun y
        f x = x

        fwd = Map.fromList $ concatMap g prog
        g (Func name args bod) | (EFun x,y) <- fromEApps bod, map eVar args == y, name /= "main" = [(name,x)]
        g _ = []
