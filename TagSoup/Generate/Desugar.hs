
module TagSoup.Generate.Desugar(
    records, untyped, core
    ) where

import TagSoup.Generate.HSE
import Data.Generics.PlateData
import Data.Maybe
import Data.List hiding (find)
import Control.Monad.State


find x xs = fromMaybe (error $ "Couldn't find: " ++ show x) $ lookup x xs


records :: [Decl] -> [Decl]
records xs = transformBi fPat $ transformBi fExp xs
    where
        recs = [(x, concatMap fst ys) | RecDecl x ys <- universeBi xs]
        typs = [(x, replicate (length ys) (Ident "")) | ConDecl x ys <- universeBi xs]

        fExp o@(RecConstr (UnQual name) xs) = Paren $ apps (Con $ UnQual name) [Paren $ find l lbls | l <- find name recs]
            where lbls = [(n,x) | FieldUpdate (UnQual n) x <- reverse xs]
        fExp x = x

        fPat (PRec (UnQual name) [PFieldWildcard]) = PParen $ PApp (UnQual name) $ map PVar $ find name recs
        fPat (PRec (UnQual name) []) = PParen $ PApp (UnQual name) $ replicate (length $ find name $ recs++typs) PWildCard
        fPat x = x


untyped :: [Decl] -> [Decl]
untyped = map (descendBi untyped) . filter (not . isTypeSig)
    where isTypeSig TypeSig{} = True
          isTypeSig _ = False


core :: [Decl] -> [Decl]
core = lambdaLift . transformBi flatMatches . transformBi removeWhere
    where
        -- afterwards, no where
        removeWhere (Match a b c d bod (BDecls whr)) | whr /= [] = Match a b c d (f bod) (BDecls [])
            where f (UnGuardedRhs x) = UnGuardedRhs $ Let (BDecls whr) x
                  f (GuardedRhss xs) = GuardedRhss [GuardedRhs a b $ Let (BDecls whr) c | GuardedRhs a b c <- xs]
        removeWhere x = x

        -- afterwards, no multiple equations for one function
        flatMatches (FunBind xs@(Match a b c d e f:_)) | length xs > 1 =
                FunBind [Match a b (map PVar ps) Nothing (UnGuardedRhs bod) (BDecls [])]
            where
                ps = map (Ident . (++) "p_" . show) [1..length c]
                bod = Case (Tuple $ map (Var . UnQual) ps) [Alt sl (PTuple p) (f b) d | Match _ _ p _ b d <- xs]
                f (UnGuardedRhs x) = UnGuardedAlt x
                f (GuardedRhss xs) = GuardedAlts $ map g xs
                g (GuardedRhs x y z) = GuardedAlt x y z
        flatMatches x = x


-- afterwards no PatBind or Lambda, only FunBind
lambdaLift :: [Decl] -> [Decl]
lambdaLift = concatMap f . transformBi add
    where
        f (PatBind a (PVar b) c (UnGuardedRhs (Lambda x y z)) d) | not $ any isLambda $ universe z =
            [FunBind [Match a b y c (UnGuardedRhs z) d]]
        f (PatBind a (PVar b) c (UnGuardedRhs z) d) | not $ any isLambda $ universe z =
            [FunBind [Match a b [] c (UnGuardedRhs z) d]]
        f x@(PatBind _ (PVar (Ident name)) _ _ _) = pat $ uniques (name++"_") $ descendBiM (push []) $ root x
        f x = [x]

        -- convert a PatBind in to several FunBind's
        pat x = transformBi dropLam $ x : filter isPatLambda (universeBi x)
            where dropLam (Let (BDecls x) y) = Let (BDecls $ filter (not . isPatLambda) x) y
                  dropLam x = x

        -- include all variables at lambdas
        push :: [String] -> Exp -> Unique Exp
        push seen o@(Let (BDecls xs) y) = descendM (push (nub $ seen++concat [pats v | PatBind _ v _ _ _ <- xs])) o
        push seen o@(Lambda a xs y) = do
            v <- fresh
            let next = concatMap pats xs
                now = seen \\ next
            y <- push (nub $ now++next) y
            xs <- return $ map (PVar . Ident) now ++ xs
            o <- return $ Lambda a xs y
            return $ Let
                (BDecls [PatBind sl (PVar $ Ident v) Nothing (UnGuardedRhs o) (BDecls [])])
                (apps (var v) (map var now))
        push seen o = descendM (push seen) o

        -- introduce a root
        root (PatBind a b@(PVar (Ident name)) c (UnGuardedRhs d) e) = PatBind a b c (UnGuardedRhs bod) e
            where bod = Let (BDecls [PatBind sl (PVar $ Ident $ name ++ "_root_") Nothing (UnGuardedRhs $ transformBi f d) (BDecls [])]) (var $ name ++ "_root_")
                  -- f (PatBind a (PVar (Ident n)) c d e) = PatBind a (PVar $ Ident $ name ++ "_" ++ n) c d e
                  f x = x::Int

        -- introduce lambdas
        add (FunBind [Match a b c d (UnGuardedRhs e) f]) = PatBind a (PVar b) d (UnGuardedRhs $ Lambda a c e) f
        add x = x


isLambda Lambda{} = True; isLambda _ = False
isPatLambda (PatBind a b c (UnGuardedRhs d) e) = isLambda d; isPatLambda _ = False


pats = concatMap f . universe
    where f (PVar (Ident v)) = [v]
          f (PAsPat (Ident v) _) = [v]
          f _ = []

{-

| PatBind SrcLoc Pat (Maybe Type) Rhs Binds

Match SrcLoc Name [Pat] (Maybe Type) Rhs Binds  
-}

type Unique a = State [String] a

uniques :: String -> (Unique a) -> a
uniques s o = evalState o $ map ((++) s . show) [1..]

fresh :: Unique String
fresh = do
    s:ss <- get
    put ss
    return s
