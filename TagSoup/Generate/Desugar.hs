
module TagSoup.Generate.Desugar(
    records, untyped, core
    ) where

import TagSoup.Generate.HSE
import Data.Generics.PlateData
import Data.Maybe


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
core = transformBi flatMatches . transformBi removeWhere
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
