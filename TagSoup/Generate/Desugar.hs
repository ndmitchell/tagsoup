
module TagSoup.Generate.Desugar(desugarRecords) where

import TagSoup.Generate.HSE
import Data.Generics.PlateData
import Data.Maybe


find x xs = fromMaybe (error $ "Couldn't find: " ++ show x) $ lookup x xs


desugarRecords :: [Decl] -> [Decl]
desugarRecords xs = transformBi fPat $ transformBi fExp xs
    where
        recs = [(x, concatMap fst ys) | RecDecl x ys <- universeBi xs]
        typs = [(x, replicate (length ys) (Ident "")) | ConDecl x ys <- universeBi xs]

        fExp o@(RecConstr (UnQual name) xs) = Paren $ apps (Con $ UnQual name) [Paren $ find l lbls | l <- find name recs]
            where lbls = [(n,x) | FieldUpdate (UnQual n) x <- reverse xs]
        fExp x = x

        fPat (PRec (UnQual name) [PFieldWildcard]) = PParen $ PApp (UnQual name) $ map PVar $ find name recs
        fPat (PRec (UnQual name) []) = PParen $ PApp (UnQual name) $ replicate (length $ find name $ recs++typs) PWildCard
        fPat x = x
