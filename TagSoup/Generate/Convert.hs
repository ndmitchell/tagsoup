
module TagSoup.Generate.Convert(input,output,showProg,showFunc,showExpr) where

import TagSoup.Generate.Type
import TagSoup.Generate.HSE
import Data.List


showProg = unlines . map showFunc
showFunc = prettyPrint . outFunc
showExpr = prettyPrint . outExpr


input :: [Decl] -> [Func]
input xs = map (inFunc names) xs
    where names = [name | PatBind _ (PVar (Ident name)) _ _ _ <- xs]

inFunc names (PatBind _ (PVar (Ident name)) Nothing (UnGuardedRhs bod) (BDecls [])) =
    case bod of
        Lambda _ ps x -> Func name (map fromPVar ps) (inExpr names x)
        _ -> Func name [] (inExpr names bod)
inFunc names x = error $ show x

inExpr names (Paren x) = inExpr names x
inExpr names (Var (UnQual (Ident x))) = if x `elem` names then eFun x else eVar x
inExpr names (Con (UnQual (Ident x))) = eCon x
inExpr names (App x y) = eApp (inExpr names x) (inExpr names y)
inExpr names (Let (BDecls xs) y) = unrecLet [(x,inExpr names y) | PatBind _ (PVar (Ident x)) _ (UnGuardedRhs y) (BDecls []) <- xs] $ inExpr names y
inExpr names (Case x ys) = eCase (inExpr names x) [(inPatt p, inExpr names x) | Alt _ p (UnGuardedAlt x) _ <- ys]
inExpr names (Lit x) = eLit x
inExpr names x = error $ show ("inExpr",x)


unrecLet :: [(String, Expr)] -> Expr -> Expr
unrecLet xy z | null xy = z
              | null now = error $ "Recursive let: " ++ prettyPrint (outExpr $ eLet xy z)
              | otherwise = eLet now $ unrecLet next z
    where (now,next) = partition (disjoint xs . freeVars . snd) xy
          xs = map fst xy


inPatt (PApp (UnQual (Ident c)) vs) = Patt c (map fromPVar vs)
inPatt PWildCard = PattAny
inPatt (PParen x) = inPatt x
inPatt (PLit x) = PattLit x
inPatt x = error $ show ("inPatt",x)


output :: [Func] -> [Decl]
output = map outFunc

outFunc (Func x y z) = PatBind sl (pvar x) Nothing (UnGuardedRhs $ (if null y then id else Lambda sl (map pvar y)) (outExpr z)) (BDecls [])

outExpr (ECon x) = con x
outExpr (ELit x) = Lit x
outExpr (EVar x) = var x
outExpr (EFun x) = var x
outExpr (EApp _ x y) = Paren $ App (outExpr x) (outExpr y)
outExpr (ECase _ on alts) = Paren $ Case (outExpr on) [Alt sl (outPatt p) (UnGuardedAlt $ outExpr x) (BDecls []) | (p,x) <- alts]
outExpr (ELet _ xs y) = Paren $ Let (BDecls [PatBind sl (pvar a) Nothing (UnGuardedRhs $ outExpr b) (BDecls []) | (a,b) <- xs]) (outExpr y)
outExpr (ELam _ v x) = Lambda sl [pvar v] $ outExpr x

outPatt (Patt c vs) = PApp (UnQual $ Ident c) (map pvar vs)
outPatt PattAny = PWildCard
outPatt (PattLit x) = PLit x

{-

data Func = Func Var [Var] Expr

data Expr = EApp Expr Expr
          | ELet [(Var,Expr)] Expr
          | ECase Var [((Con, [Var]),Expr)]
          | EVar Var
          | ECon Con
-}


fromPVar (PVar (Ident x)) = x



