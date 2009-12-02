{-# LANGUAGE ViewPatterns, PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

module TagSoup.Generate.Desugar(
    records, untyped, irrefutable, core, core2
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


irrefutable :: [Decl] -> [Decl]
irrefutable = concatMap f . descend irrefutable
    where
        f (PatBind sl (PTuple [PVar (Ident x), PVar (Ident y)]) a b c) =
            [PatBind sl (pvar xy) a b c
            ,PatBind sl (pvar x) Nothing (UnGuardedRhs $ App (var "fst") (var xy)) (BDecls [])
            ,PatBind sl (pvar y) Nothing (UnGuardedRhs $ App (var "snd") (var xy)) (BDecls [])]
            where xy = x++"_"++y
        f x = [x]


core :: [Decl] -> [Decl]
core = id {- transformBi simpleCase . transformBi removeGuards . lambdaLift . transformBi flatMatches . transformBi removeWhere
    where
        -- afterwards, no where
        removeWhere (Match a b c d bod (BDecls whr)) | whr /= [] = Match a b c d (f bod) (BDecls [])
            where f (UnGuardedRhs x) = UnGuardedRhs $ Let (BDecls whr) x
                  f (GuardedRhss xs) = GuardedRhss [GuardedRhs a b $ Let (BDecls whr) c | GuardedRhs a b c <- xs]
        removeWhere x = x

        -- afterwards, no multiple equations for one function
        flatMatches (FunBind xs@(Match a b c d e f:_)) | length xs > 1 || any isPAsPat (universeBi c) =
                FunBind [Match a b (map PVar ps) Nothing (UnGuardedRhs bod) (BDecls [])]
            where
                ps = map (Ident . (++) "p_" . show) [1..length c]
                bod = Case (tuple $ map (Var . UnQual) ps) [Alt sl (ptuple p) (f b) d | Match _ _ p _ b d <- xs]
                f (UnGuardedRhs x) = UnGuardedAlt x
                f (GuardedRhss xs) = GuardedAlts $ map g xs
                g (GuardedRhs x y z) = GuardedAlt x y z
        flatMatches x = x

        removeGuards (Alt s p (GuardedAlts xs) d:rest) | length xs > 1 = removeGuards $ [Alt s p (GuardedAlts [x]) d | x <- xs] ++ rest
        removeGuards (Alt s p (GuardedAlts [GuardedAlt _ tsts e]) d:rest) =
            [Alt s (PAsPat (Ident "u_") p) (UnGuardedAlt
                (Case (tuple as) $ removeGuards
                    [Alt s (ptuple bs) (UnGuardedAlt e) d] ++ removeGuards
                    [Alt s PWildCard (UnGuardedAlt $ Case (var "u_") rest) d | rest /= []])) d]
            where (as,bs) = unzip $ map f tsts
                  f (Qualifier x) = (x, PApp (UnQual $ Ident "True") [])
                  f (Generator _ p x) = (x, p)
        removeGuards x = x


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
        pat x = map f $ transformBi dropLam $ x : filter isPatLambda (universeBi x)
            where dropLam (Let (BDecls x) y) = let ds = filter (not . isPatLambda) x in if null ds then y else Let (BDecls ds) y
                  dropLam x = x
                  f (PatBind a (PVar b) c (UnGuardedRhs (Lambda x y z)) d) = FunBind [Match a b y c (UnGuardedRhs z) d]
                  f x = x

        -- include all variables at lambdas
        push :: [String] -> Exp -> Unique Exp
        push seen o@(Let (BDecls xs) y) = descendM (push (nub $ seen++concat [pats v | PatBind _ v _ _ _ <- xs])) o
        push seen o@(Lambda a xs y) = do
            v <- fresh
            let used = [x | Var (UnQual (Ident x)) <- universe y]
                next = concatMap pats xs `intersect` used
                now = (seen \\ next) `intersect` used
            y <- push (nub $ now++next) y
            xs <- return $ map (PVar . Ident) now ++ xs
            o <- return $ Lambda a xs y
            return $ Let
                (BDecls [PatBind sl (PVar $ Ident v) Nothing (UnGuardedRhs o) (BDecls [])])
                (apps (var v) (map var now))
        push seen o = descendM (push seen) o

        -- introduce a root
        root (PatBind a b@(PVar (Ident name)) c (UnGuardedRhs d) e) = PatBind a b c (UnGuardedRhs bod) e
            where bod = Let (BDecls [PatBind sl (PVar $ Ident $ name ++ "_root_") Nothing (UnGuardedRhs d) (BDecls [])]) (var $ name ++ "_root_")

        -- introduce lambdas
        add (FunBind [Match a b c d (UnGuardedRhs e) f]) = PatBind a (PVar b) d (UnGuardedRhs $ Lambda a c e) f
        add x = x


simpleCase :: Exp -> Exp
simpleCase (Case on alts) | any (\(Alt _ p _ _) -> isPAsPat $ fromPParen p) alts =
    case fromParen on of
        Var v -> f on
        _ -> let1 "a_" on $ f $ var "a_"
    where f v = Case v $ map (g v) alts
          g v (Alt a (fromPParen -> PAsPat (Ident n) x) (UnGuardedAlt c) d) = Alt a x (UnGuardedAlt $ let1 n on c) d
          g v x = x
simpleCase x = x




isLambda Lambda{} = True; isLambda _ = False
isPatLambda (PatBind a b c (UnGuardedRhs d) e) = isLambda d; isPatLambda _ = False
isPAsPat PAsPat{} = True; isPAsPat _ = False


lets xy z = Let (BDecls [PatBind sl (PVar $ Ident x) Nothing (UnGuardedRhs y) (BDecls []) | (x,y) <- xy]) z


type Unique a = State [String] a

uniques :: String -> (Unique a) -> a
uniques s o = evalState o $ map ((++) s . show) [1..]

fresh :: Unique String
fresh = do
    s:ss <- get
    put ss
    return s


-}

{-

core3 :: [Decl] -> [Decl]
core3 = concatMap (uniques "a_" . fDecl [])


fDecl :: [String] -> Decl -> Unique [Decl]
fDecl seen (PatBind a (PVar b) c d e) = fDecl seen $ FunBind [Match a b [] c d e]
fDecl seen (FunBind xs@(Match _ nam ps _ _ _:_)) = do
    vs <- replicateM (length ps) fresh
    let f (Match _ _ ps _ bod whr) = Alt sl (ptuple ps) (g bod) whr
        g (UnGuardedRhs x) = UnGuardedAlt x
        g (GuardedRhss xs) = GuardedAlts [GuardedAlt x y z | GuardedRhs x y z <- xs]
    bod <- fExp vs $ Case (tuple $ map var vs) $ map f xs
    return [FunBind [Match sl nam (map pvar vs) Nothing (UnGuardedRhs bod) (BDecls [])]]
fDecl seen x = return [x]


fExp :: [String] -> Exp -> Unique Exp

-- case
-- if the constructor matches the first pattern, take it
fExp seen (Case on (Alt _ pat (UnGuardedAlt x) decs:rest)) | Just bind <- patMatch pat on = fExp seen $ lets bind x
fExp seen x = return x



patMatch :: Pat -> Exp -> Maybe [(String, Exp)]
patMatch (PTuple xs) (Tuple ys) = do guard (length xs == length ys) ; concatZipWithM patMatch xs ys
patMatch (PVar (Ident x)) y@(Var _) = Just [(x,y)]
patMatch _ _ = Nothing


patMatch1 :: Pat -> Exp -> Maybe (String,Exp)
patMatch1 = undefined


concatZipWithM f xs ys = fmap concat $ sequence $ zipWith f xs ys

-}











core2 :: [Decl] -> [Decl]
core2 = {- concatMap g . -} map f . transformBi qname . transformBi name
    where
        -- transform
        f (PatBind a (PVar name) b c d) =
            PatBind a (PVar name) Nothing (UnGuardedRhs $ run $ fExp bod) (BDecls [])
            where bod = Let (BDecls [PatBind a (pvar "root") b c d]) (var "root")
        f (FunBind ms@(Match a name b c d e : _)) =
            PatBind a (PVar name) Nothing (UnGuardedRhs $ run $ fExp bod) (BDecls [])
            where bod = Let (BDecls [FunBind [Match a (Ident "root") b c d e | Match a _ b c d e <- ms]]) (var "root")
        f x = x

        -- lambda lift
        g (PatBind a (PVar (Ident name)) b (UnGuardedRhs x) c) =
                [PatBind a (PVar (Ident name)) b (UnGuardedRhs $ transformBi dropPatLambda bod) c] ++
                map (transformBi dropPatLambda) (filter isPatLambda (universeBi bod))
            where
                names = [n | PatBind _ (PVar (Ident n)) _ (UnGuardedRhs Lambda{}) _ <- universeBi x]
                bod = f names $ transformBi ren x
                    where f [] x = x
                          f (n:ns) x = let1Simplify n (var $ name++"_"++n) x

                ren (Let (BDecls [PatBind a (PVar (Ident n)) b c@(UnGuardedRhs Lambda{}) d]) bod) =
                    Let (BDecls [PatBind a (PVar $ Ident n2) b c d]) (let1Simplify n (var n2) bod)
                    where n2 = name++"_"++n
                ren x = x

                dropPatLambda (Let (BDecls xs) y) = if null xs2 then y else Let (BDecls xs2) y
                    where xs2 = filter (not . isPatLambda) xs
                dropPatLambda x = x

        g x = [x]

        -- pre simplify
        qname (Qual _ x) = UnQual x
        qname (Special x) = UnQual $ Ident $ "("++prettyPrint x++")"
        qname x = x
        name (Symbol x) = Ident $ "("++x++")"
        name x = x



fExp (Let (BDecls whr) x) = fDecls whr >> fExp x
fExp x@(Var _) = ret x
fExp x@(Con _) = ret x
fExp x@(Lit _) = ret x
fExp (Tuple xs) = fExp $ apps (var $ "(" ++ replicate (length xs - 1) ',' ++ ")") xs
fExp (Paren x) = fExp x
fExp (InfixApp x y z) = fExp $ App (App (opExp y) x) z
fExp (LeftSection x op) = fExp $ App (opExp op) x
fExp (RightSection op x) = fExp $ App (App (var "flip") (opExp op)) x
fExp (App x y) = app (fExp x) (fExp y)
fExp (Case on alts) = do
    v <- share $ fExp on
    branch $ map (fAlt v) alts
fExp (If x y z) = branch [match (fExp x) ("True",0) >> fExp y, fExp z]
fExp (List []) = ret $ con "[]"
fExp (List (x:xs)) = fExp $ App (App (var "(:)") x) (List xs)
fExp (Lambda _ ps x) = do
    vs <- lam (length ps)
    fPats vs ps
    fExp x
fExp x = err ("fExp",x)



fDecls = binds . map fDecl

fDecl (PatBind _ (PVar (Ident name)) _ (UnGuardedRhs bod) (BDecls whr)) = (name, fDecls whr >> fExp bod)
fDecl (FunBind xs@(Match _ (Ident name) ps _ _ _ : _)) = (name, do vs <- lam $ length ps ; branch $ map (fMatch vs) xs)
fDecl x = err ("fDecl",x)

fMatch vs (Match _ _ ps _ rhs (BDecls whr)) = fPats vs ps >> fDecls whr >> fRhs rhs

fPats vs xs | length vs == length xs = sequence_ $ zipWith fPat vs xs

fPat v (PVar (Ident x)) = bind x (ret $ var v)
fPat v (PParen x) = fPat v x
fPat v (PAsPat (Ident x) y) = bind x (ret $ var v) >> fPat v y
fPat v (PLit x) = matchLit (ret $ var v) x
fPat v (PWildCard) = return ()
fPat v (PTuple xs) = fPat v $ PApp (UnQual $ Ident $ "(" ++ replicate (length xs - 1) ',' ++ ")") xs
fPat v (PList []) = fPat v $ PApp (UnQual $ Ident "[]") []
fPat v (PList (x:xs)) = fPat v $ PApp (UnQual $ Ident "(:)") [x, PList xs]
fPat v (PInfixApp x y z) = fPat v $ PApp y [x,z]
fPat v (PApp (UnQual (Ident x)) xs) = do
    vs <- match (ret $ var v) (x,length xs) 
    fPats vs xs
fPat v x = err ("fPat",v,x)


fRhs (UnGuardedRhs x) = fExp x
fRhs (GuardedRhss xs) = branch $ map fGRhs xs

fRhs x = err ("fRhs",x)

fAlt v (Alt _ p bod (BDecls whr)) = fPat v p >> fDecls whr >> fGAlts bod

fGAlts (UnGuardedAlt x) = fExp x
fGAlts (GuardedAlts xs) = branch $ map fGAlt xs

fGAlt (GuardedAlt _ xs x) = mapM_ fStmt xs >> fExp x
fGRhs (GuardedRhs _ xs x) = mapM_ fStmt xs >> fExp x


fStmt (Qualifier x) = match (fExp x) ("True",0) >> return ()
fStmt (Generator _ p x) = do
    v <- share $ fExp x
    fPat v p
fStmt x = err ("fStmt",x)




share :: M Exp -> M String
share x = do
    x <- go x
    case fromParen x of
        Var (UnQual (Ident v)) -> return v
        _ -> do
            v <- fresh
            bind v (ret x)
            return v

bind :: String -> M Exp -> M ()
bind name x = do
    x <- go x
    addSeen [name]
    addExp $ let1Simplify name x

binds :: [(String, M Exp)] -> M ()
binds xs = do
    let (vs,bs) = unzip xs
    bs <- mapM go bs
    addSeen vs
    addExp $ lets $ zip vs bs

branch :: [M Exp] -> M Exp
branch [x] = x
branch (x:xs) = bind "fail_" (branch xs) >> x

match :: M Exp -> (String,Int) -> M [String]
match x (s,n) = do
    x <- go x
    vs <- replicateM n fresh
    addExp $ \bod -> Case x [alt (PApp (UnQual $ Ident s) (map pvar vs)) bod, alt PWildCard (var "fail_")]
    addSeen vs
    return vs

matchLit :: M Exp -> Literal -> M ()
matchLit x lit = do
    x <- go x
    addExp $ \bod -> Case x [alt (PLit lit) bod, alt PWildCard (var "fail_")]

lam :: Int -> M [String]
lam n = do
    v <- fresh
    vs <- replicateM n fresh
    seen <- getSeen
    addExp $ \bod -> lams vs bod -- let1 v (lams (seen++vs) bod) (apps (var v) $ map var seen)
    addSeen vs
    return vs


app :: M Exp -> M Exp -> M Exp
app x y = do
    x <- go x
    y <- go y
    ret $ App x y




type M a = State (Exp -> Exp, [String], Int) a

run :: M Exp -> Exp
run x = evalState (bind "fail_" (ret $ var "patternMatchFail") >> x) (id, [], 1)


go :: M Exp -> M Exp
go x = do
    (k,s,i) <- get
    put (id,s,i)
    res <- x
    (k2,s2,i2) <- get
    put (k,s,i2)
    return res


addExp :: (Exp -> Exp) -> M ()
addExp x = modify $ \(k,s,i) -> (k . paren . x, s, i)

addSeen :: [String] -> M ()
addSeen x = modify $ \(k,s,i) -> (k, nub $ s ++ x, i)

getSeen :: M [String]
getSeen = do (k,s,i) <- get; return s

fresh :: M String
fresh = do
    (k,s,i) <- get
    put (k,s,i+1)
    return $ "v_" ++ show i

ret :: Exp -> M Exp
ret x = do
    (k,s,i) <- get
    return $ k $ paren x

err x = error $ show x




let1 x y z = Let (BDecls [PatBind sl (pvar x) Nothing (UnGuardedRhs y) (BDecls [])]) z

lets xy z = Let (BDecls [PatBind sl (pvar x) Nothing (UnGuardedRhs y) (BDecls []) | (x,y) <- xy]) z

let1Simplify name (fromParen -> Var (UnQual (Ident bind))) z = f z
    where
        f (App x y) = App (f x) (f y)
        f (Paren x) = Paren $ f x
        f (Var (UnQual (Ident x))) = if x == name then var bind else var x
        f (Lit x) = Lit x
        f (Con x) = Con x
        f (Case on alts) = Case (f on) $ map g alts
        f (Let (BDecls bind) x) = if name `elem` map h bind then Let (BDecls bind) x else
            Let (BDecls [PatBind a b c (UnGuardedRhs $ f d) e | PatBind a b c (UnGuardedRhs d) e <- bind]) (f x)
        f (Lambda a ps x) = Lambda a ps $ if name `elem` concatMap pats ps then x else f x
        f x = err ("let1Simplify", x)

        g a@(Alt sl pat (UnGuardedAlt bod) (BDecls [])) = if name `elem` pats pat then a else Alt sl pat (UnGuardedAlt $ f bod) (BDecls [])
        h (PatBind _ (PVar (Ident x)) _ (UnGuardedRhs _) (BDecls [])) = x

let1Simplify x y z = let1 x y z


lams xs y = Lambda sl (map pvar xs) y

alt p x = Alt sl p (UnGuardedAlt x) (BDecls [])

opExp (QConOp x) = Con x
opExp (QVarOp x) = Var x


paren (Paren x) = Paren x
paren (Var x) = Var x
paren x = Paren x

pats = concatMap f . universe
    where f (PVar (Ident v)) = [v]
          f (PAsPat (Ident v) _) = [v]
          f _ = []

isLambda Lambda{} = True; isLambda _ = False
isPatLambda (PatBind a b c (UnGuardedRhs d) e) = isLambda d; isPatLambda _ = False
