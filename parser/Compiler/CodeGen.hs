
module Compiler.CodeGen where

import qualified Compiler.Lp as Lp
import Compiler.Lp(Rule(..),Choice(..),Seq(..),Bind(..),Pat(..))
import Compiler.Hs
import Compiler.Util


codeGen :: Lp.Program -> Program
codeGen = map genRule


genRule :: Rule -> Func
genRule (Rule name args bod) = Func name (args++["s0"]) (genChoice bod)

s i = Var $ "s" ++ show i
pair x y = App (Con "(,)") [x,y]

eof = Bind Nothing (PPrim "eof")

genChoice :: Choice -> Exp
genChoice (NoChoice x) = genSeq x

genChoice (Choice xs) = Let [(Var "def",def)] $ caseChoice std
    where (def,std) = getDefault $ map (second genSeq) xs


caseOn = App (Var "un") [s 0]

caseChoice :: [(Bind Pat,Exp)] -> Exp
caseChoice xs | isJust e = Case on $ (Con "Nothing",fromJust e) : alts
    where e = lookup eof xs
          Case on alts = caseChoice $ filter ((/=) eof . fst) xs
caseChoice [] = Case caseOn [(Wildcard,Var "def")]
caseChoice xs = strChoice $ map (first (fromPLit . bindBody)) xs

strChoice :: [(String,Exp)] -> Exp
strChoice [("",x)] = x
strChoice xs = Case caseOn $ map f holes ++ [(Wildcard, if null dead then Var "def" else snd $ head dead)]
    where
        (dead,alive) = partition (null . fst) xs
        holes = groupBy (equating $ head . fst) $ sortBy (comparing $ head . fst) alive
        f xs = (App (Con "Just") [pair (Con $ show $ head $ fst $ head xs) (s 0)]
               ,strChoice $ map (first tail) xs)


getDefault :: [(Bind Pat,Exp)] -> (Exp,[(Bind Pat,Exp)])
getDefault xs | length def /= 1 = error "No default case found"
              | otherwise = (snd $ head def, rest)
    where (def,rest) = partition ((Bind Nothing PWildcard ==) . fst) xs


genSeq :: Seq -> Exp
genSeq (Seq xs act) = Let (zipWith genBindExp [1..] xs) $
    pair (s $ length xs) (Raw act)


genBindExp :: Int -> Bind Lp.Exp -> (Exp,Exp)
genBindExp i (Bind x y) = (pair (s i) (maybe Wildcard Var x), genExp (i-1) y)


genExp :: Int -> Lp.Exp -> Exp
genExp i (Lp.Var x) = Var x
genExp i (Lp.Lit x) = App (Var "lit") [Con (show x)]
genExp i (Lp.Call name args) = App (Var name) $ map (genExp i) args ++ [s i]
genExp i (Lp.Prim x xs) = genExp i (Lp.Call x xs)
