
module Compiler.Resolve(resolve) where

import Compiler.Lp
import Compiler.Util


primMatch_ = ["eof"]
primRule_ = ["endBy","takeWhileNot"]

data Names = Names {primMatch :: [String]
                   ,primRule :: [String]
                   ,realRule :: [String]
                   }
                   deriving Show


resolve :: Program -> Program
resolve xs = explicitRes $ map (resolveRule names) real
    where
        names = Names (primMatch_ ++ f "primMatch") (primRule_ ++ f "primRule") (map ruleName real)
        (prim,real) = partition (isPrefixOf "prim" . ruleName) xs
        f x = maybe [] (map expName . childrenBi) $ find ((==) x . ruleName) prim


resolveRule :: Names -> Rule -> Rule
resolveRule names r@(Rule name args _) = transformBi fExp $ transformBi fPat r
    where
        args2 = args ++ [x | Just x <- childrenBi r]

        fExp (Prim x xs) = f x $ [(primRule names,Prim x xs),(realRule names,Call x xs)] ++
                                 [(args2,Var x) | null xs]
        fExp x = x
        
        fPat (PPrim x) = f x [(args2,PVar x),(primMatch names,PPrim x)]
        fPat x = x

        f x xs = case find (elem x . fst) xs of
            Nothing -> error $ "Can't resolve name " ++ x ++ " to one of: " ++ unwords (concatMap fst xs)
            Just y -> snd y


explicitRes :: Program -> Program
explicitRes = transformBi f
    where
        f (Seq xs act) | all (isNothing . bindVar) xs &&
                         length (filter (isCall . bindBody) xs) == 1
                       = Seq  (map g xs) act
        f x = x

        g (Bind Nothing x) | isCall x = Bind (Just "res") x
        g x = x

                         