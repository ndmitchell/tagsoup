
module Compiler.Simplify where

import Data.Generics.PlateData
import Compiler.Type
import Data.List
import Data.Maybe

simplify :: Program1 -> Program2
simplify = addDollar . reform . freezeRules


-- change rule application rule(arg) to rule_arg and replace all bits
freezeRules :: Program1 -> Program1
freezeRules xs = map useDyn $ staticRules ++ dynInvoke
    where
        (staticRules,dynRules) = partition (\(Stmt1 a b c) -> null b) xs

        dynNames = [(a, (b,c)) | Stmt1 a b c <- dynRules]
        dynInvoke = [gen name arg res
                    |Call1 name (Just arg) bind <- nub $ universeBi staticRules
                    ,Just res <- [lookup name dynNames]]

        gen name arg (var,bod) = Stmt1 (getName name arg) "" (transformBi f bod)
            where f (Call1 v Nothing Nothing) | v == var = arg
                  f x = x

        useDyn = transformBi f
            where f (Call1 v (Just x) y) | v `elem` map fst dynNames = Call1 (getName v x) Nothing y
                  f x = x

        getName name (Literal1 x) = name ++ "_" ++ fromMaybe x (lookup x reps)
            where reps = [("'","squot"),("\"","dquot")]


reform :: Program1 -> Program2
reform = concatMap reformStmt

reformStmt (Stmt1 a "" (Choice1 xs))
    | fst (last xs) == Call1 "_" Nothing Nothing
    = Stmt2 a (Choice2 (zipWith f [1..] $ init xs) (g $ length xs)) :
      zipWith (\i (_,v) -> Stmt2 (g i) (reformSeq v)) [1..] xs
    where f i (Literal1 x,_) = (x, g i)
          g i = a ++ "_" ++ show i
reformStmt (Stmt1 a "" x) = [Stmt2 a $ reformSeq x]

reformSeq (Seq1 act xs) = Seq2 act (map reformItem xs)

reformItem (Literal1 x) = Literal2 x
reformItem (Call1 name (Just (Literal1 x)) y) | name `elem` prims = Prim2 name x y
reformItem (Call1 name Nothing y) = Rule2 name y


addDollar :: Program2 -> Program2
addDollar = transformBi f
    where
        f (Seq2 act xs) | all (isNothing . getBind) xs &&
                          length (filter (isJust . getBind) ys) == 1
                        = Seq2 act ys
            where ys = transformBi (const $ Just (1::Int)) xs
        f x = x
