{-# LANGUAGE DeriveDataTypeable #-}

module Compiler.Type2 where

import Data.Data
import Data.List


prims = ["takeWhileNot","endBy"]
libs = ["spaces","name"]


type RuleName = String
type RuleArg = String

type Program = [Rule]

data Rule = Rule RuleName [RuleArg] Choice

data Choice = Choice [(Bind Pat,Seq)]
            | NoChoice Seq

type Action = String

data Seq = Seq [Bind Exp] Action

data Exp = Prim String [Val]
         | Call RuleName [Val]

data Val = Var RuleArg
         | Lit String

data Pat = PVar RuleArg
         | PLit String
         | PPrim String
         | PWildcard

data Bind a = Bind (Maybe String) a



showProgram :: Program -> String
showProgram = unlines . concatMap showRule


showRule (Rule name args x) = unwords (name:args++["="])
    where a:b = showChoice x

{- unlines . map showStmt2

showStmt2 (Stmt2 x y) = x ++ " = " ++ showExp2 y

showExp2 (Seq2 x ys) = "{" ++ x ++ "} " ++ unwords (map showItem2 ys)
showExp2 (Choice2 xs y) = concat $ intersperse " | " $
    [show a ++ " -> " ++ b | (a,b) <- xs] ++ ["_ -> " ++ y]

showItem2 (Rule2 name pos) = name ++ showBind pos
showItem2 (Prim2 name x pos) = name ++ "(" ++ show x ++ ")" ++ showBind pos
showItem2 (Literal2 x) = show x

showBind Nothing = ""
showBind (Just i) = '$' : show i


getBind (Rule2 _ x) = x
getBind (Prim2 _ _ x) = x
getBind _ = Nothing
-}
