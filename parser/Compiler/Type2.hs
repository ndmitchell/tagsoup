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



-- $ = variable
-- @ = rule
-- ! = primitive

instance Show Rule where
    show (Rule name args x) = unwords $ name : map ('$':) args ++["=",show x]
    showList = showString . unlines . map show

instance Show Choice where
    show (NoChoice x) = show x
    show (Choice xs) = concat ["\n  " ++ show a ++ " = " ++ show b | (a,b) <- xs]

instance Show Seq where
    show (Seq xs y) = unwords $ map show xs ++ ["{" ++ y ++ "}"]

instance Show Exp where
    show (Prim x ys) = show $ Call ('#':x) ys
    show (Call x []) = x
    show (Call x ys) = "(" ++ unwords (x : map show ys) ++ ")"

instance Show Val where
    show (Var x) = '$':x
    show (Lit x) = show x

instance Show Pat where
    show (PVar x) = '$':x
    show (PPrim x) = '#':x
    show (PLit x) = show x
    show PWildcard = "_"

instance Show a => Show (Bind a) where
    show (Bind (Just a) b) = a ++ "@" ++ show b
    show (Bind _ b) = show b
