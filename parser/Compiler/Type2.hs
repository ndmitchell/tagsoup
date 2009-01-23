{-# LANGUAGE DeriveDataTypeable #-}

module Compiler.Type2 where

import Data.Data
import Compiler.Util


prims = ["takeWhileNot","endBy"]
libs = ["spaces","name"]


type RuleName = String
type RuleArg = String

type Program = [Rule]

data Rule = Rule {ruleName :: RuleName, ruleArgs :: [RuleArg], ruleBody :: Choice}
            deriving (Eq,Ord,Typeable,Data)

data Choice = Choice [(Bind Pat,Seq)]
            | NoChoice Seq
              deriving (Eq,Ord,Typeable,Data)

type Action = String

data Seq = Seq [Bind Exp] Action
           deriving (Eq,Ord,Typeable,Data)

data Exp = Prim {expName :: String  , expArgs :: [Exp]}
         | Call {expName :: RuleName, expArgs :: [Exp]}
         | Lit  String
         | Var  RuleArg
           deriving (Eq,Ord,Typeable,Data)

data Pat = PVar RuleArg
         | PLit String
         | PPrim String
         | PWildcard
           deriving (Eq,Ord,Typeable,Data)

data Bind a = Bind {bindVar :: Maybe String, bindBody :: a}
              deriving (Eq,Ord,Typeable,Data)


asLit (Lit x) = Just x
asLit _ = Nothing

isCall Call{} = True; isCall _ = False


getRule xs name = fromJust $ find ((==) name . ruleName) xs


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
    show (Lit x) = show x
    show (Var x) = '$':x

instance Show Pat where
    show (PVar x) = '$':x
    show (PPrim x) = '#':x
    show (PLit x) = show x
    show PWildcard = "_"

instance Show a => Show (Bind a) where
    show (Bind (Just a) b) = a ++ "@" ++ show b
    show (Bind _ b) = show b
