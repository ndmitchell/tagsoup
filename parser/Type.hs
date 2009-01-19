{-# LANGUAGE DeriveDataTypeable #-}

module Type where

import Data.Data
import Data.List


prims = ["takeWhileNot","endBy"]
libs = ["spaces","name"]


type Program1 = [Stmt1]

data Stmt1 = Stmt1 String String Exp1 -- name = body
            deriving (Eq,Show,Typeable,Data)

data Exp1 = Choice1 [(Item1, Exp1)]
          | Seq1 String [Item1]
           deriving (Eq,Show,Typeable,Data)

data Item1 = Literal1 String
           | Call1 String (Maybe Item1) (Maybe Int)
             deriving (Eq,Show,Typeable,Data)

defaultAction = "$1"


type Program2 = [Stmt2]

type RuleName = String

data Stmt2 = Stmt2 RuleName Exp2
             deriving (Eq,Show,Typeable,Data)

data Exp2 = Seq2 String [Item2]
          | Choice2 [(String,RuleName)] RuleName
            deriving (Eq,Show,Typeable,Data)

data Item2 = Rule2 RuleName (Maybe Int)
           | Prim2 String String (Maybe Int)
           | Literal2 String
             deriving (Eq,Show,Typeable,Data)


showProgram2 :: Program2 -> String
showProgram2 = unlines . map showStmt2

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
