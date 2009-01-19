
module Compiler.CodeGen where

import Compiler.Type
import Data.List
import Data.Char


codeGen :: Program2 -> String
codeGen x = unlines $ [warning,"{-",showProgram2 x ++ "-}"] ++ concatMap genStmt x
    where warning = "-- AUTO-GENERATED CODE, DO NOT MODIFY"

indent n = (++) (replicate n ' ')

spw n = concat $ intersperse (show n) ["(s",",p",",w",")"]


genStmt (Stmt2 name x) = "" : (name ++ " opts (s0,p0,w0) = " ++ a) : b
    where (a:b) = genExp x


genExp (Choice2 alts def) = "case s0 of" : map (indent 4) (map f alts ++ [d])
    where
        f ("",rule) = "\"\" -> " ++ rule ++ " opts " ++ spw 0
        f (str,rule) = pat ++ " -> " ++ rule ++ " opts (s1,p0,w0)"
            where pat = concat $ intersperse ":" $ map show str ++ ["s1"]
        d = "_ -> " ++ def ++ " opts " ++ spw 0


genExp (Seq2 act []) = [genAct 0 act]
genExp (Seq2 act xs) = genAct (length xs) act : ("    where " ++ y) : map (indent 10) ys
    where y:ys = zipWith genItem [1..] xs


genAct n x = "(" ++ genOp x ++ "," ++ spw n ++ ")"

genItem i x = "(" ++ v ++ "," ++ spw i ++ ") = " ++ f x
    where
        v = maybe "_ " (\i -> "v" ++ show i) $ getBind x

        f (Rule2 name _) = name ++ " opts " ++ spw (i-1)
        f (Literal2 x) = "literal opts " ++ show x ++ " " ++ spw (i-1)
        f (Prim2 name arg _) = name ++ " opts " ++ show arg ++ " " ++ spw (i-1)


genOp ('$':x:xs) | isDigit x = 'v':x : genOp xs
genOp ('$':'w':'a':'r':'n':xs) = "w0" ++ genOp xs
genOp ('$':'p':'o':'s':xs) = "p0" ++ genOp xs
genOp (x:xs) = x : genOp xs
genOp [] = []


{-




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

-}
