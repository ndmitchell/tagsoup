{-# LANGUAGE DeriveDataTypeable #-}

module Compiler.Hs where

import Text.PrettyPrint.HughesPJ
import Compiler.Util hiding ((<+>))
import Data.Data


type Program = [Func]

data Func = Func String [String] Exp
            deriving (Eq,Ord,Typeable,Data)

data Exp = Case Exp [(Exp,Exp)]
         | Let [(Exp,Exp)] Exp
         | Raw String
         | Wildcard
         | Var String
         | Con String
         | App Exp [Exp]
           deriving (Eq,Ord,Typeable,Data)


instance Show Func where
    show (Func name args bod) = render (text initial <>> docExp bod) ++ "\n"
        where initial = unwords $ name:args++["="]
    showList = showString . unlines . map show

instance Show Exp where
    show = render . docExp



inner :: Doc -> Doc
inner = nest 4

(<>>) :: Doc -> Doc -> Doc
a <>> b = sep [a, inner b]


docExp :: Exp -> Doc
docExp x = f False x
    where
        f b (Raw x) = brack (b && not (all isAlphaNum x)) $ text x
        f b Wildcard = text "_"
        f b (Var x) = text x
        f b (Con x) = text x
        f b (App (Con "(,)") [x,y]) = parens $ f False x <> text "," <+> f False y
        f b (App x xs) = brack b $ hsep $ f True x : map (f True) xs

        f b (Case on alts) = brack b (text "case" <+> f True on <+> text "of" $$ inner (vcat $ map g alts))
            where
                g (a,b) = (f False a <+> text "->") <>> f False b

        f b (Let [] x) = f b x
        f b (Let binds x) = brack b $ text "let" <+> vcat (map g binds) $$ text "in" <+> f False x
            where g (lhs,rhs) = f False lhs <+> text "=" <>> f False rhs

brack b = if b then parens else id
