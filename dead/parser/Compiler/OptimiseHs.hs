
module Compiler.OptimiseHs(optimiseHs) where

import Compiler.Hs
import Compiler.Util


optimiseHs :: Program -> Program
optimiseHs = error . ("\n"++) . show . simpleLet


simpleLet :: Program -> Program
simpleLet = transformBi f
    where
        f (Let [] x) = x
        f (Let [(x,y)] z) | show x == show z = y
        f x = x
