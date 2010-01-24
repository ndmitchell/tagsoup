
module Main(main) where

import Control.Monad
import Data.List
import Data.Generics.PlateData

import HSE
import Type
import Convert
import qualified Desugar
import Supercompile
import Translate
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    if "translate" `elem` args then
        translate
     else
        mapM_ optimise args


optimise x = do
    prog <- fmap fromParseResult $ parseFile $ "TagSoup/Generate/programs/" ++ x ++ ".hs"
    res <- return $ output $ supercompiler $ input $ moduleDecls prog
    writeFile ("TagSoup/Generate/programs/" ++ x ++ ".opt") $
        unlines $ concatMap (lines . prettyPrint) res
