{-
Example program for TagSoup.
Detects basic syntax errors like isolated ampersands and unquoted attribute values.
-}
module Main where

import Text.HTML.TagSoup
import qualified Text.HTML.TagSoup.Position as Position

import System.Environment (getArgs)

import Data.Maybe (mapMaybe)


validate :: FilePath -> String -> String
validate fileName input =
   let tags = parseFilePosTags fileName input
       warnings =
          mapMaybe (\(pos,tag) -> fmap ((,) pos) $ maybeTagWarning tag) tags
   in  unlines $ map (\(pos,msg) -> Position.toReportText pos ++ " " ++ msg) warnings

validateIO :: FilePath -> IO ()
validateIO fileName =
   do text <- readFile fileName
      putStrLn (validate fileName text)


main :: IO ()
main = mapM_ validateIO =<< getArgs
