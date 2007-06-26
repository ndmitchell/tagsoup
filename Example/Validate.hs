{-
Example program for TagSoup.
Detects basic syntax errors like isolated ampersands and unquoted attribute values.
-}
module Main where

import Text.HTML.TagSoup
import Text.HTML.TagSoup.Position
import System.Environment


validate :: FilePath -> String -> String
validate file input = unlines
    [toReportText (setFileName file pos) ++ " " ++ msg
    | TagPos pos (TagWarning msg) <- parseTagsGeneric input :: [TagPos Char]]

validateIO :: FilePath -> IO ()
validateIO file =
   do text <- readFile file
      putStrLn (validate file text)


main :: IO ()
main = mapM_ validateIO =<< getArgs
