{-
Example program for TagSoup.
Detects basic syntax errors like isolated ampersands and unquoted attribute values.
-}
module Main where

import Text.HTML.TagSoup
import System.Environment


validate :: String -> String
validate = unlines . f . parseTagsOptions opts
    where
        opts = options{optTagPosition=True, optTagWarning=True}

        f (TagPosition row col:TagWarning warn:rest) =
            ("Warning (" ++ show row ++ "," ++ show col ++ "): " ++ warn) : f rest
        f (TagWarning warn:rest) =
            ("Warning (?,?): " ++ warn) : f rest
        f (x:xs) = f xs
        f [] = []

validateIO :: FilePath -> IO ()
validateIO file =
   do text <- readFile file
      putStrLn (validate text)


main :: IO ()
main = mapM_ validateIO =<< getArgs
