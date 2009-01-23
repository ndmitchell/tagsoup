
module Compiler.Parser(parse) where

import Compiler.Lp
import Compiler.Util


parse :: String -> Program
parse = map parseRule . chunks . filter (not . dull) . lines
    where
        dull x = all isSpace x || "#" `isPrefixOf` x
        chunks = rep (\(x:xs) -> first (x:) $ break (not . isSpace . head) xs)


parseRule :: [String] -> Rule
parseRule [x] = Rule name args $ NoChoice $ parseSeq body
    where (name:args,body) = break' "=" $ lexemes x
parseRule (x:ys) = Rule name args $ Choice $ map (parseAlt . lexemes) ys
    where (name:args) = lexemes x


parseAlt :: [String] -> (Bind Pat,Seq)
parseAlt (x:"=":y) = (parseBind parsePat x, parseSeq y)


parseSeq :: [String] -> Seq
parseSeq xs = Seq (map (parseBind parseExp) a) (if null b then "res" else uncurly $ head b)
    where (a,b) = break ("{" `isPrefixOf`) xs


parseBind :: (String -> a) -> String -> Bind a
parseBind f x | "@" `isPrefixOf` b = Bind (Just a) $ f $ unround $ tail b
              | otherwise = Bind Nothing $ f $ unround x
    where (a,b) = span isAlpha x


parsePat :: String -> Pat
parsePat "_" = PWildcard
parsePat x@('\"':_) = PLit $ read x
parsePat x = PPrim x


parseExp :: String -> Exp
parseExp x@('\"':_) = Lit $ read x
parseExp x = Prim name $ map parseExp args
    where (name:args) = words x


---------------------------------------------------------------------
-- UTILITIES

break' :: (Show a, Eq a) => a -> [a] -> ([a],[a])
break' x xs | null b = error $ "Parse error, expected " ++ show a ++ " in " ++ unwords (map show xs)
            | otherwise = (a, tail b)
    where (a,b) = break (== x) xs


lexemes :: String -> [String]
lexemes = f . words
    where
        f (x:xs) | isJust v = unwords (a++[b]) : f bs
            where v = getBracket x
                  (a,b:bs) = break (fromJust v `elem`) (x:xs)
        f (x:xs) = x : f xs
        f [] = []


getBracket ('(':xs) = Just ')'
getBracket ('{':xs) = Just '}'
getBracket (_:xs) = getBracket xs
getBracket [] = Nothing


unround ('(':xs) | ")" `isSuffixOf` xs = init xs
unround x = x

uncurly ('{':xs) | "}" `isSuffixOf` xs = init xs
uncurly x = x

