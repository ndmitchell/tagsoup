
module Parser(parse) where

import Type
import Data.List
import Data.Char


parse :: String -> Program1
parse = map (parseStmt . words) . joinLines .
        filter (\x -> not $ all isSpace x || "#" `isPrefixOf` x) . lines


joinLines :: [String] -> [String]
joinLines (x:(y:ys):zs) | isSpace y = joinLines ((x ++ y:ys):zs)
joinLines (x:xs) = x : joinLines xs
joinLines [] = []


parseStmt :: [String] -> Stmt1
parseStmt (x:"=":xs) = Stmt1 a (f b) $ parseExp xs
    where
        (a,b) = parseCall x
        f (Just (Call1 x Nothing Nothing)) = x
        f Nothing = ""


parseExp :: [String] -> Exp1
parseExp x = case split "|" x of
    [x] -> parseSeq x
    xs -> Choice1 $ map (f . parseSeq) xs
    where
        f (Seq1 act (x:xs)) = (x,Seq1 act xs)


parseSeq :: [String] -> Exp1
parseSeq xs@(('{':_):_) = Seq1 (init $ tail $ unwords $ a ++ [b]) $ parseItems bs
    where (a,b:bs) = break ("}" `isSuffixOf`) xs
parseSeq xs = Seq1 defaultAction $ parseItems xs


parseItems :: [String] -> [Item1]
parseItems x = map parseItem x


parseItem :: String -> Item1
parseItem ('\"':xs) | "\"" `isSuffixOf` xs = Literal1 $ init xs
parseItem x = Call1 c d $ if null b then Nothing else Just $ read $ tail b
    where
        (a,b) = break (== '$') x
        (c,d) = parseCall a


-- foo(xs) = (foo,xs)
parseCall :: String -> (String, Maybe Item1)
parseCall xs | ")" `isSuffixOf` xs = (a, Just $ parseItem $ init b)
    where (a,_:b) = break (=='(') xs
parseCall xs = (xs,Nothing)


split :: Eq a => a -> [a] -> [[a]]
split y xs = case break (== y) xs of
    (a,b:bs) -> a : split y bs
    _ -> [xs]

