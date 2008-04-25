
module Text.HTML.TagSoup.Development.ParserUtils
    (PState, Parser, Pick, runParser
    ,(&), (|->), cons, nil, pick
    ,nullS, headS, tailS
    ,dropS, spanS, dropWhileS
    ,spanEndS
    ) where

import Data.List


infix 2 |->
infix 3 &

---------------------------------------------------------------------
-- DATA TYPES

data PState = PState {text :: String, row :: !Int, col :: !Int}

type Parser a = PState -> a

type Pick a = Parser (Maybe a)


runParser :: Parser a -> String -> a
runParser p s = p (PState s 1 1)


---------------------------------------------------------------------
-- PARSER COMBINATORS

(&) :: Parser a -> (a -> b) -> Parser b
(&) p f s = f (p s)


(|->) :: String -> Parser a -> Pick a
(|->) t p s | t `isPrefixOf` text s = Just $ p $ dropS (length t) s
            | otherwise = Nothing


cons :: (Char -> PState -> a) -> Pick a
cons f s | not $ nullS s = Just $ f (headS s) (tailS s)
cons f _ = Nothing


nil :: a -> Pick a
nil r s | nullS s = Just r
nil r _ = Nothing


pick :: [Pick a] -> Parser a
pick (x:xs) s = case x s of
                    Nothing -> pick xs s
                    Just y -> y
pick _ s = error "No pick alternatives matched"


---------------------------------------------------------------------
-- STATE COMBINATORS

update x xs r c = case x of
    '\n' -> PState xs (r+1) 1
    '\t' -> PState xs r (c + 8 - mod (c-1) 8)
    _    -> PState xs r (c+1)


nullS :: PState -> Bool
nullS = null . text


tailS :: PState -> PState
tailS (PState (x:xs) r c) = update x xs r c


headS :: PState -> Char
headS = head . text


dropS :: Int -> PState -> PState
dropS n (PState (x:xs) r c) | n > 0 = dropS (n-1) (update x xs r c)
dropS n s = s


spanS :: (Char -> Bool) -> PState -> (String, PState)
spanS f (PState (x:xs) r c) | f x = (x:a, b)
    where (a,b) = spanS f $ update x xs r c
spanS f s = ("", s)


dropWhileS :: (Char -> Bool) -> PState -> PState
dropWhileS f (PState (x:xs) r c) | f x = dropWhileS f $ update x xs r c
dropWhileS f s = s


-- keep reading until you reach some end text
-- (x,y,True ) = spanEnd t s  ==> s == x ++ t ++ y
-- (x,y,False) = spanEnd t s  ==> s == x  &&  y == ""
spanEndS :: String -> PState -> (String, PState, Bool)
spanEndS t s@(PState xs r c) | t `isPrefixOf` xs = ("", dropS (length t) s, True)
spanEndS t s@(PState []     r c) = ("", s, False)
spanEndS t s@(PState (x:xs) r c) = (x:r1,r2,r3)
    where (r1,r2,r3) = spanEndS t $ update x xs r c
