
module Text.HTML.TagSoup.Development.Sample(test) where

import Text.HTML.TagSoup.Development.ParserUtils
import GHC.Base

{-
data Sample = Comment String | Letter Char


sample :: Parser [Sample]
sample = pick
        ["<!--" |-> comment & \ ~(a,b) -> Comment a : b
        ,cons $ \x s -> Letter x : sample s
        ,nil [] ]


comment :: Parser (String,[Sample])
comment s0 = (text, sample s1)
    where (text, s1, found) = spanEndS "-->" s0

-}


{-
test s | ['n','a'] `isPrefixOf` s = "test"
       | ['n','b'] `isPrefixOf` s = "test2"


{ -# INLINE isPrefixOf #- }

isPrefixOf :: [Char] -> [Char] -> Bool
isPrefixOf (x:xs) (y:ys) | x == y = isPrefixOf2 xs ys
isPrefixOf [] _ = True
isPrefixOf _ _ = False

isPrefixOf2 :: [Char] -> [Char] -> Bool
isPrefixOf2 (x:xs) (y:ys) | x == y = True
isPrefixOf2 [] _ = True
isPrefixOf2 _ _ = False
-}


{-# INLINE begin1 #-}
{-# INLINE begin2 #-}


test s | begin2 'n' 'a' s = "test"
       | begin2 'n' 'b' s = "test2"


begin2 :: Char -> Char -> String -> Bool
begin2 x1 x2 (y:ys) | x1 == y = begin1 x2 ys
begin2 _ _ _ = False

begin1 :: Char -> String -> Bool
begin1 x1 (y:ys) | x1 == y = True



{- # RULES "prefix"       forall x xs y. isPrefixOf (x:xs) y = case y of {y:ys | y == x -> isPrefixOf xs ys ; _ -> False} #-}

