
module Main where

suCC :: Int -> Int
suCC x = x + 1

isdivs :: Int  -> Int -> Bool
isdivs n x = neqInt (mod x n) 0

the_filter :: [Int] -> [Int]
the_filter x = case x of
    n:ns -> filter (isdivs n) ns
    _ -> error "the_filter failed"

primes :: [Int]
primes = map head (iterate the_filter (iterate suCC 2))

main x = bang primes x


map f xs = case xs of
    [] -> []
    y:ys -> f y : map f ys

head x = case x of
    [] -> error "error in head"
    x:xs -> x


iterate f x = x : iterate f (f x)


filter f xs = case xs of
    [] -> []
    y:ys -> case f y of
        True -> y : filter f ys
        False -> filter f ys


bang xs n = case xs of
    [] -> error "bang"
    x:xs -> case eqInt n 0 of
        True -> x
        False -> bang xs (n-1)
