{-# OPTIONS_GHC -O2 #-}

module Text.ParserCombinators.LazyParse
    (Parser, runParser
    ,get, put, modify
    ,eof, def, (==>), choice
    ,takesUntil, many, one, lit
    ) where

import Control.Monad.State
import Data.Maybe
import Data.Char
import Text.StringLike

infix 0 ==>


type Parser s a = State s a

runParser :: Parser s a -> s -> a
runParser = evalState


unstr :: StringLike s => String -> s -> Maybe s
unstr [] s = Just s
unstr (x:xs) s = case uncons s of
    Just (c,s2) | c == x -> unstr xs s2
    _ -> Nothing


data Lhs = Lit String
         | Def
         | EOF

data Choice a b = Choice [(Lhs,a)]

instance Monad (Choice a) where
    Choice xs >> Choice ys = Choice $ xs++ys
    a >>= f = a >> f undefined
    return a = error "Choice does not implement return"


class AsLhs a where
    asLhs :: a -> Lhs

instance AsLhs Lhs where
    asLhs = id

instance CharLike a => AsLhs [a] where
    asLhs = Lit . map toChar

eof = EOF
def = Def


(==>) :: (AsLhs l, StringLike s) => l -> Parser s a -> Choice (Parser s a) b
xs ==> p = Choice [(asLhs xs,p)]

choice :: StringLike s => Choice (Parser s a) b -> Parser s a
choice (Choice xs) = do
    s <- get
    f s xs
    where
        f s [] = error "Failed to match choice"
        f s ((Def,x):xs) = x
        f s ((EOF,x):xs) | isEmpty s = x
        f s ((Lit y,x):xs) = case unstr y s of
            Nothing -> f s xs
            Just s2 -> do put s2; x
        f s (x:xs) = f s xs


takesUntil :: (StringLike s, StringLike r) => String -> Parser s r
takesUntil xs = do
    s <- get
    case unstr xs s of
        Just s -> do put s; return empty
        Nothing -> case uncons s of
            Nothing -> return empty
            Just (c,s2) -> do
                put s2
                cs <- takesUntil xs
                return $ cons c cs


many :: (StringLike s, StringLike r) => (Char -> Bool) -> Parser s r
many f = do
    s <- get
    case uncons s of
        Just (c,s2) | f c -> do
            put s2
            res <- many f
            return $ cons c res
        _ -> return empty

one :: (StringLike s, StringLike r) => (Char -> Bool) -> Parser s r
one f = do
    s <- get
    case uncons s of
        Just (c,s2) | f c -> do put s2 ; return $ cons c empty
        _ -> return empty

lit :: (StringLike s, StringLike r) => String -> Parser s r
lit xs = do
    s <- get
    case unstr xs s of
        Nothing -> return empty
        Just s2 -> do put s2 ; return $ fromString xs
