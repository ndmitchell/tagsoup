{-# OPTIONS_GHC -O2 #-}

module Text.ParserCombinators.LazyParse
    (Parser, Parse(..), runParser
    ,get, put, modify
    ,eof, def, (==>), choice
    ,takesUntil, many, one, lit
    ,space, spaces
    ) where

import Control.Monad.State
import Data.Maybe
import Data.Char

infix 0 ==>

class AsChar a where
    asChar :: a -> Char

instance AsChar Char where
    asChar = id



type Parser s a = State s a

runParser :: Parser s a -> s -> a
runParser = evalState

class Parse s where
    un :: s -> Maybe (Char,s)


instance AsChar a => Parse [a] where
    un [] = Nothing
    un (x:xs) = Just (asChar x, xs) 


uns :: Parse s => String -> s -> Maybe s
uns [] s = Just s
uns (x:xs) s = case un s of
    Just (c,s2) | c == x -> uns xs s2
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

instance AsChar a => AsLhs [a] where
    asLhs = Lit . map asChar

eof = EOF
def = Def


(==>) :: (AsLhs l, Parse s) => l -> Parser s a -> Choice (Parser s a) b
xs ==> p = Choice [(asLhs xs,p)]

choice :: Parse s => Choice (Parser s a) b -> Parser s a
choice (Choice xs) = do
    s <- get
    f s xs
    where
        f s [] = error "Failed to match choice"
        f s ((Def,x):xs) = x
        f s ((EOF,x):xs) | isNothing (un s) = x
        f s ((Lit y,x):xs) = case uns y s of
            Nothing -> f s xs
            Just s2 -> do put s2; x
        f s (x:xs) = f s xs


takesUntil :: Parse s => String -> Parser s String
takesUntil xs = do
    s <- get
    case uns xs s of
        Just s -> do put s; return ""
        Nothing -> case un s of
            Nothing -> return ""
            Just (c,s2) -> do
                put s2
                cs <- takesUntil xs
                return $ c:cs


many :: Parse s => (Char -> Bool) -> Parser s String
many f = do
    s <- get
    case un s of
        Just (c,s2) | f c -> do
            put s2
            res <- many f
            return $ c:res
        _ -> return ""


one :: Parse s => (Char -> Bool) -> Parser s String
one f = do
    s <- get
    case un s of
        Just (c,s2) | f c -> do put s2 ; return [c]
        _ -> return ""


space :: Parse s => Parser s String
space = one isSpace

spaces :: Parse s => Parser s String
spaces = many isSpace


lit :: Parse s => String -> Parser s String
lit xs = do
    s <- get
    case uns xs s of
        Nothing -> return ""
        Just s2 -> do put s2 ; return xs
