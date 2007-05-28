module Text.HTML.TagSoup.Parser (
   Parser, Status(Status),
   char, dropSpaces, eof, force, getPos,
   many, many1, many1Satisfy, manySatisfy, readUntil,
   satisfy, source, string,
   emit,
   eval, write, gets, mfix)
  where


import Text.ParserCombinators.Parsec.Pos
          (SourcePos, initialPos)

import Text.HTML.TagSoup.Parser.Custom
-- import Text.HTML.TagSoup.Parser.MTL

import Control.Monad (mplus, liftM, liftM2)
import Control.Monad.Fix (mfix)

import Data.Char (isSpace)


write :: Parser w () -> String -> Maybe [w]
write p =
   fmap (\ ~(_,_,ws) -> ws) .
   run p .
   Status (initialPos "input")

eval :: Parser w a -> String -> Maybe a
eval p =
   fmap (\ ~(x,_,_) -> x) .
   run p .
   Status (initialPos "input")



eof :: Parser w Bool
eof = gets (null . source)

getPos :: Parser w SourcePos
getPos = gets sourcePos

satisfy :: (Char -> Bool) -> Parser w Char
satisfy p =
   do c <- nextChar
      if p c
        then return c
        else fail "character not matched"

-- | does never fail
many :: Parser w a -> Parser w [a]
many x =
   {- It is better to have 'force' at the place it is,
      instead of writing it to the recursive call,
      because 'x' can cause an infinite loop. -}
   force $ mplus (many1 x) (return [])

many1 :: Parser w a -> Parser w [a]
many1 x = liftM2 (:) x (many x)

manySatisfy :: (Char -> Bool) -> Parser w String
manySatisfy = ignoreEmit . many . satisfy

many1Satisfy :: (Char -> Bool) -> Parser w String
many1Satisfy = ignoreEmit . many1 . satisfy

dropSpaces :: Parser w ()
dropSpaces =
   ignoreEmit $ liftM (const ()) $ manySatisfy isSpace


char :: Char -> Parser w Char
char c = satisfy (c==)

string :: String -> Parser w String
string = ignoreEmit . mapM char


readUntil :: String -> Parser w (Bool,String)
readUntil pattern =
   let recurse =
          force $
          liftM (const (True,[])) (string pattern)
          `mplus`
          do isEOF <- eof
             if isEOF
               then return (False,[])
               else liftM2
                       (\c ~(found,str) -> (found,c:str))
                       nextChar recurse
   in  ignoreEmit recurse
{-
runStateT (readUntil "-->") (initialPos "input", "<!-- comment --> other stuff")
-}



emit :: w -> Parser w ()
emit w = tell [w]
