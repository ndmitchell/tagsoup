module Text.HTML.TagSoup.Parser (
   Parser, Status(Status),
   char, dropSpaces, eof, force, getPos,
   many, many1, many1Satisfy, manySatisfy, readUntil,
   satisfy, source, string,
   emit,
   eval, write, gets, mfix)
  where


import Text.ParserCombinators.Parsec.Pos
          (SourcePos, initialPos, updatePosChar)

import Control.Monad.RWS (RWST(..), evalRWST, gets, mplus, liftM2, tell)
import Control.Monad.Fix (mfix)

import Data.Char (isSpace)


-- cf. Haskore.General.Parser
type Parser w a = RWST () [w] Status Maybe a

data Status =
   Status {
      sourcePos :: SourcePos,
      source    :: String}
   deriving Show

write :: Parser w () -> String -> Maybe [w]
write p =
   fmap snd .
   evalRWST p () .
   Status (initialPos "anonymous input")

eval :: Parser w a -> String -> Maybe a
eval p =
   fmap fst .
   evalRWST p () .
   Status (initialPos "anonymous input")

nextChar :: Parser w Char
nextChar =
   RWST $ \ () (Status pos str) ->
      case str of
         []     -> Nothing
         (c:cs) -> Just (c, Status (updatePosChar pos c) cs, [])

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
   ignoreEmit $ fmap (const ()) $ manySatisfy isSpace


char :: Char -> Parser w Char
char c = satisfy (c==)

string :: String -> Parser w String
string = ignoreEmit . mapM char

readUntil :: String -> Parser w (Bool,String)
readUntil pattern =
   let recurse =
          force $
          fmap (const (True,[])) (string pattern)
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


{- |
Turn a parser @x@ into one that evaluates to bottom if @x@ fails.
This is useful if you know that @x@ cannot fail.
Using force let you return data immediately and thus makes parsing lazier.
-}
force :: Parser w a -> Parser w a
force x =
   RWST $ \ () pcs ->
      let Just (y,pcs',w) = runRWST x () pcs
      in  Just (y,pcs',w)

{- |
Turn a parser @x@ into one that is evaluated to bottom if @x@ emits something
throught the writer part of the monad.
This is useful if you know that @x@ does not emit something.
In this case you allow subsequent emitters to emit,
also if the current statement needs infinite steps to complete.

It checks, whether the ignored information is actually empty.
However this safety leads to too much strictness.

noEmit :: Parser w a -> Parser w a
noEmit x =
   RWST $ \ () pcs ->
      fmap (\ ~(y,pcs',empty@ ~[]) -> (y,pcs',empty)) $ runRWST x () pcs
-}

ignoreEmit :: Parser w a -> Parser w a
ignoreEmit x =
   RWST $ \ () pcs ->
      fmap (\ ~(y,pcs',_) -> (y,pcs',[])) $ runRWST x () pcs

emit :: w -> Parser w ()
emit w = tell [w]
