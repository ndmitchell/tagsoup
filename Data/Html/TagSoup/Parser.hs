module Data.Html.TagSoup.Parser where


import Text.ParserCombinators.Parsec.Pos
          (SourcePos, updatePosChar)

import Control.Monad.RWS (RWST(..), gets, mplus, liftM2)

import Data.Monoid (mempty)
import Data.Char (isSpace)


-- cf. Haskore.General.Parser
type Parser w a = RWST () [w] Status Maybe a

data Status =
   Status {
      sourcePos :: SourcePos,
      source    :: String}
   deriving Show

nextChar :: Parser w Char
nextChar =
   RWST $ \ () (Status pos str) ->
      case str of
         []     -> Nothing
         (c:cs) -> Just (c, Status (updatePosChar pos c) cs, mempty)

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
manySatisfy = many . satisfy

many1Satisfy :: (Char -> Bool) -> Parser w String
many1Satisfy = many1 . satisfy

dropSpaces :: Parser w ()
dropSpaces =
   fmap (const ()) $ manySatisfy isSpace


char :: Char -> Parser w Char
char c = satisfy (c==)

string :: String -> Parser w String
string = mapM char

readUntilStrict :: String -> Parser w String
readUntilStrict pattern =
   let recurse =
          string pattern
          `mplus`
          liftM2 (:) nextChar recurse
   in  recurse

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
   in  recurse
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
