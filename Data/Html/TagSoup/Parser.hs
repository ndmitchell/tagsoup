module Data.Html.TagSoup.Parser where


import Text.ParserCombinators.Parsec.Pos
          (SourcePos, updatePosChar)

import Control.Monad.State (StateT(..), gets, mplus, liftM2)

import Data.Char (isSpace)


-- cf. Haskore.General.Parser
type Parser a = StateT (SourcePos,String) Maybe a

nextChar :: Parser Char
nextChar =
   StateT $ \(pos,str) ->
      case str of
         []     -> Nothing
         (c:cs) -> Just (c,(updatePosChar pos c, cs))

eof :: Parser Bool
eof = gets (null . snd)

getPos :: Parser SourcePos
getPos = gets fst

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
   do c <- nextChar
      if p c
        then return c
        else fail "character not matched"

-- | does never fail
many :: Parser a -> Parser [a]
many x =
   {- It is better to have 'force' at the place it is,
      instead of writing it to the recursive call,
      because 'x' can cause an infinite loop. -}
   force $ mplus (many1 x) (return [])

many1 :: Parser a -> Parser [a]
many1 x = liftM2 (:) x (many x)

manySatisfy :: (Char -> Bool) -> Parser String
manySatisfy = many . satisfy

many1Satisfy :: (Char -> Bool) -> Parser String
many1Satisfy = many1 . satisfy

dropSpaces :: Parser ()
dropSpaces =
   fmap (const ()) $ manySatisfy isSpace


char :: Char -> Parser Char
char c = satisfy (c==)

string :: String -> Parser String
string = mapM char

readUntilStrict :: String -> Parser String
readUntilStrict pattern =
   let recurse =
          string pattern
          `mplus`
          liftM2 (:) nextChar recurse
   in  recurse

readUntil :: String -> Parser (Bool,String)
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
force :: Parser a -> Parser a
force x =
   StateT (\pcs ->
      let Just (y,pcs') = runStateT x pcs
      in  Just (y,pcs'))
