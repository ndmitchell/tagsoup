module Data.Html.TagSoup.Parser where


import Text.ParserCombinators.Parsec.Pos
          (SourcePos, initialPos, updatePosChar)

import Control.Monad.State (StateT(..), gets, mplus, liftM2)


-- cf. Haskore.General.Parser
type Parser a = StateT (SourcePos,String) Maybe a

zeroOrMore   :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p `mplus` return []

oneOrMore    :: Parser a -> Parser [a]
oneOrMore p = liftM2 (:) p (zeroOrMore p)

nextChar :: Parser Char
nextChar =
   StateT $ \(pos,str) ->
      case str of
         []     -> Nothing
         (c:cs) -> Just (c,(updatePosChar pos c, cs))

eof :: Parser Bool
eof = gets (null . snd)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p =
   do c <- nextChar
      if p c
        then return c
        else fail "character not matched"

string :: String -> Parser String
string = mapM (\c -> satisfy (c==))

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
          fmap (const (True,[])) (string pattern)
          `mplus`
          do isEOF <- eof
             if isEOF
               then return (False,[])
               else liftM2
                       (\c ~(found,str) -> (found,c:str))
                       nextChar (force recurse)
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
