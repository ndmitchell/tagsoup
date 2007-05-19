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
               else liftParseLazy2
                       (\c ~(found,str) -> (found,c:str))
                       nextChar recurse
   in  recurse
{-
runStateT (readUntil "-->") (initialPos "input", "<!-- comment --> other stuff")
-}


{- |
@apply@ is similar to @liftM ($)@
but @apply@ expects that the second parser succeeds.
This way it can return constructors
created by the first parser quickly,
which is important for lazy parsing.
-}
apply :: Parser (a -> b) -> Parser a -> Parser b
apply f x =
   f >>= (\g -> StateT (\pcs ->
       -- use laziness of 'let'
       let Just (y, pcs') = runStateT x pcs
       in  Just (g y, pcs')))


liftParseLazy ::
   (a -> b)
     -> Parser a
     -> Parser b
liftParseLazy f x =
   return f `apply` x

liftParseLazy2 ::
   (a -> b -> c)
     -> Parser a
     -> Parser b
     -> Parser c
liftParseLazy2 f x y =
   liftParseLazy f x `apply` y

