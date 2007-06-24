{-|
    Module      :  Text.HTML.TagSoup
    Copyright   :  (c) Neil Mitchell 2006-2007
    License     :  BSD-style

    Maintainer  :  http://www.cs.york.ac.uk/~ndm/
    Stability   :  moving towards stable
    Portability :  portable

    This module is for extracting information out of unstructured HTML code,
    sometimes known as tag-soup. This is for situations where the author of
    the HTML is not cooperating with the person trying to extract the information,
    but is also not trying to hide the information.

    The standard practice is to parse a String to 'Tag's using 'parsePosTags',
    then operate upon it to extract the necessary information.
-}

module Text.HTML.TagSoup(
    -- * Data structures and parsing
    Tag(..), PosTag, Attribute, CharType, HTMLChar(..),
    parseTags, parsePosTags, parseFilePosTags,
    parseTag, parseInnerOfTag,
    canonicalizeTags, canonicalizePosTags,

    -- * Tag identification
    isTagOpen, isTagClose, isTagText, isTagWarning,

    -- * Extraction
    fromTagText, fromAttrib,
    maybeTagText, maybeTagWarning,
    innerText,

    -- * Utility
    sections, partitions,
    ) where

import Text.HTML.TagSoup.Parser
   (char, dropSpaces, force, getPos,
    many, many1, many1Satisfy, readUntil,
    satisfy, string,
    emit, mfix)

import qualified Text.HTML.TagSoup.Parser as Parser
import qualified Text.HTML.TagSoup.Entity as HTMLEntity

import Text.HTML.TagSoup.Position (Position)
import Text.HTML.TagSoup.Type

import Control.Monad (mplus, msum, when, liftM)

import Data.Char (isAlphaNum, isDigit, toLower, toUpper, chr)
import Data.List (tails, groupBy)
import Data.Maybe (fromMaybe, mapMaybe)


type PosTag char = (Position, Tag char)

type Parser char a = Parser.Parser (PosTag char) a

{- |
Turns all tag names to lower case and
converts DOCTYPE to upper case.
-}
canonicalizePosTags :: [PosTag char] -> [PosTag char]
canonicalizePosTags =
   map (\(i,tag) -> (i, canonicalizeTag tag))

canonicalizeTags :: [Tag char] -> [Tag char]
canonicalizeTags =
   map canonicalizeTag

canonicalizeTag :: Tag char -> Tag char
canonicalizeTag t =
   case t of
      TagOpen  name attrs  -> TagOpen  (map toLower name) attrs
      TagClose name        -> TagClose (map toLower name)
      TagSpecial name info -> TagSpecial (map toUpper name) info
      _ -> t


{- |
Parse a single tag, throws an error if there is a syntax error.
This is useful for parsing a match pattern.
-}
parseTag :: (CharType char, Show char) =>
   String -> Tag char
parseTag str =
   let tags =
          fromMaybe (error "tagEqualElement: parse should never fail") $
          Parser.write "string" parsePosTag str
       throwError = error $
          "parseTag: parsing results in\n" ++
          unlines (map show tags)
   in  case tags of
          [(_,tag)] ->
              if isTagWarning tag
                then throwError
                else tag
          _ -> throwError

{- |
Parse the inner of a single tag.
That is, @parseTag \"\<bla\>\"@ is the same as @parseInnerOfTag \"\<bla\>\"@.
-}
parseInnerOfTag :: (CharType char, Show char) =>
   String -> Tag char
parseInnerOfTag str = parseTag $ "<"++str++">"



parseFilePosTags :: CharType char => FilePath -> String -> [PosTag char]
parseFilePosTags fileName =
   fromMaybe (error "parseFilePosTag can never fail.") .
   Parser.write fileName (many parsePosTag >> return ())


-- | Parse an HTML document to a list of 'Tag'.
-- Automatically expands out escape characters.
parsePosTags :: CharType char => String -> [PosTag char]
parsePosTags =
   fromMaybe (error "parsePosTag can never fail.") .
   Parser.write "input" (many parsePosTag >> return ())

-- | Like 'parsePosTags' but hides source file positions.
parseTags :: CharType char => String -> [Tag char]
parseTags = map snd . parsePosTags


parsePosTag :: CharType char => Parser char ()
parsePosTag = do
   pos <- getPos
   msum $
    (do char '<'
        msum $
         parseSpecialTag pos :
         parseCloseTag pos :
         parseOpenTag pos :
         (do emitTag pos (TagText [fromChar '<'])
             emitWarning pos "A '<', that is not part of a tag. Encode it as &lt; please."
         ) :
         []
    ) :
    parseText pos :
    []


parseOpenTag :: CharType char => Position -> Parser char ()
parseOpenTag pos =
   do name <- parseName
      dropSpaces
      mfix
         (\attrs ->
            emit (pos, TagOpen name attrs) >>
            many parseAttribute)
      force $ msum $
        (do closePos <- getPos
            string "/>"
            emitTag closePos (TagClose name)) :
        (do junkPos <- getPos
            readUntilTerm
               (\ junk ->
                  emitWarningWhen
                     (not $ null junk)
                     junkPos ("Junk in opening tag: \"" ++ junk ++ "\""))
               ("Unterminated open tag \"" ++ name ++ "\"") ">") :
        []

parseCloseTag :: Position -> Parser char ()
parseCloseTag pos =
   do char '/'
      name <- parseName
      emitTag pos (TagClose name)
      dropSpaces
      junkPos <- getPos
      readUntilTerm
         (\ junk ->
            emitWarningWhen
               (not $ null junk)
               junkPos ("Junk in closing tag: \"" ++ junk ++"\""))
         ("Unterminated closing tag \"" ++ name ++"\"") ">"

parseSpecialTag :: Position -> Parser char ()
parseSpecialTag pos =
   do char '!'
      msum $
       (do string "--"
           readUntilTerm
              (\ cmt -> emitTag pos (TagComment cmt))
              "Unterminated comment" "-->") :
       (do name <- many1Satisfy isAlphaNum
           dropSpaces
           readUntilTerm
              (\ info -> emitTag pos (TagSpecial name info))
              ("Unterminated special tag \"" ++ name ++ "\"") ">") :
       []

parseText :: CharType char => Position -> Parser char ()
parseText pos =
   mfix
     (\ text ->
        emitTag pos (TagText text) >>
        parseString1 ('<'/=))
     >> return ()


parseAttribute :: CharType char => Parser char (Attribute char)
parseAttribute =
   do name <- parseName
      dropSpaces
      value <-
         force $
         mplus
            (string "=" >> dropSpaces >> parseValue)
            (return [])
      dropSpaces
      return (name, value)

parseName :: Parser char String
parseName =
   many1Satisfy (\c -> isAlphaNum c || c `elem` "_-:")

parseValue :: CharType char => Parser char [char]
parseValue =
   force $ msum $
      parseQuoted "Unterminated doubly quoted value string" '"' :
      parseQuoted "Unterminated singly quoted value string" '\'' :
      parseUnquotedValue :
      []

parseUnquotedValueChar :: Parser Char String
parseUnquotedValueChar =
   let parseValueChar =
          do pos <- getPos
             str <- parseChar (not . flip elem " >\"\'")
             let wrong = filter (not . isValidValueChar) str
             emitWarningWhen
                (not (null wrong))
                pos $ "Illegal characters in unquoted value: " ++ wrong
             return str
   in  liftM concat $ many parseValueChar

parseUnquotedValueHTMLChar :: Parser HTMLChar [HTMLChar]
parseUnquotedValueHTMLChar =
   let parseValueChar =
          do pos <- getPos
             hc <- parseHTMLChar (not . flip elem " >\"\'")
             let wrong =
                    case hc of
                       Char c -> not (isValidValueChar c)
                       _ -> False
             emitWarningWhen wrong pos $
                "Illegal characters in unquoted value: '" ++ show wrong ++ "'"
             return hc
   in  many parseValueChar

isValidValueChar :: Char -> Bool
isValidValueChar c  =  isAlphaNum c || c `elem` "_-:."

parseQuoted :: CharType char => String -> Char -> Parser char [char]
parseQuoted termMsg quote =
   do char quote
      str <- parseString (quote/=)
      force $ mplus
         (char quote >> return ())
         (do termPos <- getPos
             emitWarning termPos termMsg)
      return str

{-
Instead of using 'generateTag' we could also wrap the call to 'readUntilTerm'
in 'mfix' in order to emit a tag, where some information is read later.
-}
readUntilTerm ::
   (String -> Parser char ()) -> String -> String -> Parser char ()
readUntilTerm generateTag termWarning termPat =
   do ~(termFound,str) <- readUntil termPat
      generateTag str
      termPos <- getPos
      emitWarningWhen (not termFound) termPos termWarning


class CharType char where
   fromChar :: Char -> char
   parseString  :: (Char -> Bool) -> Parser char [char]
   parseString1 :: (Char -> Bool) -> Parser char [char]
   parseUnquotedValue :: Parser char [char]

instance CharType Char where
   fromChar = id
   parseString  p = liftM concat $ many  (parseChar p)
   parseString1 p = liftM concat $ many1 (parseChar p)
   parseUnquotedValue = parseUnquotedValueChar

instance CharType HTMLChar where
   fromChar = Char
   parseString  p = many  (parseHTMLChar p)
   parseString1 p = many1 (parseHTMLChar p)
   parseUnquotedValue = parseUnquotedValueHTMLChar


parseChar :: (Char -> Bool) -> Parser char String
parseChar p =
   do pos <- getPos
      x <- parseHTMLChar p
      let returnChar c = return $ c:[]
      case x of
         Char c -> returnChar c
         NumericRef num -> returnChar (chr num)
         NamedRef name ->
            maybe
               (let refName = '&':name++";"
                in  emitWarning pos ("Unknown HTML entity " ++ refName) >>
                    return refName)
               (returnChar . chr)
               (lookup name HTMLEntity.table)


parseHTMLChar :: (Char -> Bool) -> Parser char HTMLChar
parseHTMLChar p =
   do pos <- getPos
      c <- satisfy p
      if c=='&'
        then
          force $
          mplus
            (do ent <-
                   mplus
                      (char '#' >>
                       liftM (NumericRef . read) (many1Satisfy isDigit))
                      (liftM NamedRef $ many1Satisfy isAlphaNum)
                char ';'
                return ent)
            (emitWarning pos "Ill formed entity" >>
             return (Char '&'))
        else return (Char c)


emitWarningWhen :: Bool -> Position -> String -> Parser char ()
emitWarningWhen cond pos msg =
   force $ when cond $ emitWarning pos msg

emitWarning :: Position -> String -> Parser char ()
emitWarning pos msg = emitTag pos (TagWarning msg)

emitTag :: Position -> Tag char -> Parser char ()
emitTag = curry emit



-- | This function takes a list, and returns all suffixes whose
--   first item matches the predicate.
sections :: (a -> Bool) -> [a] -> [[a]]
sections p = filter (p . head) . init . tails

-- | This function is similar to 'sections', but splits the list
--   so no element appears in any two partitions.
partitions :: (a -> Bool) -> [a] -> [[a]]
partitions p =
   let notp = not . p
   in  groupBy (const notp) . dropWhile notp
