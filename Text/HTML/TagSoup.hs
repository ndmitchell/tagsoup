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
    Tag(..), PosTag, Attribute,
    parseTags, parsePosTags, parseFilePosTags,
    canonicalizeTags, canonicalizePosTags,

    -- * Tag Combinators
    (~==), (~/=),
    TagComparison, TagComparisonElement, {- Haddock want to refer to then -}
    isTagOpen, isTagClose, isTagText, isTagWarning,
    fromTagText, fromAttrib,
    maybeTagText, maybeTagWarning,
    isTagOpenName, isTagCloseName,
    sections, partitions, getTagContent,

    -- * extract all text
    InnerText(..),

    -- * QuickCheck properties
    propSections, propPartitions,
    ) where

import Text.HTML.TagSoup.Parser
   (char, dropSpaces, eof, force, getPos,
    many, many1, many1Satisfy, manySatisfy, readUntil,
    satisfy, source, string,
    emit, mfix, gets)

import qualified Text.HTML.TagSoup.Parser as Parser
import qualified Text.HTML.TagSoup.Entity as HTMLEntity

import Text.HTML.TagSoup.Position (Position)

import Control.Monad (mplus, msum, when, liftM)

import Data.Char
import Data.List
import Data.Maybe


-- | An HTML attribute @id=\"name\"@ generates @(\"id\",\"name\")@
type Attribute = (String,String)

-- | An HTML element, a document is @[Tag]@.
--   There is no requirement for 'TagOpen' and 'TagClose' to match
data Tag =
     TagOpen String [Attribute]  -- ^ An open tag with 'Attribute's in their original order.
   | TagClose String             -- ^ A closing tag
   | TagText String              -- ^ A text node, guranteed not to be the empty string
   | TagComment String           -- ^ A comment
   | TagSpecial String String    -- ^ A tag like <!DOCTYPE ...>
   | TagWarning String           -- ^ Mark a syntax error in the input file
     deriving (Show, Eq, Ord)


type PosTag = (Position,Tag)

type Parser a = Parser.Parser PosTag a

{- |
Turns all tag names to lower case and
converts DOCTYPE to upper case.
-}
canonicalizePosTags :: [PosTag] -> [PosTag]
canonicalizePosTags =
   map (\(i,tag) -> (i, canonicalizeTag tag))

canonicalizeTags :: [Tag] -> [Tag]
canonicalizeTags =
   map canonicalizeTag

canonicalizeTag :: Tag -> Tag
canonicalizeTag t =
   case t of
      TagOpen  name attrs  -> TagOpen  (map toLower name) attrs
      TagClose name        -> TagClose (map toLower name)
      TagSpecial name info -> TagSpecial (map toUpper name) info
      _ -> t



parseFilePosTags :: FilePath -> String -> [PosTag]
parseFilePosTags fileName =
   fromMaybe (error "parsePosTag can never fail.") .
   Parser.write fileName (many parsePosTag >> return ())


-- | Parse an HTML document to a list of 'Tag'.
-- Automatically expands out escape characters.
parsePosTags :: String -> [PosTag]
parsePosTags =
   fromMaybe (error "parsePosTag can never fail.") .
   Parser.write "input" (many parsePosTag >> return ())

-- | Like 'parsePosTags' but hides source file positions.
parseTags :: String -> [Tag]
parseTags = map snd . parsePosTags


parsePosTag :: Parser ()
parsePosTag = do
   pos <- getPos
   msum $
    (do char '<'
        msum $
         (do char '/'
             name <- manySatisfy isAlphaNum
             emitTag pos (TagClose name)
             dropSpaces
             junkPos <- getPos
             readUntilTerm
                (\ junk ->
                   emitWarningWhen
                      (not $ null junk)
                      junkPos ("Junk in closing tag: \"" ++ junk ++"\""))
                ("Unterminated closing tag \"" ++ name ++"\"") ">"
         ) :
         (do char '!'
             msum $
              (do string "--"
                  readUntilTerm
                     (\ cmt -> emitTag pos (TagComment cmt))
                     "Unterminated comment" "-->") :
              (do name <- manySatisfy isAlphaNum
                  dropSpaces
                  readUntilTerm
                     (\ info -> emitTag pos (TagSpecial name info))
                     ("Unterminated special tag \"" ++ name ++ "\"") ">") :
              []
         ) :
         (do name <- manySatisfy isAlphaNum
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
         ) :
         []
    ) :
    (mfix
       (\ text ->
          emitTag pos (TagText text) >>
          parseString1 ('<'/=))
       >> return ()
    ) :
    []



parseAttribute :: Parser Attribute
parseAttribute =
   do name <- many1Satisfy (\c -> isAlpha c || c `elem` "_-:")
      dropSpaces
      value <-
         force $
         mplus
            (string "=" >> dropSpaces >> parseValue)
            (return "")
      dropSpaces
      return (name, value)


parseValue :: Parser String
parseValue =
   force $ msum $
      parseQuoted "Unterminated doubly quoted value string" '"' :
      parseQuoted "Unterminated singly quoted value string" '\'' :
      (let parseValueChar =
              do str <- parseChar (not . flip elem " >\"\'")
                 let wrong =
                       filter (\c -> not (isAlphaNum c || c `elem` "_-")) str
                 pos <- getPos
                 emitWarningWhen
                    (not (null wrong))
                    pos $ "Illegal characters in unquoted value: " ++ wrong
                 return str
       in  liftM concat $ many parseValueChar) :
      []

parseQuoted :: String -> Char -> Parser String
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
readUntilTerm :: (String -> Parser ()) -> String -> String -> Parser ()
readUntilTerm generateTag termWarning termPat =
   do ~(termFound,str) <- readUntil termPat
      generateTag str
      termPos <- getPos
      emitWarningWhen (not termFound) termPos termWarning



parseString :: (Char -> Bool) -> Parser String
parseString p =
   liftM concat $ many (parseChar p)

parseString1 :: (Char -> Bool) -> Parser String
parseString1 p =
   liftM concat $ many1 (parseChar p)

parseChar :: (Char -> Bool) -> Parser String
parseChar p =
   do pos <- getPos
      c <- satisfy p
      if c=='&'
        then
          force $
          mplus
            (do ent <-
                   mplus
                      (do char '#'
                          numStr <- many1Satisfy isDigit
                          return [parseNumericEntity numStr])
                      (do nameStr <- many1Satisfy isAlphaNum
                          either
                             (\msg -> emitWarning pos msg >>
                                      return ('&':nameStr++";"))
                             (\ch -> return [ch]) $
                             parseNamedEntity nameStr)
                char ';'
                return ent)
            (emitWarning pos "Ill formed entity" >>
             return "&")
        else return (c:[])

parseNumericEntity :: String -> Char
parseNumericEntity = chr . read

parseNamedEntity :: String -> Either String Char
parseNamedEntity name =
   maybe
      (Left $ "Unknown HTML entity &" ++ name ++ ";")
      (Right . chr)
      (lookup name HTMLEntity.table)

emitWarningWhen :: Bool -> Position -> String -> Parser ()
emitWarningWhen cond pos msg =
   force $ when cond $ emitWarning pos msg

emitWarning :: Position -> String -> Parser ()
emitWarning pos msg = emitTag pos (TagWarning msg)

emitTag :: Position -> Tag -> Parser ()
emitTag = curry emit


infixr 5 ?:

(?:) :: (Bool, a) -> [a] -> [a]
(?:) (True,  x) xs = x:xs
(?:) (False, _) xs = xs


-- | Extract all text content from tags (similar to Verbatim found in HaXml)
class InnerText a where
    innerText :: a -> String

instance InnerText Tag where
    innerText = fromMaybe "" . maybeTagText

instance (InnerText a) => InnerText [a] where
    innerText = concatMap innerText


-- | Test if a 'Tag' is a 'TagOpen'
isTagOpen :: Tag -> Bool
isTagOpen (TagOpen {})  = True; isTagOpen  _ = False

-- | Test if a 'Tag' is a 'TagClose'
isTagClose :: Tag -> Bool
isTagClose (TagClose {}) = True; isTagClose _ = False

-- | Test if a 'Tag' is a 'TagText'
isTagText :: Tag -> Bool
isTagText (TagText {})  = True; isTagText  _ = False

-- | Extract the string from within 'TagText', otherwise 'Nothing'
maybeTagText :: Tag -> Maybe String
maybeTagText (TagText x) = Just x
maybeTagText _ = Nothing

{-# DEPRECIATED fromTagText #-}
-- | Extract the string from within 'TagText', crashes if not a 'TagText'
--   (DEPRECIATED, use 'innerText' instead)
fromTagText :: Tag -> String
fromTagText (TagText x) = x
fromTagText x = error ("(" ++ show x ++ ") is not a TagText")

-- | Test if a 'Tag' is a 'TagWarning'
isTagWarning :: Tag -> Bool
isTagWarning (TagWarning {})  = True; isTagWarning _ = False

-- | Extract the string from within 'TagWarning', otherwise 'Nothing'
maybeTagWarning :: Tag -> Maybe String
maybeTagWarning (TagWarning x) = Just x
maybeTagWarning _ = Nothing

-- | Extract an attribute, crashes if not a 'TagOpen'.
--   Returns @\"\"@ if no attribute present.
fromAttrib :: String -> Tag -> String
fromAttrib att (TagOpen _ atts) = fromMaybe "" $ lookup att atts
fromAttrib _ x = error ("(" ++ show x ++ ") is not a TagOpen")

-- | Returns True if the 'Tag' is 'TagOpen' and matches the given name
{-# DEPRECATED isTagOpenName "use ~== \'tagname\" instead" #-}
-- useg (~== "tagname") instead
isTagOpenName :: String -> Tag -> Bool
isTagOpenName name (TagOpen n _) = n == name
isTagOpenName _ _ = False

-- | Returns True if the 'Tag' is 'TagClose' and matches the given name
isTagCloseName :: String -> Tag -> Bool
isTagCloseName name (TagClose n) = n == name
isTagCloseName _ _ = False


-- | Performs an inexact match, the first item should be the thing to match.
-- If the second item is a blank string, that is considered to match anything.
-- ( See "Example\/Example.hs" function tests for some examples

class TagComparison a where
  (~==), (~/=) :: Tag -> a -> Bool
  a ~== b = not (a ~/= b)
  -- | Negation of '~=='
  a ~/= b = not (a ~== b)

instance TagComparison Tag where
  -- For 'TagOpen' missing attributes on the right are allowed.
  (TagText y)    ~== (TagText x)    = null x || x == y
  (TagClose y)   ~== (TagClose x)   = null x || x == y
  (TagOpen y ys) ~== (TagOpen x xs) = (null x || x == y) && all f xs
      where
         f ("",val) = val `elem` map snd ys
         f (name,"") = name `elem` map fst ys
         f nameval = nameval `elem` ys
  _ ~== _ = False

-- | This is a helper class for instantiating TagComparison for Strings

class TagComparisonElement a where
  tagEqualElement :: Tag -> [a] -> Bool

instance TagComparisonElement Char where
  tagEqualElement a ('/':tagname) = a ~== TagClose tagname
  tagEqualElement a tagname =
       let (name, attrStr) = span (/= ' ') tagname
           parsed_attrs =
              fromMaybe (error "tagEqualElement: parse should never fail") $
              Parser.eval "input"
                 (do dropSpaces
                     attrs <- many parseAttribute
                     isEOF <- eof
                     if isEOF
                       then return attrs
                       else liftM (error . ("trailing characters " ++))
                                  (gets source))
                 attrStr
       in  a ~== TagOpen name parsed_attrs


instance TagComparisonElement a => TagComparison [a] where
  (~==) = tagEqualElement


-- | This function takes a list, and returns all suffixes whose
--   first item matches the predicate.
sections :: (a -> Bool) -> [a] -> [[a]]
sections p = filter (p . head) . init . tails

sections_rec :: (a -> Bool) -> [a] -> [[a]]
sections_rec f =
   let recurse [] = []
       recurse (x:xs) = (f x, x:xs) ?: recurse xs
   in  recurse

propSections :: [Int] -> Bool
propSections xs  =  sections (<=0) xs == sections_rec (<=0) xs

-- | This function is similar to 'sections', but splits the list
--   so no element appears in any two partitions.
partitions :: (a -> Bool) -> [a] -> [[a]]
partitions p =
   let notp = not . p
   in  groupBy (const notp) . dropWhile notp

partitions_rec :: (a -> Bool) -> [a] -> [[a]]
partitions_rec f = g . dropWhile (not . f)
    where
        g [] = []
        g (x:xs) = (x:a) : g b
            where (a,b) = break f xs

propPartitions :: [Int] -> Bool
propPartitions xs  =  partitions (<=0) xs == partitions_rec (<=0) xs

getTagContent :: String -> [( String, String )] -> [Tag] -> [Tag]
getTagContent name attr tagsoup =
   let start = sections ( ~== TagOpen name attr ) tagsoup !! 0
   in takeWhile (/= TagClose name) $ drop 1 start
