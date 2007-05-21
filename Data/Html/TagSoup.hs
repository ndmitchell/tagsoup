{-|
    Module      :  Data.Html.TagSoup
    Copyright   :  (c) Neil Mitchell 2006-2007
    License     :  BSD-style

    Maintainer  :  http://www.cs.york.ac.uk/~ndm/
    Stability   :  moving towards stable
    Portability :  portable

    This module is for extracting information out of unstructured HTML code,
    sometimes known as tag-soup. This is for situations where the author of
    the HTML is not cooperating with the person trying to extract the information,
    but is also not trying to hide the information.

    The standard practice is to parse a String to 'Tag's using 'parseTags',
    then operate upon it to extract the necessary information.
-}

module Data.Html.TagSoup(
    -- * Data structures and parsing
    Tag(..), PosTag, Attribute, parseTags, parseTagsNoPos,
    module Data.Html.Download,

    -- * Tag Combinators
    (~==), (~/=),
    TagComparison, TagComparisonElement, {- Haddock want to refer to then -}
    isTagOpen, isTagClose, isTagText, isTagWarning,
    fromTagText, fromAttrib,
    isTagOpenName, isTagCloseName,
    sections, partitions, getTagContent,

    -- * extract all text
    InnerText(..),

    -- * QuickCheck properties
    propSections, propPartitions,
    ) where

import Data.Html.TagSoup.Parser
   (Status(Status),
    char, dropSpaces, eof, force, getPos,
    many, many1, many1Satisfy, manySatisfy, readUntil,
    satisfy, source, string)

import qualified Data.Html.TagSoup.Parser as Parser

import Text.ParserCombinators.Parsec.Pos
          (SourcePos, initialPos)

import Control.Monad.RWS (mplus, msum, evalRWST, gets, tell, when)

import Data.Html.Download

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


type PosTag = (SourcePos,Tag)

type Parser a = Parser.Parser PosTag a



-- | Parse an HTML document to a list of 'Tag'.
-- Automatically expands out escape characters.
parseTags :: String -> [PosTag]
parseTags str =
   snd $
   fromMaybe (error "parseTagPos can never fail.") $
   evalRWST (many parseTagPos)
      () (Status (initialPos "anonymous input") str)

-- | Like 'parseTags' but hides source file positions.
parseTagsNoPos :: String -> [Tag]
parseTagsNoPos = map snd . parseTags


parseTagPos :: Parser ()
parseTagPos = do
   pos <- getPos
   msum $
    (do char '<'
        msum $
         (do char '/'
             name <- manySatisfy isAlphaNum
             emitTag pos (TagClose name)
             dropSpaces
             junkPos <- getPos
             junk <- readUntilTerm
                ("Unterminated closing tag \"" ++ name ++"\"") ">"
             emitWarningWhen
                (not $ null junk)
                junkPos ("Junk in closing tag: \"" ++ junk ++"\"")
         ) :
         (do char '!'
             msum $
              (do string "--"
                  cmt <-
                     readUntilTerm "Unterminated comment" "-->"
                  emitTag pos (TagComment cmt)) :
              (do name <- manySatisfy isAlphaNum
                  dropSpaces
                  info <- readUntilTerm
                     ("Unterminated special tag \"" ++ name ++ "\"") ">"
                  emitTag pos (TagSpecial name info)) :
              []
         ) :
         (do name <- manySatisfy isAlphaNum
             dropSpaces
             attrs <- many parseAttribute
             tell [(pos, TagOpen name attrs)]
             force $ msum $
               (do closePos <- getPos
                   string "/>"
                   emitTag closePos (TagClose name)) :
               (do junkPos <- getPos
                   junk <- readUntilTerm
                      ("Unterminated open tag \"" ++ name ++ "\"") ">"
                   emitWarningWhen
                      (not $ null junk)
                      junkPos ("Junk in opening tag: \"" ++ junk ++"\"")) :
               []
         ) :
         []
    ) :
    (do text <- parseString1 ('<'/=)
        emitTag pos (TagText text)
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
      (do pos <- getPos
          str <- parseString (not . flip elem " >")
          -- maybe this introduces a space leak for long values
          -- 'nub' will run too slowly
          let wrong = filter (\c -> not (isAlphaNum c || c `elem` "_-")) str
          emitWarningWhen
             (not (null wrong))
             pos $ "Illegal characters in unquoted value: " ++ wrong
          return str) :
      []

parseQuoted :: String -> Char -> Parser String
parseQuoted termMsg quote =
   do char quote
      str <- parseString (quote/=)
      mplus
         (char quote >> return str)
         (do termPos <- getPos
             emitWarning termPos termMsg
             return str)

readUntilTerm :: String -> String -> Parser String
readUntilTerm warning termPat =
   do ~(termFound,str) <- readUntil termPat
      termPos <- getPos
      emitWarningWhen (not termFound) termPos warning
      return str



escapes :: [(String,Char)]
escapes = [("gt",'>')
          ,("lt",'<')
          ,("amp",'&')
          ,("quot",'\"')
          ]


parseString :: (Char -> Bool) -> Parser String
parseString p =
   fmap concat $ many (parseChar p)

parseString1 :: (Char -> Bool) -> Parser String
parseString1 p =
   fmap concat $ many1 (parseChar p)

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
      Right
      (lookup name escapes)


emitWarningWhen :: Bool -> SourcePos -> String -> Parser ()
emitWarningWhen cond pos msg =
   force $ when cond $ emitWarning pos msg

emitWarning :: SourcePos -> String -> Parser ()
emitWarning pos msg = emitTag pos (TagWarning msg)

emitTag :: SourcePos -> Tag -> Parser ()
emitTag pos tag = tell [(pos, tag)]


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
              fst $
              fromMaybe (error "tagEqualElement: parse should never fail") $
              evalRWST
                 (do dropSpaces
                     attrs <- many parseAttribute
                     isEOF <- eof
                     if isEOF
                       then return attrs
                       else fmap (error . ("trailing characters " ++))
                                 (gets source))
                 () (Status (initialPos "attribute string") attrStr)
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
