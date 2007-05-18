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
    addSourcePositions,
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

import Data.Char
import Data.List
import Data.Maybe
import Data.Html.Download

import Control.Monad (mplus)

import Text.ParserCombinators.Parsec.Pos
          (SourcePos, initialPos, updatePosChar)


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


type PosString p = [(p, Char)]
type PosTag    p = (p,Tag)


-- | Parse an HTML document to a list of 'Tag'.
-- Automatically expands out escape characters.
parseTags :: String -> [PosTag SourcePos]
parseTags = parseTagPos . addSourcePositions

-- | Like 'parseTags' but hides source file positions.
parseTagsNoPos :: String -> [Tag]
parseTagsNoPos = map snd . parseTags

parseTagPos :: PosString p -> [PosTag p]

parseTagPos ((pos,'<'):inner) =
   case inner of
      (_,'/'):xs ->
         let (name,rest) = spanStripPos isAlphaNum xs
             rest2 = dropSpaces rest
             trail =
                case rest2 of
                   (_,'>'):rest3 -> parseTagPos rest3
                   _ ->
                     parseToEndTag ">"
                        (\junk ->
                           junkWarning rest2
                              ("Junk in closing tag: \"" ++ junk ++"\""))
                        (pos,
                         TagWarning ("Unterminated closing tag \"" ++ name ++"\""))
                        rest2
         in  (pos, TagClose name) : trail
      (_,'!'):xs ->
         case xs of
            (_,'-'):(_,'-'):rest ->
               parseToEndTag "-->"
                  (\cmt -> [(pos, TagComment cmt)])
                  (pos, TagWarning "Unterminated comment")
                  rest
            _ ->
               let (name,rest) = spanStripPos isAlphaNum xs
               in  parseToEndTag ">"
                      (\info -> [(pos, TagSpecial name info)])
                      (pos, TagWarning ("Unterminated special tag \"" ++ name ++ "\""))
                      (dropSpaces rest)
      xs ->
         let (name,rest) = spanStripPos isAlphaNum xs
             ((attrs,warnings),rest2) = parseAttributes (dropSpaces rest)
             maybeProperTrail =
                mplus
                   (fmap (((fst (head rest2), TagClose name) :) . parseTagPos)
                         (splitPrefix "/>" rest2))
                   (fmap parseTagPos (splitPrefix ">" rest2))
             trail =
                fromMaybe
                   (parseToEndTag ">"
                      (\junk ->
                         junkWarning rest2
                            ("Junk in opening tag: \"" ++ junk ++"\""))
                      (pos,
                       TagWarning ("Unterminated opening tag \"" ++ name ++"\""))
                      rest2)
                   maybeProperTrail
             warningTags = map (\(p,msg) -> (p, TagWarning msg)) warnings
         in  (pos, TagOpen name attrs) :  warningTags ++ trail

parseTagPos [] = []
parseTagPos xs =
   [(fst (head xs), TagText $ parseString pre) | not $ null pre] ++
       parseTagPos post
    where (pre,post) = spanStripPos ('<'/=) xs

junkWarning :: PosString p -> String -> [PosTag p]
junkWarning ((p,_):_) warning = [(p, TagWarning warning)]
junkWarning _ _ = []

parseToEndTag ::
   String ->
   (String -> [PosTag p]) ->
   PosTag p ->
   PosString p -> [PosTag p]
parseToEndTag pattern handleContent handleFailure text =
   let (parsed,rest) = searchSplit pattern text
   in  handleContent parsed ++
          maybe [handleFailure] parseTagPos rest


parseAttributes :: PosString p -> (([Attribute], [(p,String)]), PosString p)
parseAttributes xt@((_,x):_) =
   if not $ isAlpha x
     then (([], []), xt)
     else
        let (lhs,rest) = spanStripPos isAlphaNum xt
            rest2 = dropSpaces rest
            ((rhs,warning), other) =
                maybe
                  (("", Nothing), rest2)
                  (parseValue . dropSpaces)
                  (splitPrefix "=" rest2)
            ((attrs,warnings), over) = parseAttributes (dropSpaces other)
        in  (((lhs, parseString rhs):attrs,
              maybeToList warning ++ warnings), over)
parseAttributes [] =
   (([], [(error "end of file", "parse attributes: unexpected end of input")]),
    [])


parseValue :: PosString p -> ((String, Maybe (p,String)), PosString p)
parseValue ((pos,'\"'):xs) =
   let (str,maybeRest) = searchSplit "\"" xs
       (warning,rest) =
          maybe
             (Just (pos, "unterminated value string"), [])
             (\rest1 -> (Nothing, rest1))
             maybeRest
   in  ((str,warning),rest)
parseValue x =
   let isValid c = isAlphaNum c || c `elem` "_-"
       (value,rest) = spanStripPos isValid x
   in  ((value,Nothing), rest)



escapes :: [(String,Char)]
escapes = [("gt",'>')
          ,("lt",'<')
          ,("amp",'&')
          ,("quot",'\"')
          ]


parseEscape :: String -> Maybe Char
parseEscape ('#':xs) = toMaybe (all isDigit xs) (chr $ read xs)
parseEscape xs = lookup xs escapes



parseString :: String -> String
parseString ('&':xs) =
     case parseEscape a of
        Nothing -> '&' : parseString xs
        Just x -> x : parseString (drop 1 b)
    where (a,b) = break (== ';') xs
parseString (x:xs) = x : parseString xs
parseString [] = []


-- cf. Text.ParserCombinators.Parsec.Pos.SourcePos
addSourcePositions :: String -> [(SourcePos,Char)]
addSourcePositions str =
   zip (scanl updatePosChar (initialPos "anonymous input") str) str

dropSpaces :: PosString p -> PosString p
dropSpaces = dropWhile (isSpace . snd)

-- | like 'Data.List.span' but it ignores source positions
spanPos :: (a -> Bool) -> [(i,a)] -> ([(i,a)],[(i,a)])
spanPos p = span (p . snd)

spanStripPos :: (a -> Bool) -> [(i,a)] -> ([a],[(i,a)])
spanStripPos p xs =
   let (ys,zs) = spanPos p xs
   in  (map snd ys, zs)

splitPrefix :: (Eq a) => [a] -> [(i,a)] -> Maybe [(i,a)]
splitPrefix pattern str =
   toMaybe
      (isPrefixOf pattern (map snd str))
      (dropMatch pattern str)


toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

dropMatch :: [b] -> [a] -> [a]
dropMatch (_:_) [] = []
dropMatch (_:xs) (_:ys) = dropMatch xs ys
dropMatch [] ys = ys

searchSplit :: (Eq a) => [a] -> [(i,a)] -> ([a], Maybe [(i,a)])
searchSplit pattern xs =
   {- We take care that the input characters can be garbaged collected
      immediately after they are read. -}
   let (suffixes,rest) =
            break (isPrefixOf pattern . map snd) (init $ tails $ xs)
       prefix = map (snd . head) suffixes
       trail =
          case rest of
             (rest2:_) -> Just $ dropMatch pattern $ rest2
             _ -> Nothing
   in  (prefix, trail)



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
       let (name, attrs) = span (/= ' ') tagname
           parsed_attrs =
              case parseAttributes (map ((,) ()) (attrs ++ ">")) of
                 ((found_attrs, _), [(_,'>')]) -> found_attrs
                 (_, trailing) -> error $ "trailing characters " ++ map snd trailing
       in  a ~== TagOpen name parsed_attrs


instance TagComparisonElement a => TagComparison [a] where
  (~==) = tagEqualElement


-- | This function takes a list, and returns all suffixes whose
--   first item matches the predicate.
sections :: (a -> Bool) -> [a] -> [[a]]
sections p = filter (p . head) . init . tails

sections_rec :: (a -> Bool) -> [a] -> [[a]]
sections_rec _ [] = []
sections_rec f (x:xs) = [x:xs | f x] ++ sections f xs

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
