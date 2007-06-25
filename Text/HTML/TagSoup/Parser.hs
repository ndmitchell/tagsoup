
module Text.HTML.TagSoup.Parser(parseTags) where

import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Position
import Control.Monad.State
import Data.Char
import Data.List


---------------------------------------------------------------------
-- * Driver

parseTags :: CharType char => String -> [Tag char]
parseTags x = mergeChars $ parse (initialize "",x)
    where
        -- should be more lazy
        mergeChars (TagText x:TagText y:xs) = mergeChars $ TagText (x++y) : xs
        mergeChars (x:xs) = x : mergeChars xs
        mergeChars [] = []


---------------------------------------------------------------------
-- * Combinators


type PosStr = (Position,String)

parseUntil :: (String -> Bool) -> PosStr -> (String,PosStr)
parseUntil test (p,s) | null s || test s = ("",(p,s))
parseUntil test (p,s:ss) = (s:a,b)
    where (a,b) = parseUntil test (p,ss)


parseEnd :: CharType char => String -> String -> PosStr -> (String,[Tag char],PosStr)
parseEnd msg end ps = (a,b,c)
    where
        (a,(p,s)) = parseUntil (end `isPrefixOf`) ps
        
        (b,c) = case s of
                    "" -> ([TagWarning p $ "Unexpected end, looking for " ++ show end ++ " in " ++ msg], (p,s))
                    _ -> ([], (p += end, drop (length end) s))


parseSpan :: (Char -> Bool) -> PosStr -> (String, PosStr)
parseSpan test = parseUntil (test . head)


dropSpaces :: PosStr -> PosStr
dropSpaces = snd . parseSpan isSpace


parseName :: PosStr -> (String,PosStr)
parseName = parseSpan isNameChar

isNameChar x = isAlphaNum x || x `elem` "-_:"


(+=) = updateOnString


-- In text mode we are looking for
--
-- if we see '</' that must be a close tag
-- if we see '<!' that must be a special tag
-- if we see '<' then anything else that must be an open tag
-- if we see '&' then we want an entity

parse :: CharType char => PosStr -> [Tag char]
parse (p, '<':'!':'-':'-':xs) = TagComment a : errs ++ parse b
    where
        (a,errs,b) = parseEnd "comment" "-->" (p += "<!--", xs)

parse (p, '<':'!':xs) = TagSpecial name contents : errs ++ parse ps2
    where
        (name,ps1) = parseName (p += "<!", xs)
        (contents,errs,ps2) = parseEnd "special" ">" $ dropSpaces ps1

parse (p, '<':'/':xs) = TagClose name : errs ++ parse ps3
    where
        (name,ps1) = parseName (p += "</", xs)
        (p2,s2) = dropSpaces ps1
        (errs,ps3) = case s2 of
                         '>':xs -> ([],(p2 += ">",xs))
                         [] -> ([TagWarning p2 "Unexpected end, looking for > in closing tag"],(p2,s2))
                         _:xs -> let ~(a,b,c) = parseEnd "closing tag" ">" (p2,s2)
                                 in (TagWarning p2 "Unexpected junk in closing tag" : b, c)

parse (p, '<':xs) = TagOpen name attrs : errs ++ [TagClose name | shut] ++ parse ps2
    where
        (name,ps1) = parseName (p += "<", xs)
        (attrs,errs,shut,ps2) = parseAttribs ps1

parse (p, '&':xs) = TagText c : errs : parse ps
    where
        (c,errs,ps) = parseEntity (p += "&", xs)

parse (p, x:xs) = TagText [fromHTMLChar $ Char x] : parse (p += [x], xs)
parse (p, []) = []



parseAttribs :: CharType char => PosStr -> ([Attribute char],[Tag char],Bool,PosStr)
parseAttribs ps =
    case dropSpaces ps of
        (p,'/':'>':xs) -> ([],[],True,(p += "/>",xs))
        (p,'>':xs) -> ([],[],False,(p += ">",xs))
        ps2 -> let ~(a,b,c) = parseAttrib ps2
                   ~(d,e,f,g) = parseAttribs c
               in (a:d,b++e,f,g)


parseAttrib :: CharType char => PosStr -> (Attribute char,[Tag char],PosStr)
parseAttrib ps = ((name,val2),[TagWarning (fst ps) "Blank attribute" | null name] ++ errs2 ++ errs,ps3)
    where
        (name,ps2) = parseName ps
        (val2,errs2) = fixupEntities ps2 val
        (val,errs,ps3) = case dropSpaces ps2 of
                             (p,'=':s) -> case dropSpaces (p += "=",s) of
                                (p,q:s) | q `elem` "'\"" -> parseEnd "string" [q] (p += [q],s)
                                ps -> let ~(a,b) = parseName ps
                                      in (a,[],b)
                             ps -> ([],[],ps)


fixupEntities :: CharType char => PosStr -> String -> ([char],[Tag char])
fixupEntities = undefined
{-
fixupEntities ps ('&':xs) = (a,b
    where (a,b,c) = parseEntity ps xs
-}

parseEntity = undefined

{-

parse (p0, '<':'!':xs) =
        parseSpaceName (parseEnd "special tag" ">" . cont) (p0 += "<!", xs)
    where
        cont name special rest = TagSpecial name special ++
            [TagWarning p0 "Blank name in special tag" | null name] ++
            parse rest
-}


-- parse p ('<':'/':xs) = parseName (updateOnString "</") p xs 

{-

parse p ('<':xs)

parse p ('&':xs) = 

-}

{-

= do
    c <- getc
    case c of
        Nothing -> return []
        Just '&' -> parseEntity
        Just '<' -> do
            c <- getc
            case c of
                Just '!' -> parseSpecial
                Just '/' -> parseCloseTag
                Just c -> parseOpenTag c
                Nothing -> do
                    p <- getp
                    let warn = TagWarning p "Unexpected end of file when reading '<'"
                    return [warn, TagText [Char '<']]




parseOpenTag :: Char -> Parser [Tag HTMLChar]
parseOpenTag c = undefined


parseCloseTag = undefined

parseSpecial = undefined


parseEntity = undefined
-}
{-


do
    c <- dropSpaces c

    do


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

--Instead of using 'generateTag' we could also wrap the call to 'readUntilTerm'
--in 'mfix' in order to emit a tag, where some information is read later.
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

-}
