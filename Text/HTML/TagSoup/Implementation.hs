
module Text.HTML.TagSoup.Implementation where

import Data.List
import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Options
import Text.StringLike as Str
import Numeric
import Data.Char

---------------------------------------------------------------------
-- BOTTOM LAYER

data Out
    = Char Char
    | Tag          -- <
    | TagShut      -- </
    | AttName
    | AttVal
    | TagEnd       -- >
    | TagEndClose  -- />
    | Comment      -- <!--
    | CommentEnd   -- -->
    | Entity       -- &
    | EntityNum    -- &#
    | EntityHex    -- &#x
    | EntityEnd    -- ;
    | EntityEndAtt -- missing the ; and in an attribute
    | Warn String
    | Pos Position
      deriving (Show,Eq)

-- Entering a Tag output, delay all Warn/Pos tags until afterwards
enter x = x `elem` [Tag,TagShut,Comment,Entity,EntityNum,EntityHex]
leave x = x `elem` [TagEnd,TagEndClose,CommentEnd,EntityEnd,EntityEndAtt]


errSeen x = Warn $ "Unexpected " ++ show x
errWant x = Warn $ "Expected " ++ show x

data S = S
    {s :: S
    ,tl :: S
    ,hd :: Char
    ,eof :: Bool
    ,next :: String -> Maybe S
    ,pos :: [Out] -> [Out]
    }


expand :: Position -> String -> S
expand p text = res
    where res = S{s = res
                 ,tl = expand (positionChar p (head text)) (tail text)
                 ,hd = if null text then '\0' else head text
                 ,eof = null text
                 ,next = next p text
                 ,pos = (Pos p:)
                 }

          next p (t:ext) (s:tr) | t == s = next (positionChar p t) ext tr
          next p text [] = Just $ expand p text
          next _ _ _ = Nothing

infixr &
(&) :: Outable a => a -> [Out] -> [Out]
(&) x xs = outable x : xs

class Outable a where outable :: a -> Out
instance Outable Char where outable = Char
instance Outable Out where outable = id


state :: String -> S
state s = expand nullPosition s

---------------------------------------------------------------------
-- MIDDLE LAYER

data Result str
    = RText str (Result str)
    | RTagOpen str (Result str)
    | RTagShut str (Result str)
    | RAttName str (Result str)
    | RAttVal str (Result str)
    | RTagEnd (Result str)
    | RTagEndClose (Result str)
    | RComment str (Result str)
    | REntity str Bool (Result str) -- Bool is does it have a closing ;
    | REntityChar Int (Result str)
    | RWarn str (Result str)
    | RPos !Row !Column (Result str) -- only beore RText, RTagOpen, RTagShut, RComment, REntity, REntityChar and RWarn
    | REof
      deriving Show

rtail :: Result str -> Result str
rtail (RText _ r) = r
rtail (RTagOpen _ r) = r
rtail (RTagShut _ r) = r
rtail (RAttName _ r) = r
rtail (RAttVal _ r) = r
rtail (RTagEnd r) = r
rtail (RTagEndClose r) = r
rtail (RComment _ r) = r
rtail (REntity _ _ r) = r
rtail (REntityChar _ r) = r
rtail (RWarn _ r) = r
rtail (RPos _ _ r) = r



-- filter out warning/pos if they are not wanted
output :: ParseOptions String -> [Out] -> Result String
output opts = output2 Nothing . (if warn || pos then delay else id) . filter f
    where (warn,pos) = (optTagWarning opts,optTagPosition opts)
          f Warn{} = warn
          f Pos{} = pos
          f _ = True


-- move all Warn/Pos nodes after enter/leave pairs
delay :: [Out] -> [Out]
delay = f
    where
        f (x:xs) | enter x = x : g [] xs
        f (x:xs) = x : f xs
        f [] = []

        g acc (x:xs) | leave x = reverse acc ++ x : f xs
        g acc (x@Warn{}:xs) = g (x:acc) xs
        g acc (x@Pos{}:xs) = g (x:acc) xs
        g acc (x:xs) = x : g acc xs
        g acc [] = reverse acc


output2 :: Maybe Position -> [Out] -> Result String
output2 p [] = REof
output2 p (Entity:xs) = outputEntity p REntity xs
output2 p (EntityNum:xs) = outputEntity p (\x y -> REntityChar (read x)) xs
output2 p (EntityHex:xs) = outputEntity p (\x y -> REntityChar (fst $ head $ readHex x)) xs
output2 p (TagShut:xs) = let (a,b) = readChars xs in outputPos p $ RTagShut a (output2 p b)
output2 p (Tag:xs) = let (a,b) = readChars xs in outputPos p $ RTagOpen a (output2 p b)
output2 p (TagEnd:xs) = RTagEnd (output2 p xs)
output2 p (TagEndClose:xs) = RTagEndClose (output2 p xs)
output2 p (AttName:xs) = let (a,b) = readChars xs in RAttName a (output2 p b)
output2 p (AttVal:xs) = let (a,b) = readChars xs in RAttVal a (output2 p b)
output2 p (Comment:xs) = let (a,b) = readChars xs in outputPos p $ RComment a (output2 p b)
output2 p (CommentEnd:xs) = output2 p xs
output2 p (Warn x:xs) = outputPos p $ RWarn x (output2 p xs)
output2 p (Pos x:xs) = output2 (Just x) xs

output2 p (Char x:xs) = outputPos p $ RText (x:a) $ output2 p b
    where (a,b) = readChars xs

output2 p xs = error $ "output: " ++ show (take 10 xs)


outputPos Nothing x = x
outputPos (Just (Position a b)) x = RPos a b x

outputEntity :: Maybe Position -> (String -> Bool -> Result String -> Result String) -> [Out] -> Result String
outputEntity p f xs = outputPos p $ f a c (output2 p d)
    where (a,b) = readChars xs
          (c,d) = readEntityEnd b


readChars :: [Out] -> (String,[Out])
readChars (Char x:xs) = (x:a,b)
    where (a,b) = readChars xs
readChars xs = ("",xs)


-- True if EntityEnd, False is EntityEndAtt
readEntityEnd :: [Out] -> (Bool,[Out])
readEntityEnd (EntityEnd:xs) = (True,xs)
readEntityEnd (EntityEndAtt:xs) = (False,xs)
readEntityEnd (x:xs) = (a,x:b) -- x should be pos or warn
    where (a,b) = readEntityEnd xs


---------------------------------------------------------------------
-- TOP LAYER

-- assume that warning/pos tags are there through choice, so don't look at options
result :: StringLike str => ParseOptions str -> Result str -> [Tag str]
result opts = (if optTagTextMerge opts then tagTextMerge else id) . result2 opts


-- thing on the left is user generated, so may have unwanted pos/warning
resultAdd :: StringLike str => ParseOptions str -> [Tag str] -> [Tag str] -> [Tag str]
resultAdd opts (x@TagWarning{}:xs) ys | not $ optTagWarning opts = resultAdd opts xs ys 
resultAdd opts (x@TagPosition{}:xs) ys | not $ optTagPosition opts = resultAdd opts xs ys
resultAdd opts (x:xs) ys = x : resultAdd opts xs ys
resultAdd opts [] ys = ys


result2 :: StringLike str => ParseOptions str -> Result str -> [Tag str]
result2 opts REof = []
result2 opts (RText x r) = TagText x : result2 opts r
result2 opts (RWarn x r) = TagWarning x : result2 opts r
result2 opts (RPos x y r) = TagPosition x y : result2 opts r
result2 opts (RComment x r) = TagComment x : result2 opts r
result2 opts (REntity x y r) = resultAdd opts (optEntityData opts x) (result2 opts r)
result2 opts (REntityChar x r) = TagText (fromString1 $ chr x) : result2 opts r

result2 opts (RTagShut x r) = TagClose x : (if optTagWarning opts then g else f) r
    where f (RTagEnd r) = result2 opts r
          f (RTagEndClose r) = result2 opts r
          f (RPos a b (RWarn x r)) = TagPosition a b : TagWarning x : f r
          f (RWarn x r) = TagWarning x : f r
          f REof = []
          f x = f $ rtail x
          
          g (RTagEnd r) = result2 opts r
          g (RTagEndClose r) = tagWarning "Shut tag with self closing" : result2 opts r
          g (RPos a b (RWarn x r)) = TagPosition a b : TagWarning x : f r
          g (RWarn x r) = TagWarning x : g r
          g REof = []
          g x = tagWarning "Junk in closing tag" : f (rtail x)

result2 opts (RTagOpen x r) = TagOpen x atts : rest
    where (atts,rest) = f r
          f (RTagEnd r) = ([], result2 opts r)
          f (RTagEndClose r) = ([], TagClose x : result2 opts r)
          f (RPos x y (RWarn z r)) = (a, TagPosition x y : TagWarning z: b)
              where (a,b) = f r
          f (RWarn x r) = (a, TagWarning x : b)
              where (a,b) = f r
          f (RPos x y r) = f r
          f (RAttVal x r) = ((empty,a):b, c)
              where (a,b,c) = h x r
          f (RAttName x r) = ((x,a):b, c)
              where (a,b,c) = g r
          f x = ([], result2 opts x)

          g (RAttVal x r) = h x r
          g (RPos x y (RWarn z r)) = (a, b, TagPosition x y : TagWarning z : c)
              where (a,b,c) = g r
          g (RWarn x r) = (a, b, TagWarning x : c)
              where (a,b,c) = g r
          g (RPos x y r) = g r
          g x = (empty,a,b)
              where (a,b) = f x

          h s r = (strConcat $ s:a, b, c)
              where (a,b,c) = h2 r
          h2 (RPos x y (RWarn z r)) = (a, b, TagPosition x y : TagWarning z : c)
              where (a,b,c) = h2 r
          h2 (RWarn x r) = (a, b, TagWarning x : c)
              where (a,b,c) = h2 r
          h2 (REntity x y r) = (d:a,b,resultAdd opts e c)
              where (a,b,c) = h2 r ; (d,e) = optEntityAttrib opts (x,y)
          h2 (REntityChar x r) = (fromString1 (chr x) : a, b, c)
              where (a,b,c) = h2 r
          h2 (RText x r) = (x:a,b,c)
              where (a,b,c) = h2 r
          h2 r = ([],a,b)
              where (a,b) = f r

result2 _ RAttName{} = error "attname"
result2 _ RAttVal{} = error "attval"
result2 _ _ = error "unknown"


-- Merge all adjacent TagText bits
tagTextMerge :: StringLike str => [Tag str] -> [Tag str]
tagTextMerge (TagText x:xs) = TagText (strConcat (x:a)) : tagTextMerge b
    where
        (a,b) = f xs

        f (TagText x:xs) = (x:a,b)
            where (a,b) = f xs
        f x = ([], x)

tagTextMerge (x:xs) = x : tagTextMerge xs
tagTextMerge [] = []


tagWarning x = TagWarning (fromString x)
