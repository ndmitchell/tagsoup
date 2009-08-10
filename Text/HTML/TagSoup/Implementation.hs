
module Text.HTML.TagSoup.Implementation where

import Data.List
import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Options_
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
    | Warn
    | Pos Position
      deriving Show


data S = S
    {s :: S
    ,tl :: S
    ,hd :: Char
    ,eof :: Bool
    ,err :: Out
    ,next :: String -> Maybe S
    ,pos :: Position
    }


expand :: String -> S
expand text = res
    where res = S{s = res
                 ,tl = expand (tail text)
                 ,hd = if null text then '\0' else head text
                 ,eof = null text
                 ,err = Warn
                 ,next = next text
                 ,pos = error "Todo: Calculate position"
                 }

          next (t:ext) (s:tr) | t == s = next ext tr
          next text [] = Just $ expand text
          next _ _ = Nothing

infixr &
(&) :: Outable a => a -> [Out] -> [Out]
(&) x xs = outable x : xs

class Outable a where outable :: a -> Out
instance Outable Char where outable = Char
instance Outable Out where outable = id


state :: String -> S
state s = expand s

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
    | REntity str Bool (Result str)   -- True is has final ;
    | REntityChar Int (Result str)
    | RWarn str (Result str)
    | RPos !Row !Column (Result str)
    | REof

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
output opts = output2 . filter f
    where f Warn{} = optTagWarning opts
          f Pos{} = optTagPosition opts
          f _ = True

output2 :: [Out] -> Result String
output2 [] = REof
output2 (Entity:xs) = outputEntity REntity xs
output2 (EntityNum:xs) = outputEntity (\x y -> REntityChar (read x)) xs
output2 (EntityHex:xs) = outputEntity (\x y -> REntityChar (fst $ head $ readHex x)) xs
output2 (TagShut:xs) = let (a,b) = readChars xs in RTagShut a (output2 b)
output2 (Tag:xs) = let (a,b) = readChars xs in RTagOpen a (output2 b)
output2 (TagEnd:xs) = RTagEnd (output2 xs)
output2 (TagEndClose:xs) = RTagEndClose (output2 xs)
output2 (AttName:xs) = let (a,b) = readChars xs in RAttName a (output2 b)
output2 (AttVal:xs) = let (a,b) = readChars xs in RAttVal a (output2 b)
output2 (Comment:xs) = let (a,b) = readChars xs in RComment a (output2 b)
output2 (CommentEnd:xs) = output2 xs


output2 (Char x:xs) = RText (x:a) b
    where (a,b) = f xs
          f (Char x:xs) = (x:a,b)
               where (a,b) = f xs
          f xs = ("", output2 xs)

output2 xs = error $ "output: " ++ show (take 10 xs)


outputEntity f xs = f a c (output2 d)
    where (a,b) = readChars xs
          (c,d) = readEntityEnd b


-- things which are Pos or Warn are moved back until after the Char's
readChars :: [Out] -> (String,[Out])
readChars (Char x:xs) = (x:a,b)
    where (a,b) = readChars xs
readChars (x@Warn{}:xs) = (a,x:b)
    where (a,b) = readChars xs
readChars (x@Pos{}:xs) = (a,x:b)
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
          f (RWarn x r) = TagWarning x : f r
          f REof = []
          f x = f $ rtail x
          
          g (RTagEnd r) = result2 opts r
          g (RTagEndClose r) = tagWarning "Shut tag with self closing" : result2 opts r
          g (RWarn x r) = TagWarning x : g r
          g REof = []
          g x = tagWarning "Junk is closing tag" : f (rtail x)

result2 opts (RTagOpen x r) = TagOpen x atts : rest
    where (atts,rest) = f r
          f (RTagEnd r) = ([], result2 opts r)
          f (RTagEndClose r) = ([], TagClose x : result2 opts r)
          f (RWarn x r) = (a, TagWarning x : b)
              where (a,b) = f r
          f (RPos x y r) = (a, TagPosition x y : b)
              where (a,b) = f r
          f (RAttVal x r) = ((empty,a):b, c)
              where (a,b,c) = h x r
          f (RAttName x r) = ((x,a):b, c)
              where (a,b,c) = g r
          f x = ([], result2 opts x)

          g (RAttVal x r) = h x r
          g (RWarn x r) = (a, b, TagWarning x : c)
              where (a,b,c) = g r
          g (RPos x y r) = (a, b, TagPosition x y : c)
              where (a,b,c) = g r
          g x = (empty,a,b)
              where (a,b) = f x

          h s r = (Str.concat $ s:a, b, c)
              where (a,b,c) = h2 r
          h2 (RPos x y r) = (a,b,TagPosition x y : c)
              where (a,b,c) = h2 r
          h2 (RWarn x r) = (a,b,TagWarning x : c)
              where (a,b,c) = h2 r
          h2 (REntity x y r) = (d:a,b,resultAdd opts e c)
              where (a,b,c) = h2 r ; (d,e) = optEntityAttrib opts (x,y)
          h2 (REntityChar x r) = (fromString1 (chr x) : a, b, c)
              where (a,b,c) = h2 r
          h2 (RText x r) = (x:a,b,c)
              where (a,b,c) = h2 r
          h2 r = ([],a,b)
              where (a,b) = f r


-- Merge all adjacent TagText bits
tagTextMerge :: StringLike str => [Tag str] -> [Tag str]
tagTextMerge (TagText x:xs) = TagText (Str.concat (x:a)) : tagTextMerge b
    where
        (a,b) = f xs

        f (TagText x:xs) = (x:a,b)
            where (a,b) = f xs
        f x = ([], x)

tagTextMerge (x:xs) = x : tagTextMerge xs
tagTextMerge [] = []


tagWarning x = TagWarning (fromString x)
