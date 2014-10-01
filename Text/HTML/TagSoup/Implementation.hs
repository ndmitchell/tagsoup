{-# LANGUAGE RecordWildCards, PatternGuards, ScopedTypeVariables #-}

module Text.HTML.TagSoup.Implementation where

import Text.HTML.TagSoup.Type
import Text.HTML.TagSoup.Options
import Text.StringLike as Str
import Numeric (readHex)
import Data.Char (chr, ord)
import Data.Ix
import Control.Exception(assert)
import Control.Arrow

---------------------------------------------------------------------
-- BOTTOM LAYER

data Out
    = Char Char
    | Tag             -- <
    | TagShut         -- </
    | AttName
    | AttVal
    | TagEnd          -- >
    | TagEndClose     -- />
    | Comment         -- <!--
    | CommentEnd      -- -->
    | EntityName      -- &
    | EntityNum       -- &#
    | EntityHex       -- &#x
    | EntityEnd Bool  -- Attributed followed by ; for True, missing ; for False
    | Warn String
    | Pos Position
      deriving (Show,Eq)

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

class Outable a where (&) :: a -> [Out] -> [Out]
instance Outable Char where (&) = ampChar
instance Outable Out where (&) = ampOut
ampChar x y = Char x : y
ampOut x y = x : y


state :: String -> S
state s = expand nullPosition s

---------------------------------------------------------------------
-- TOP LAYER


output :: forall str . StringLike str => ParseOptions str -> [Out] -> [Tag str]
output ParseOptions{..} x = (if optTagTextMerge then tagTextMerge else id) $ go ((nullPosition,[]),x)
    where
        -- main choice loop
        go :: ((Position,[Tag str]),[Out]) -> [Tag str]
        go ((p,ws),xs) | p `seq` False = [] -- otherwise p is a space leak when optTagPosition == False
        go ((p,ws),xs) | not $ null ws = (if optTagWarning then (reverse ws++) else id) $ go ((p,[]),xs)
        go ((p,ws),Pos p2:xs) = go ((p2,ws),xs)

        go x | isChar x = pos x $ TagText a : go y
            where (y,a) = charsStr x
        go x | isTag x = pos x $ TagOpen a b : (if isTagEndClose z then pos x $ TagClose a : go (next z) else go (skip isTagEnd z))
            where (y,a) = charsStr $ next x
                  (z,b) = atts y
        go x | isTagShut x = pos x $ (TagClose a:) $
                (if not (null b) then warn x "Unexpected attributes in close tag" else id) $
                if isTagEndClose z then warn x "Unexpected self-closing in close tag" $ go (next z) else go (skip isTagEnd z)
            where (y,a) = charsStr $ next x
                  (z,b) = atts y
        go x | isComment x = pos x $ TagComment a : go (skip isCommentEnd y)
            where (y,a) = charsStr $ next x
        go x | isEntityName x = poss x ((if optTagWarning then id else filter (not . isTagWarning)) $ optEntityData (a, getEntityEnd y)) ++ go (skip isEntityEnd y) 
            where (y,a) = charsStr $ next x
        go x | isEntityNumHex x = pos x $ TagText (fromChar $ entityChr x a) : go (skip isEntityEnd y)
            where (y,a) = chars $ next x
        go x | Just a <- fromWarn x = if optTagWarning then pos x $ TagWarning (fromString a) : go (next x) else go (next x)
        go x | isEof x = []

        atts :: ((Position,[Tag str]),[Out]) -> ( ((Position,[Tag str]),[Out]) , [(str,str)] )
        atts x | isAttName x = second ((a,b):) $ atts z
            where (y,a) = charsStr (next x)
                  (z,b) = if isAttVal y then charsEntsStr (next y) else (y, empty)
        atts x | isAttVal x = second ((empty,a):) $ atts y
            where (y,a) = charsEntsStr (next x)
        atts x = (x, [])

        -- chars
        chars x = charss False x
        charsStr x = (id *** fromString) $ chars x
        charsEntsStr x = (id *** fromString) $ charss True x

        -- loop round collecting characters, if the b is set including entity
        charss :: Bool -> ((Position,[Tag str]),[Out]) -> ( ((Position,[Tag str]),[Out]) , String)
        charss t x | Just a <- fromChr x = (y, a:b)
            where (y,b) = charss t (next x)
        charss t x | t, isEntityName x = second (toString n ++) $ charss t $ addWarns m z
            where (y,a) = charsStr $ next x
                  b = getEntityEnd y
                  z = skip isEntityEnd y
                  (n,m) = optEntityAttrib (a,b)
        charss t x | t, isEntityNumHex x = second (entityChr x a:) $ charss t z
            where (y,a) = chars $ next x
                  z = skip isEntityEnd y
        charss t ((_,w),Pos p:xs) = charss t ((p,w),xs)
        charss t x | Just a <- fromWarn x = charss t $ (if optTagWarning then addWarns [TagWarning $ fromString a] else id) $ next x
        charss t x = (x, [])

        -- utility functions
        next x = second (drop 1) x
        skip f x = assert (isEof x || f x) (next x)
        addWarns ws x@((p,w),y) = ((p, reverse (poss x ws) ++ w), y)
        pos ((p,_),_) rest = if optTagPosition then tagPosition p : rest else rest
        warn x s rest = if optTagWarning then pos x $ TagWarning (fromString s) : rest else rest
        poss x = concatMap (\w -> pos x [w]) 


entityChr x s | isEntityNum x = chr_ $ read s
              | isEntityHex x = chr_ $ fst $ head $ readHex s
    where chr_ x | inRange (toInteger $ ord minBound, toInteger $ ord maxBound) x = chr $ fromInteger x
                 | otherwise = '?'


isEof (_,[]) = True; isEof _ = False
isChar (_,Char{}:_) = True; isChar _ = False
isTag (_,Tag{}:_) = True; isTag _ = False
isTagShut (_,TagShut{}:_) = True; isTagShut _ = False
isAttName (_,AttName{}:_) = True; isAttName _ = False
isAttVal (_,AttVal{}:_) = True; isAttVal _ = False
isTagEnd (_,TagEnd{}:_) = True; isTagEnd _ = False
isTagEndClose (_,TagEndClose{}:_) = True; isTagEndClose _ = False
isComment (_,Comment{}:_) = True; isComment _ = False
isCommentEnd (_,CommentEnd{}:_) = True; isCommentEnd _ = False
isEntityName (_,EntityName{}:_) = True; isEntityName _ = False
isEntityNumHex (_,EntityNum{}:_) = True; isEntityNumHex (_,EntityHex{}:_) = True; isEntityNumHex _ = False
isEntityNum (_,EntityNum{}:_) = True; isEntityNum _ = False
isEntityHex (_,EntityHex{}:_) = True; isEntityHex _ = False
isEntityEnd (_,EntityEnd{}:_) = True; isEntityEnd _ = False
isWarn (_,Warn{}:_) = True; isWarn _ = False

fromChr (_,Char x:_) = Just x ; fromChr _ = Nothing
fromWarn (_,Warn x:_) = Just x ; fromWarn _ = Nothing

getEntityEnd (_,EntityEnd b:_) = b


-- Merge all adjacent TagText bits
tagTextMerge :: StringLike str => [Tag str] -> [Tag str]
tagTextMerge (TagText x:xs) = TagText (strConcat (x:a)) : tagTextMerge b
    where
        (a,b) = f xs

        -- additional brackets on 3 lines to work around HSE 1.3.2 bugs with pattern fixities
        f (TagText x:xs) = (x:a,b)
            where (a,b) = f xs
        f (TagPosition{}:(x@TagText{}:xs)) = f $ x : xs
        f x = g x id x

        g o op (p@TagPosition{}:(w@TagWarning{}:xs)) = g o (op . (p:) . (w:)) xs
        g o op (w@TagWarning{}:xs) = g o (op . (w:)) xs
        g o op (p@TagPosition{}:(x@TagText{}:xs)) = f $ p : x : op xs
        g o op (x@TagText{}:xs) = f $ x : op xs
        g o op _ = ([], o)

tagTextMerge (x:xs) = x : tagTextMerge xs
tagTextMerge [] = []
