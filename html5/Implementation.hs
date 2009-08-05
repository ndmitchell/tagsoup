{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Implementation where

import Data.List

---------------------------------------------------------------------
-- OUTPUT


data Out
    = Char Char
    | TagOpen     -- <
    | TagShut
    | AttName
    | AttVal
    | TagEnd      -- >
    | TagEndClose -- />
    | CommentOpen -- <!--
    | CommentEnd  -- -->
    | DocTypeEnd
    | Error
      deriving Show


---------------------------------------------------------------------
-- STATE

data Content = PCData | RCData | CData | PlainText
               deriving Eq
    



data S = S
    -- REAL INFORMATION
    {prev :: String -- reversed, like a zipper
    ,next :: String
    ,content :: Content
    ,esc :: Bool
    
    -- USEFUL SUGAR
    ,s :: S
    ,tl :: S
    ,hd :: Char
    ,pcdata :: Bool
    ,pcdata_rcdata :: Bool
    ,rcdata_cdata :: Bool
    ,escape :: Bool
    ,noescape :: Bool
    ,eof :: Bool
    ,err :: Out
    ,before :: String -> Bool
    ,after :: String -> Bool
    ,drp :: Int -> S
    }


expand :: S -> S
expand o@S{prev=prev, next=next, content=content, esc=esc} = o
    {s = expand o
    ,tl = expand o{prev = head next : prev, next = tail next}
    ,hd = if null next then '\0' else head next
    ,pcdata = content == PCData
    ,pcdata_rcdata = content == PCData || content == RCData
    ,rcdata_cdata = content == RCData || content == CData
    ,escape = esc
    ,noescape = not esc
    ,eof = null next
    ,err = Error
    ,before = \x -> reverse x `isPrefixOf` prev
    ,after = \x -> x `isPrefixOf` next
    ,drp = \i -> if i == 0 then expand o else drp (tl $ expand o) (i-1)
    }

infixr &
(&) :: Outable a => a -> [Out] -> [Out]
(&) x xs = outable x : xs

class Outable a where outable :: a -> Out
instance Outable Char where outable = Char
instance Outable Out where outable = id


escape_ :: S -> S
escape_ s = expand s{esc=True}

noescape_ :: S -> S
noescape_ s = expand s{esc=False}



state :: String -> S
state s = expand S{prev=[],next=s,content=PCData,esc=False}

