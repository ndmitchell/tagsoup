
module Implementation where

import Data.List

---------------------------------------------------------------------
-- OUTPUT


data Out
    = Char Char
    | TagOpen      -- <
    | TagShut
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
    | Error
      deriving Show


---------------------------------------------------------------------
-- STATE

data S = S
    -- REAL INFORMATION
    {prev :: String -- reversed, like a zipper
    ,next :: String
    
    -- USEFUL SUGAR
    ,s :: S
    ,tl :: S
    ,hd :: Char
    ,eof :: Bool
    ,err :: Out
    ,before :: String -> Bool
    ,after :: String -> Bool
    ,drp :: Int -> S
    }


expand :: String -> String -> S
expand prev next = res
    where res = S{prev = prev
                 ,next = next
                 ,s = res
                 ,tl = expand (head next : prev) (tail next)
                 ,hd = if null next then '\0' else head next
                 ,eof = null next
                 ,err = Error
                 ,before = \x -> reverse x `isPrefixOf` prev
                 ,after = \x -> x `isPrefixOf` next
                 ,drp = \i -> if i == 0 then res else drp (tl res) (i-1)
                 }

infixr &
(&) :: Outable a => a -> [Out] -> [Out]
(&) x xs = outable x : xs

class Outable a where outable :: a -> Out
instance Outable Char where outable = Char
instance Outable Out where outable = id


state :: String -> S
state s = expand [] s

