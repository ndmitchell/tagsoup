
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
    {text :: String
    
    -- USEFUL SUGAR
    ,s :: S
    ,tl :: S
    ,hd :: Char
    ,eof :: Bool
    ,err :: Out
    ,next :: String -> Maybe S
    }


expand :: String -> S
expand text = res
    where res = S{text = text
                 ,s = res
                 ,tl = expand (tail text)
                 ,hd = if null text then '\0' else head text
                 ,eof = null text
                 ,err = Error
                 ,next = next text
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

