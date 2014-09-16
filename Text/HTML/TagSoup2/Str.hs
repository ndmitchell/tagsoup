
module Text.HTML.TagSoup2.Str(module Text.StringLike) where

import Text.StringLike


data Pos str = Pos Position str

instance StringLike str => StringLike (Pos str) where


position :: Tag (Position str) -> Tag str
position = undefined
