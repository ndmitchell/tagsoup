
module Text.HTML.TagSoup2.Out(Out(..), output) where

import Text.HTML.TagSoup.Type

data Out str
      -- Strings and continuations
    = OutStr str (Out str)
    | OutCont (Maybe str) (str -> Out str)

      -- TagSoup tags
    | OutTag             (Out str)  -- <
    | OutTagShut         (Out str)  -- </
    | OutAttName         (Out str)
    | OutAttVal          (Out str)
    | OutTagEndClose     (Out str)  -- />
    | OutComment         (Out str)  -- <!--
    | OutEntityName      (Out str)  -- &
    | OutEntityNum       (Out str)  -- &#
    | OutEntityHex       (Out str)  -- &#x
    | OutEntityEnd Bool  (Out str)  -- Attributed followed by ; for True, missing ; for False
    | OutWarn str        (Out str)


output :: ParseOptions str => Out str -> Tag str
output = undefined
