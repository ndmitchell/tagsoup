
module TagSoup.Generate.Supercompile(supercompile) where

import TagSoup.Generate.Type
import TagSoup.Generate.Simplify


supercompile :: Prog -> Prog
supercompile = simplifyProg
