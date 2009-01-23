
module Main where

import Compiler.Parser2
import Compiler.Simplify2
import Compiler.CodeGen2

main = do
    src <- readFile "tagsoup2.txt"
    pre <- readFile "Prefix.hs"
    let code = codeGen $ simplify $ parse src
    writeFile "../Text/HTML/TagSoup/Generated/Parser.hs" (pre ++ code)


