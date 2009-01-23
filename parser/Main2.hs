
module Main where

import Compiler.Parser2
import Compiler.Optimise
import Compiler.CodeGen2
import Compiler.Resolve

main = do
    src <- readFile "tagsoup2.txt"
    pre <- readFile "Prefix.hs"
    let code = codeGen $ optimise $ resolve $ parse src
    writeFile "../Text/HTML/TagSoup/Generated/Parser.hs" (pre ++ code)
