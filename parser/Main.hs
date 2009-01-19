
module Main where

import Compiler.Parser
import Compiler.Simplify
import Compiler.CodeGen

main = do
    src <- readFile "tagsoup.txt"
    pre <- readFile "Prefix.hs"
    let code = codeGen $ simplify $ parse src
    writeFile "generated.hs" (pre ++ code)


