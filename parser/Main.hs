
module Main where

import Parser
import Simplify
import CodeGen

main = do
    src <- readFile "tagsoup.txt"
    pre <- readFile "prefix.txt"
    let code = codeGen $ simplify $ parse src
    writeFile "generated.hs" (pre ++ code)


