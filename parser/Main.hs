
module Main where

import Compiler.Resolve
import Compiler.Parser
import Compiler.OptimiseLp
import Compiler.CodeGen
import Compiler.OptimiseHs

main = do
    src <- readFile "tagsoup2.txt"
    pre <- readFile "Prefix.hs"
    let code = show $ optimiseHs $ codeGen $ optimiseLp $ resolve $ parse src
    writeFile "../Text/HTML/TagSoup/Generated/Parser.hs" (pre ++ code)
