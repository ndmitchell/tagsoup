
module Main(main) where

import System.Environment
import Example.Example
import Example.Regress
import System.Exit


type Test = (String,IO ())

allTests = tests ++ regression

tests = [("hitcount",("Haskell Hit Count",haskellHitCount))
        ,("google",("Google Tech News",googleTechNews))
        ,("spj",("Simon Peyton Jones' papers",spjPapers))
        ,("ndm",("Neil Mitchell's papers",ndmPapers))
        ,("time",("Current Time",currentTime))
        ]

regression = [("regress",("Regression Tests",regress))]


main = do
    args <- getArgs
    todo <- if null args then do
                putStrLn "Running all tests"
                return $ map snd tests
            else 
                mapM getTest args
    mapM doTest todo


getTest :: String -> IO Test
getTest x = case lookup x allTests of 
                 Just y -> return y
                 Nothing -> do
                    putStrLn $ "Unrecognised test: " ++ x
                    putStrLn $ "Expected one of: " ++ unwords (map fst allTests)
                    exitFailure


doTest :: Test -> IO ()
doTest (name,act) = do
    putStrLn $ "= " ++ name ++ "="
    act
    putStrLn ""
