module Main where
import Example.Example (tests)

main :: IO ()
main =
   print $
      if tests
        then "test successful"
        else "test failed !!"
