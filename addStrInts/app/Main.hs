module Main where

import StrAdd2 (displayResult, strAddInts)

main :: IO ()
main = do
  putStrLn "Enter first argument"
  a <- getLine
  putStrLn "Enter second argument"
  b <- getLine

  let result = strAddInts a b

  putStrLn (displayResult result)