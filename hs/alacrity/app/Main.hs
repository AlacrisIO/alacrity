module Main where

import System.Environment 
import Alacrity.Compiler

main :: IO ()
main = do
  [srcp] <- getArgs
  compile srcp
