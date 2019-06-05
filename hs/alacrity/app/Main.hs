module Main where

import System.Environment 
import System.IO
import Alacrity.Compiler

main :: IO ()
main = do
  [srcp] <- getArgs
  compile srcp
