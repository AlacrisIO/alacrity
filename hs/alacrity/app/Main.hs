module Main where

import System.Environment 
import System.IO
import Lib

main :: IO ()
main = do
  [srcp] <- getArgs
  compile srcp
