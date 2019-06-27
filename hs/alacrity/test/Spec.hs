import System.Process
import System.Exit

import Alacrity.Util

main :: IO ()
main = do
  maybeDie $ system "stack exec alacrityc ala/rps.ala"
  exitSuccess
