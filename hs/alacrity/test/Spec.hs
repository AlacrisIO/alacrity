import System.Process
import System.Exit

import Alacrity.Util

main :: IO ()
main = do
  maybeDie "make build failed" $ system "cd ../../examples/rps-auto && make build"
  exitSuccess
