import System.Process
import System.Exit

import Alacrity.Util

main :: IO ()
main = do
  maybeDie $ system "stack exec alacrityc ala/rps.ala"
  maybeDie $ system "solc --allow-paths $(basename \"$(basename \"$PWD\")\") -o ala/_build --bin --abi ala/rps.ala.sol"
  --- XXX maybeDie $ system "z3 ala/rps.ala.z3"
  exitSuccess
