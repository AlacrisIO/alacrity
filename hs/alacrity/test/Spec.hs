import System.Process
import System.Exit

import Alacrity.Util

main :: IO ()
main = do
  maybeDie $ system "stack exec alacrityc ala/rps.ala"
  maybeDie $ system "REPO=$(dirname \"$(dirname \"$PWD\")\") ; solc --allow-paths $REPO -o ala/_build --bin --abi --overwrite ala/rps.ala.sol"
  --- XXX maybeDie $ system "z3 ala/rps.ala.z3"
  exitSuccess
