import System.Process
import System.Exit
import Control.Monad

maybeDie :: IO ExitCode -> IO ()
maybeDie ma = do
  ec <- ma
  unless (ec == ExitSuccess) (exitWith ec)
  return ()

main :: IO ()
main = do
  maybeDie $ system "stack exec alacrityc ala/rps.ala"
  maybeDie $ system "solc --allow-paths $(basename \"$(basename \"$PWD\")\") -o ala/_build --bin ala/rps.ala.sol"
  exitSuccess
