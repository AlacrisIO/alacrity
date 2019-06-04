import System.Process
import System.Exit

main :: IO ()
main = do
  ec <- system "stack exec alacrityc ala/rps.ala"
  exitWith ec
