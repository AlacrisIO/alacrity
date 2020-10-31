module Alacrity.Util where

import System.Exit
import Control.Monad

maybeDie :: String -> IO ExitCode -> IO ()
maybeDie msg ma = do
  ec <- ma
  unless (ec == ExitSuccess)
    (do putStrLn msg
        (exitWith ec))
  return ()
