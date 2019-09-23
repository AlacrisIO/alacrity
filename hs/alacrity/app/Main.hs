module Main where

import System.Directory
import Options.Applicative

import qualified Alacrity.AST as AST

import Alacrity.Compiler


compiler :: FilePath -> Parser CompilerOpts
compiler cwd = CompilerOpts
  <$> strOption ( long "output"
               <> short 'o'
               <> metavar "DIR"
               <> help "Directory for output files"
               <> showDefault
               <> value cwd )

  <*> option auto ( long "timeout-window-in-blocks"
                 <> short 't'
                 <> metavar "BLOCKS"
                 <> help "Operations which may time out must occur within N blocks"
                 <> showDefault
                 <> value AST.defaultTimeoutWindowInBlocks )

  <*> strArgument (metavar "SOURCE")


main :: IO ()
main = do
  cwd <- getCurrentDirectory
  let opts = info ( compiler cwd <**> helper )
               ( fullDesc
               <> progDesc "Verify and compile an Alacrity program"
               <> header "alacrityc - alacrity compiler" )
  copts <- execParser opts
  compile copts
