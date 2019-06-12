module Alacrity.EmitJS where

--import Control.Monad.State.Lazy
--import qualified Data.Map.Strict as M
--import Data.Foldable
--import qualified Data.Sequence as S
--import Data.Text.Prettyprint.Doc
import Language.JavaScript.Parser as JS
import Language.JavaScript.Parser.Parser as JSParser
import Language.JavaScript.Parser.AST as JSAST
--import Language.JavaScript.Parser.SrcLocation as SrcLocation
import System.IO

import Alacrity.AST

{- Compilation to Javascript

   I'm imagining the type of the JS export is:

   Network -> Participant -> NetworkArgs x Args x (Result -> A) -> A or doesn't

   We have some standard way of interacting with the network (so we
   don't depend in the compiler on whether we're using rinkydink or
   whatever or what the name is.) Then, you can look up the code for
   one of the participants (by name). Then you provide extra arguments
   for the network (such as the contract / game id) and the
   participant's initial knowledge, then a continuation for what to do
   with the result.
  -}

as_js :: BLProgram -> JS.JSAST
as_js (BL_Prog _blparts (C_Prog _handlers)) = JS.JSAstModule
  [] JSNoAnnot

emit_js :: BLProgram -> String
emit_js blp = JS.renderToString $ as_js blp

toJsl :: [a] -> JSAST.JSCommaList a
toJsl = toJsl' . reverse

toJsl' :: [a] -> JSAST.JSCommaList a
toJsl' [] = JSAST.JSLNil
toJsl' [a] = JSAST.JSLOne a
toJsl' (a:as) = JSAST.JSLCons (toJsl' as) JSAST.JSNoAnnot a

js_import_names :: [String] -> JSAST.JSImportClause
js_import_names =
  (\x -> JSAST.JSImportClauseNamed (JSAST.JSImportsNamed JS.JSNoAnnot x JS.JSNoAnnot)) .
  toJsl .
  map (\name -> JSAST.JSImportSpecifier (JSAST.JSIdentName JSAST.JSNoAnnot name))

js_imports :: String -> [String] -> JSAST.JSModuleItem
js_imports module_name identifiers = JSAST.JSModuleImportDeclaration JS.JSNoAnnot
  (JSAST.JSImportDeclaration
   (js_import_names identifiers)
   (JSAST.JSFromClause JS.JSNoAnnot JS.JSNoAnnot module_name) JS.JSSemiAuto)

-- For debug purposes (Fare)
-- | Parse JavaScript Module (Script)
-- Parse one compound statement, or a sequence of simple statements.
-- Generally used for interactive input, such as from the command line of an interpreter.
-- Return comments in addition to the parsed statements.
parseJS :: String -- ^ The name of the Javascript source (filename or input device).
     -> IO (Either String JS.JSAST)
        -- ^ An error or maybe the abstract syntax tree (AST) of zero
        -- or more Javascript statements, plus comments.
parseJS filename = do
  x <- System.IO.readFile filename
  return (JSParser.parseModule x filename)

dbj :: String
dbj = "/home/fare/src/fare/alacrity/examples/rps-demo/dapp-backend.mjs"

pjb :: a -> IO ()
pjb _ = do
  x <- parseJS dbj
  writeFile (dbj ++ ".jsast") (show x)

pjf :: a -> IO (Either String JS.JSAST)
pjf _ = parseJS "/tmp/foo.mjs"

pjs :: String -> Either String JS.JSAST
pjs x = JSParser.parseModule x "stdin"
