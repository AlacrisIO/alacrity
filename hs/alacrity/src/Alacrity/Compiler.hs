module Alacrity.Compiler where

import Control.Monad.State.Lazy
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy.IO as TIO
import Data.Text.Prettyprint.Doc
import System.Exit
import System.IO
import Z3.Monad as Z3
import Text.Pretty.Simple

import Alacrity.AST
import Alacrity.Parser

{- ANF

   See AST for a description of the job of this pass.

   The ANF monad stores the next available variable and the list of
   defined variables.
 -}
  
type ANFMonad a = State (ILVar, [(Maybe ILVar, ILExpr)]) a

runANF :: ANFMonad a -> a
runANF am = a
  where
    (a, (_, [])) = runState am (0, [])

type XLRenaming = (M.Map XLVar ILArg)

anf :: XLProgram -> ILProgram
anf xlp = runANF xm
  where
    XL_Prog defs ps main = xlp
    anf_funs :: [XLDef] -> (M.Map XLVar XLDef)
    anf_funs ds = error "XXX anf_funs"
    funmap = anf_funs defs
    anf_defs :: [XLDef] -> ANFMonad XLRenaming
    anf_defs ds = error "XXX anf_defs"
    anf_ps :: XLPartInfo -> ANFMonad (XLRenaming, ILPartInfo)
    anf_ps ps = error "XXX anf_ps"
    anf_tail :: XLRenaming -> XLExpr -> ANFMonad ILTail
    anf_tail ρ xe = error "XXX anf_tail"
    anf_expr :: XLRenaming -> XLExpr -> ANFMonad ILExpr
    anf_expr ρ xe = error "XXX anf_expr"
    anf_arg :: XLRenaming -> XLExpr -> ANFMonad ILArg
    anf_arg ρ xe = error "XXX anf_arg"
    xm :: ANFMonad ILProgram
    xm = do
      ρ0 <- anf_defs defs
      (ρ1, nps) <- anf_ps ps
      let ρ2 = M.union ρ0 ρ1
      mt <- anf_tail ρ2 main
      return (IL_Prog nps mt)

--- End-Point Projection

{-

This stage needs to generate the sub-programs and verify the following
properties:

1. The program is well-typed. (All types can be derived from the types
   of the participant's initial knowledge, so we only require
   annotations on those.) [This implies that the contract has no
   free-variables.]

2. The contract does not execute illegal primitives. The participants
   do not execute transfer.

3. No secret information is shared. (By default, all participants'
   initial knowledge is secret.)

4. All parties assert that information they receive as message
contents are the same as things they already know.

XXX Maybe more

-}

--- XXX The default type of variables is Secret
data SecurityLevel
  = Secret
  | Public
  deriving (Show)

type SType = (AType, SecurityLevel)

epp :: ILProgram -> BLProgram
epp ilp = error "XXX epp"

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

emit_js :: BLProgram -> Doc ann
emit_js blp = error "XXX emit_js"

{- Compilation to Solidity

   The handler program becomes a contract factory where the first
   interaction is tied into the contract creation. The contract has a
   different function for each consensus block. The arguments are the
   input variables. At the end, the output variables are emitted via
   an event.

   In the future, when we compile to EVM directly, it won't work that
   way though. Instead, we'll do dispatch ourselves.
 -}

--- XXX The consensus block can know all of the variables to do hash storage.
emit_sol :: BLProgram -> Doc ann
emit_sol blp = error "XXX emit_sol"

{- Z3 Theory Generation

   The Z3 theory has to prove a few different things.

   1. The balance of CTC at the end of the protocol is 0. It will have
   to do this by employing something like the State monad to represent
   all the various modifications to the CTC value overtime and assert
   that it is 0 at the end. This ensure that the protocol doesn't
   "leave anything on the table".

   2. For each assert! in the program, assuming that all previous
   assert!s are true (including those in other participants and the
   contract) and DISHONEST is FALSE, the assert is true.

   3. For each assert! in the program, assuming that all previous
   assert!s are true (EXCLUDING those in other participants, but
   INCLUDING the contract) and DISHONEST is TRUE, the assert is true.

   When

   #1 has to be done in both #2 and #3 modes, because the transfer
   amounts may rely on participant data.

 -}

emit_z3 :: BLProgram -> Z3.Z3 [String]
emit_z3 blp
  -- XXX Actually implement theory generation
 = do
  Z3.push
  f <- Z3.mkFalse
  Z3.assert f
  (res, mm) <- Z3.solverCheckAndGetModel
  Z3.pop 1
  case res of
    Z3.Unsat -> return []
    Z3.Sat ->
      case mm of
        Nothing -> return ["Problem; no model!"]
        Just m -> do
          s <- modelToString m
          return [s]
    Z3.Undef -> return []

compile :: FilePath -> IO ()
compile srcp = do
  xlp <- readAlacrityFile srcp
  TIO.writeFile (srcp ++ ".xl") (pShowNoColor xlp)
  let ilp = anf xlp
  TIO.writeFile (srcp ++ ".il") (pShowNoColor ilp)
  let blp = epp ilp
  TIO.writeFile (srcp ++ ".bl") (pShowNoColor blp)
  z3res <- evalZ3 (emit_z3 blp)
  case z3res of
    [] -> do
      writeFile (srcp ++ ".sol") (show (emit_sol blp))
      writeFile (srcp ++ ".js") (show (emit_js blp))
      exitSuccess
    ps -> do
      mapM_ (\x -> putStrLn $ ("Z3 error:" ++ x)) ps
      die "Z3 failed to verify!"
