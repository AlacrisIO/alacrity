module Alacrity.Compiler where

import Numeric.Natural
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
import qualified Data.Text.Lazy.IO as TIO
import Data.Foldable
import qualified Data.Sequence as S
import Data.Text.Prettyprint.Doc
import Language.JavaScript.Parser as JS
import System.Exit
import Z3.Monad as Z3
import Text.Pretty.Simple

import Alacrity.AST
import Alacrity.Parser

{- ANF

   See AST for a description of the job of this pass.

   The ANF monad stores the next available variable and the list of
   defined variables.
 -}

type ANFElem = (Maybe Participant, ILVar, ILExpr)
type ANFMonad a = State (Natural, S.Seq ANFElem) a

runANF :: ANFMonad a -> a
runANF am = if null vs then a else error "ANF: Left variables in state!"
  where
    (a, (_, vs)) = runState am (0, S.Empty)

--- Run an ANF computation, with local new variables, but a global new
--- variable counter.
collectANF :: (ANFElem -> a -> a) -> (ANFMonad a) -> ANFMonad a
collectANF f ma = do
  (v0, vs0) <- get
  let (a, (v1, vs1)) = runState ma (v0, S.Empty)
  put (v1, vs0)
  return (foldr f a vs1)

consumeANF :: String -> ANFMonad ILVar
consumeANF s = do
  (nv, vs) <- get
  put (nv+1, vs)
  return (nv, s)

consumeANF_N :: Natural -> ANFMonad [ILVar]
consumeANF_N 0 = return []
consumeANF_N n = do
  v <- consumeANF ("ANF_N" ++ show n)
  vs <- consumeANF_N (n - 1)
  return $ v : vs

allocANF :: Maybe Participant -> ILExpr -> String -> ANFMonad ILVar
allocANF mp e s = do
  (nvi, vs) <- get
  let nv = (nvi, s)
  put (nvi + 1, vs S.|> (mp, nv, e))
  return nv

type XLRenaming = M.Map XLVar ILArg
type XLFuns = M.Map XLVar XLDef

anf_parg :: (XLVar, AType) -> (XLRenaming, [(ILVar, AType)]) -> ANFMonad (XLRenaming, [(ILVar, AType)])
anf_parg (v, t) (ρ, args) =
  case M.lookup v ρ of
    Nothing -> do
      nv <- consumeANF v
      return (M.insert v (IL_Var nv) ρ, args' nv)
    Just (IL_Var nv) -> return (ρ, args' nv)
    Just _ -> error $ "ANF: Participant argument not bound to variable: " ++ v
  where args' nv = args ++ [(nv,t)]

anf_part :: (XLRenaming, ILPartInfo) -> (Participant, [(XLVar, AType)]) -> ANFMonad (XLRenaming, ILPartInfo)
anf_part (ρ, ips) (p, args) = do
  (ρ', args') <- foldrM anf_parg (ρ, []) args
  let ips' = M.insert p args' ips
  return (ρ', ips')

anf_parts :: XLPartInfo -> ANFMonad (XLRenaming, ILPartInfo)
anf_parts ps = foldM anf_part (M.empty, M.empty) (M.toList ps)

anf_exprs :: XLFuns -> Maybe Participant -> XLRenaming -> [XLExpr] -> ([ILArg] -> ANFMonad (Natural, ILTail)) -> ANFMonad (Natural, ILTail)
anf_exprs σ me ρ es mk =
  case es of
    [] -> mk []
    e : more ->
      anf_expr σ me ρ e (\ [ e' ] -> anf_exprs σ me ρ more (\ es' -> mk $ e' : es'))

vsOnly :: [ILArg] -> [ILVar]
vsOnly [] = []
vsOnly (IL_Var v : m) = v : vsOnly m
vsOnly (_ : m) = vsOnly m

anf_expr :: XLFuns -> Maybe Participant -> XLRenaming -> XLExpr -> ([ILArg] -> ANFMonad (Natural, ILTail)) -> ANFMonad (Natural, ILTail)
anf_expr σ me ρ e mk =
  case e of
    XL_Con b ->
      mk [ IL_Con b ]
    XL_Var v ->
      case M.lookup v ρ of
        Nothing -> error ("ANF: Variable unbound: " ++ (show v))
        Just a -> mk [ a ]
    XL_PrimApp p args ->
      anf_exprs σ me ρ args (\args' -> ret_expr ("PrimApp" ++ show p) (IL_PrimApp p args'))
    XL_If ce te fe ->
      anf_expr σ me ρ ce k
      where k [ ca ] = do
              (tn, tt) <- anf_tail σ me ρ te mk
              (fn, ft) <- anf_tail σ me ρ fe mk
              unless (tn == fn) $ error "ANF: If branches don't have same continuation arity"
              return (tn, IL_If ca tt ft)
            k _ = error "anf_expr XL_If ce doesn't return 1"
    XL_Assert ae ->
      anf_expr σ me ρ ae (\[ aa ] -> ret_expr "Assert" (IL_Assert aa))
    XL_Consensus p msg body -> do
      anf_expr σ me ρ msg k
      where k msgas = do
              let msgvs = vsOnly msgas 
              (bn, bodyt) <- anf_tail σ Nothing ρ body anf_ktop
              outs <- consumeANF_N bn
              (kn, kt) <- mk $ map IL_Var outs
              return (kn, IL_Consensus p msgvs bodyt outs kt)
    XL_Values args ->
      anf_exprs σ me ρ args (\args' -> mk args')
    XL_Transfer from to ae ->
      anf_expr σ me ρ ae (\[ aa ] -> ret_expr "Transfer" (IL_Transfer from to aa))
    XL_Declassify ae ->
      anf_expr σ me ρ ae (\[ aa ] -> ret_expr "Declassify" (IL_Declassify aa))
    XL_LetValues mwho mvs ve be ->
      anf_expr σ who ρ ve k
      where who = case mwho of
                    Nothing -> me
                    Just _ -> mwho
            k nvs = anf_expr σ me ρ' be mk
              where ρ' = case mvs of
                      Nothing -> ρ
                      Just ovs ->
                        let olen = length ovs
                            nlen = length nvs in
                        if olen == nlen then
                          M.union ρ $ M.fromList $ zip ovs nvs
                        else
                          error $ "ANF XL_LetValues, context arity mismatch, " ++ show olen ++ " vs " ++ show nlen
    XL_FunApp f args ->
      case M.lookup f σ of
        Just (XL_DefineFun _ formals fun_body) ->
          anf_expr σ' me ρ ne mk
          where ne = XL_LetValues Nothing (Just formals) (XL_Values args) fun_body
                --- We remove f from sigma because we are not allowed to write recursive functions.
                {- XXX We can't do this because `args` might mention the function too
                   Solution 1: Do nothing and infinite programs crash.
                   Solution 2: Inline/externalize the code for LetValues that would run
                   Solution 3: Make a separate inlining pass and language level
                 -}
                --- σ' = M.delete f σ
                σ' = σ
        _ -> error $ "ANF XL_FunApp, f not defined: " ++ show f
  where ret_expr s ne = do
          nv <- allocANF me ne s
          mk [ IL_Var nv ]

anf_addVar :: ANFElem -> (Natural, ILTail) -> (Natural, ILTail)
anf_addVar (mp, v, e) (c, t) = (c, IL_Let mp (Just v) e t)

anf_tail :: XLFuns -> Maybe Participant -> XLRenaming -> XLExpr -> ([ILArg] -> ANFMonad (Natural, ILTail)) -> ANFMonad (Natural, ILTail)
anf_tail σ me ρ e mk = do
  collectANF anf_addVar (anf_expr σ me ρ e mk)

anf_ktop :: [ILArg] -> ANFMonad (Natural, ILTail)
anf_ktop args = return (fromInteger (toInteger (length args)), IL_Ret args)

anf_funs :: [XLDef] -> XLFuns
anf_funs [] = M.empty
anf_funs (XL_DefineValues _ _ : ds) = anf_funs ds
anf_funs (d@(XL_DefineFun f _ _) : ds) = M.insert f d $ anf_funs ds

wrap_defs :: [XLDef] -> XLExpr -> XLExpr
wrap_defs [] e = e
wrap_defs (d : ds) me =
  case d of
    XL_DefineFun _ _ _ -> me'
    XL_DefineValues vs e -> XL_LetValues Nothing (Just vs) e me'
  where me' = wrap_defs ds me

anf :: XLProgram -> ILProgram
anf xlp = IL_Prog ips xt
  where
    XL_Prog defs ps main = xlp
    σ = anf_funs defs
    main_w_defs = wrap_defs defs main
    (ips, xt) = runANF xm
    xm :: ANFMonad (ILPartInfo, ILTail)
    xm = do
      (ρ, nps) <- anf_parts ps
      (_, mt) <- anf_tail σ Nothing ρ main_w_defs anf_ktop
      return (nps, mt)

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
epp _ = error $ "XXX epp"

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
as_js _ = error $ "XXX as_js"

emit_js :: BLProgram -> String
emit_js blp = JS.renderToString $ as_js blp

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
emit_sol _ = error $ "XXX emit_sol"

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
emit_z3 _
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
