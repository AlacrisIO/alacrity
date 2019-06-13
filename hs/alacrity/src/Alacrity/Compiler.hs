{-# LANGUAGE TemplateHaskell #-}
module Alacrity.Compiler where

import Control.Monad.State.Lazy
import qualified Data.Map.Strict as M
import Data.FileEmbed
import Data.Foldable
import qualified Data.Sequence as S
import Data.Text.Prettyprint.Doc
import System.Exit
import Z3.Monad as Z3

import Alacrity.AST
import Alacrity.Parser
import Alacrity.EmitJS

{- Inliner

   We remove XL_FunApp and convert XL_If into IF-THEN-ELSE where
   possible.

 -}

type XLFuns = M.Map XLVar ([XLVar], XLExpr)
type XLIFuns = M.Map XLVar (Bool, ([XLVar], XLExpr))
type InlineMonad a = State (XLFuns, XLIFuns) (Bool, a)

inline_fun :: XLVar -> InlineMonad ([XLVar], XLExpr)
inline_fun f = do
  (σi, σo) <- get
  case M.lookup f σo of
    Just v -> return v
    Nothing -> do
      case M.lookup f σi of
        Nothing -> error $ "Inline: Function unbound, or in cycle: " ++ show f
        Just (formals, fun_body) -> do
          let σi' = M.delete f σi
          put (σi', σo)
          (fp, fun_body') <- inline_expr fun_body
          let v = (fp, (formals, fun_body'))
          (σi'', σo') <- get
          let σo'' = M.insert f v σo'
          put (σi'', σo'')
          return v

inline_exprs :: [XLExpr] -> InlineMonad [XLExpr]
inline_exprs es = foldM (\(tp, es') e -> do
                            (ep, e') <- inline_expr e
                            return (tp && ep, e' : es'))
                  (True, []) (reverse es)

inline_expr :: XLExpr -> InlineMonad XLExpr
inline_expr e =
  case e of
    XL_Con _ -> return (True, e)
    XL_Var _ -> return (True, e)
    XL_PrimApp p es -> inline_exprs es >>= \(ep, es') -> return (ep, XL_PrimApp p es')
    XL_If _ ce te fe -> do
      (cp, ce') <- inline_expr ce
      (tp, te') <- inline_expr te
      (fp, fe') <- inline_expr fe
      return (cp && tp && fp, XL_If (tp && fp) ce' te' fe')
    XL_Assert ae -> do
      (_, ae') <- inline_expr ae
      --- Assert is impure because it could fail
      return (False, XL_Assert ae')
    XL_ToConsensus p ins pe ce -> do
      (_, pe') <- inline_expr pe
      (_, ce') <- inline_expr ce
      return (False, XL_ToConsensus p ins pe' ce')
    XL_FromConsensus be -> do
      (_, be') <- inline_expr be
      return (False, XL_FromConsensus be')
    XL_Values es -> inline_exprs es >>= \(ep, es') -> return (ep, XL_Values es')
    XL_Transfer to te -> do
      (_, tp') <- inline_expr te
      return (False, XL_Transfer to tp')
    XL_Declassify de -> do
      (dp, de') <- inline_expr de
      return (dp, XL_Declassify de')
    XL_LetValues mp mvs ve be -> do
      (vp, ve') <- inline_expr ve
      (bp, be') <- inline_expr be
      return (vp && bp, XL_LetValues mp mvs ve' be')
    XL_FunApp f args -> do
      (arp, args') <- inline_exprs args
      (fp, (formals, fun_body')) <- inline_fun f
      return (arp && fp, XL_LetValues Nothing (Just formals) (XL_Values args') fun_body')

inline_defs :: [XLDef] -> XLFuns -> XLExpr -> XLExpr
inline_defs [] σ me = me'
  where ((_, me'), _) = runState (inline_expr me) (σ, M.empty)
inline_defs (XL_DefineFun f args body : ds) σ me = inline_defs ds σ' me
  where σ' = M.insert f (args,body) σ
inline_defs (XL_DefineValues vs e : ds) σ me = inline_defs ds σ me'
  where me'= XL_LetValues Nothing (Just vs) e me

inline :: XLProgram -> XLInlinedProgram
inline (XL_Prog defs ps m) = XL_InlinedProg ps (inline_defs defs M.empty m)

{- ANF

   See AST for a description of the job of this pass.

   The ANF monad stores the next available variable and the list of
   defined variables.

 -}

type ANFElem = (Role, ILVar, ILExpr)
type ANFMonad a = State (Int, S.Seq ANFElem) a

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

consumeANF_N :: Int -> ANFMonad [ILVar]
consumeANF_N 0 = return []
consumeANF_N n = do
  v <- consumeANF ("ANF_N" ++ show n)
  vs <- consumeANF_N (n - 1)
  return $ v : vs

allocANF :: Role -> String -> ILExpr -> ANFMonad ILVar
allocANF mp s e = do
  (nvi, vs) <- get
  let nv = (nvi, s)
  put (nvi + 1, vs S.|> (mp, nv, e))
  return nv

allocANFs :: Role -> String -> [ILExpr] -> ANFMonad [ILVar]
allocANFs mp s es = mapM (allocANF mp s) es

type XLRenaming = M.Map XLVar ILArg

anf_parg :: (XLVar, BaseType) -> (XLRenaming, [(ILVar, BaseType)]) -> ANFMonad (XLRenaming, [(ILVar, BaseType)])
anf_parg (v, t) (ρ, args) =
  case M.lookup v ρ of
    Nothing -> do
      nv <- consumeANF v
      return (M.insert v (IL_Var nv) ρ, args' nv)
    Just (IL_Var nv) -> return (ρ, args' nv)
    Just _ -> error $ "ANF: Participant argument not bound to variable: " ++ v
  where args' nv = args ++ [(nv,t)]

anf_part :: (XLRenaming, ILPartInfo) -> (Participant, [(XLVar, BaseType)]) -> ANFMonad (XLRenaming, ILPartInfo)
anf_part (ρ, ips) (p, args) = do
  (ρ', args') <- foldrM anf_parg (ρ, []) args
  let ips' = M.insert p args' ips
  return (ρ', ips')

anf_parts :: XLPartInfo -> ANFMonad (XLRenaming, ILPartInfo)
anf_parts ps = foldM anf_part (M.empty, M.empty) (M.toList ps)

anf_exprs :: Role -> XLRenaming -> [XLExpr] -> ([ILArg] -> ANFMonad (Int, ILTail)) -> ANFMonad (Int, ILTail)
anf_exprs me ρ es mk =
  case es of
    [] -> mk []
    e : more ->
      anf_expr me ρ e k1
      where k1 [ e' ] = anf_exprs me ρ more k2
              where k2 es' = mk $ e' : es'
            k1 evs = error $ "anf_exprs, expect 1, got " ++ show evs

vsOnly :: [ILArg] -> [ILVar]
vsOnly [] = []
vsOnly (IL_Var v : m) = v : vsOnly m
vsOnly (_ : m) = vsOnly m

anf_renamed_to :: XLRenaming -> XLVar -> ILArg
anf_renamed_to ρ v =
  case M.lookup v ρ of
    Nothing -> error ("ANF: Variable unbound: " ++ (show v))
    Just a -> a

anf_out_rename :: XLRenaming -> [XLVar] -> ANFMonad (XLRenaming, [ILVar])
anf_out_rename ρ outs = foldM aor1 (ρ, []) (reverse outs)
  where aor1 (ρ', outs') ov =
          case M.lookup ov ρ' of
            Just (IL_Var iv) -> return (ρ', iv:outs')
            Just (IL_Con _) -> return (ρ', outs')
            Nothing -> do
              iv <- consumeANF "Consensus_Out"
              return (M.insert ov (IL_Var iv) ρ', iv:outs')

anf_expr :: Role -> XLRenaming -> XLExpr -> ([ILArg] -> ANFMonad (Int, ILTail)) -> ANFMonad (Int, ILTail)
anf_expr me ρ e mk =
  case e of
    XL_Con b ->
      mk [ IL_Con b ]
    XL_Var v -> mk [ anf_renamed_to ρ v ]
    XL_PrimApp p args ->
      anf_exprs me ρ args (\args' -> ret_expr "PrimApp" (IL_PrimApp p args'))
    XL_If is_pure ce te fe ->
      anf_expr me ρ ce k
      where k [ ca ] =
              if is_pure then
                anf_expr me ρ te
                  (\ tvs ->
                      anf_expr me ρ fe
                        (\ fvs ->
                           if (length tvs /= length fvs) then
                             error "ANF: If branches don't have same continuation arity"
                           else do
                             ks <- allocANFs me "PureIf" $ zipWith (\ t f -> IL_PrimApp (CP IF_THEN_ELSE) [ ca, t, f ]) tvs fvs
                             mk $ map IL_Var ks))
              else do
                (tn, tt) <- anf_tail me ρ te mk
                (fn, ft) <- anf_tail me ρ fe mk
                unless (tn == fn) $ error "ANF: If branches don't have same continuation arity"
                return (tn, IL_If ca tt ft)
            k _ = error "anf_expr XL_If ce doesn't return 1"
    XL_Assert ae ->
      anf_expr me ρ ae (\[ aa ] -> ret_expr "Assert" (IL_Assert aa))
    XL_FromConsensus le -> do
      (ln, lt) <- anf_tail RoleContract ρ le mk
      return (ln, IL_FromConsensus lt)
    XL_ToConsensus from ins pe ce ->
      anf_expr (RolePart from) ρ pe
      (\ [ pa ] -> do
         let ins' = vsOnly $ map (anf_renamed_to ρ) ins
         (cn, ct) <- anf_tail RoleContract ρ ce mk
         return (cn, IL_ToConsensus from ins' pa ct))
    XL_Values args ->
      anf_exprs me ρ args (\args' -> mk args')
    XL_Transfer to ae ->
      anf_expr me ρ ae (\[ aa ] -> ret_expr "Transfer" (IL_Transfer to aa))
    XL_Declassify ae ->
      anf_expr me ρ ae (\[ aa ] -> ret_expr "Declassify" (IL_Declassify aa))
    XL_LetValues mwho mvs ve be ->
      anf_expr who ρ ve k
      where who = case mwho of
                    Nothing -> me
                    Just p -> RolePart p
            k nvs = anf_expr me ρ' be mk
              where ρ' = M.union ρvs ρ
                    ρvs = case mvs of
                      Nothing -> ρ
                      Just ovs ->
                        let olen = length ovs
                            nlen = length nvs in
                        if olen == nlen then
                          (M.fromList $ zip ovs nvs)
                        else
                          error $ "ANF XL_LetValues, context arity mismatch, " ++ show olen ++ " vs " ++ show nlen
    XL_FunApp _ _ -> error $ "ANF XL_FunApp, impossible after inliner"
  where ret_expr s ne = do
          nv <- allocANF me s ne
          mk [ IL_Var nv ]

anf_addVar :: ANFElem -> (Int, ILTail) -> (Int, ILTail)
anf_addVar (mp, v, e) (c, t) = (c, IL_Let mp (Just v) e t)

anf_tail :: Role -> XLRenaming -> XLExpr -> ([ILArg] -> ANFMonad (Int, ILTail)) -> ANFMonad (Int, ILTail)
anf_tail me ρ e mk = do
  collectANF anf_addVar (anf_expr me ρ e mk)

anf_ktop :: [ILArg] -> ANFMonad (Int, ILTail)
anf_ktop args = return (length args, IL_Ret args)

anf :: XLInlinedProgram -> ILProgram
anf xilp = IL_Prog ips xt
  where
    XL_InlinedProg ps main = xilp
    (ips, xt) = runANF xm
    xm :: ANFMonad (ILPartInfo, ILTail)
    xm = do
      (ρ, nps) <- anf_parts ps
      (_, mt) <- anf_tail RoleContract ρ main anf_ktop
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

-}

data SecurityLevel
  = Secret
  | Public
  deriving (Show,Eq)

instance Semigroup SecurityLevel where
  Secret <> _ = Secret
  _ <> Secret = Secret
  Public <> Public = Public

instance Monoid SecurityLevel where
  mempty = Public

type SType = (BaseType, SecurityLevel)

type EPPEnv = M.Map Role (M.Map ILVar SType)
type EPPRes = (CTail, M.Map Participant EPTail, Int, [CHandler])

must_be_public :: (a, SType) -> (a, BaseType)
must_be_public (v, (et, Public)) = (v, et)
must_be_public (_, (_, Secret)) = error "EPP: Must be public"

epp_expect :: SType -> (a, SType) -> a
epp_expect est (a, ast) =
  if est == ast then a
  else error $ "EPP: Expected " ++ show est ++ ", got " ++ show ast

epp_var :: EPPEnv -> Role -> ILVar -> (BLVar, SType)
epp_var γ r iv = ((n, s, et), st)
  where (n,s) = iv
        env = case M.lookup r γ of
          Nothing -> error $ "EPP: Unknown role: " ++ show r
          Just v -> v
        (et, _) = st
        st = case M.lookup iv env of
          Nothing -> error $ "EPP: Role " ++ show r ++ " does not know " ++ show iv
          Just v -> v

epp_vars :: EPPEnv -> Role -> [ILVar] -> [(BLVar, SType)]
epp_vars γ r ivs = map (epp_var γ r) ivs

epp_arg :: EPPEnv -> Role -> ILArg -> (BLArg, SType)
epp_arg _ _ (IL_Con c) = (BL_Con c, (conType c, Public))
epp_arg γ r (IL_Var iv) = (BL_Var bv, st)
  where (bv, st) = epp_var γ r iv

epp_args :: EPPEnv -> Role -> [ILArg] -> [(BLArg, SType)]
epp_args γ r ivs = map (epp_arg γ r) ivs

epp_e_ctc :: EPPEnv -> ILExpr -> (SType, CExpr)
epp_e_ctc γ e = case e of
  IL_Declassify _ -> error "EPP: Contract cannot declassify"
  IL_Transfer r am -> (sBool, C_Transfer r $ eargt am AT_Int)
  IL_Assert a -> (sBool, C_Assert $ eargt a AT_Bool)
  IL_PrimApp p@(CP cp) args -> (sRet, C_PrimApp cp args')
    where args'st = map must_be_public $ map earg args
          args' = map fst args'st
          args't = map snd args'st
          ret = checkFun (primType p) args't
          sRet = (ret,Public)
  IL_PrimApp p _ -> error $ "EPP: Contract cannot execute: " ++ show p
 where sBool = (AT_Bool, Public)
       earg = epp_arg γ RoleContract
       eargt a expected = epp_expect (expected, Public) $ earg a

epp_e_loc :: EPPEnv -> Participant -> ILExpr -> (SType, EPExpr)
epp_e_loc γ p e = case e of
  IL_Declassify a -> ((et, Public), EP_Arg a')
    where (a', (et, _)) = earg a
  IL_Transfer _ _ -> error "EPP: Local cannot transfer"
  IL_Assert a -> (st', EP_Assert a')
    where (a', st@(_, slvl)) = earg a
          st' = epp_expect (AT_Bool, slvl) (st, st)
  IL_PrimApp pr args -> ((ret, slvl), EP_PrimApp pr args')
    where args'st = map earg args
          args't = map (fst . snd) args'st
          args' = map fst args'st
          ret = checkFun (primType pr) args't
          slvl = mconcat $ map (snd . snd) args'st
 where earg = epp_arg γ (RolePart p)

epp_e_ctc2loc :: CExpr -> EPExpr
epp_e_ctc2loc (C_PrimApp cp al) = (EP_PrimApp (CP cp) al)
epp_e_ctc2loc (C_Assert a) = (EP_Assert a)
epp_e_ctc2loc (C_Transfer _ _) = (EP_Arg (BL_Con (Con_B True)))

epp_it_ctc :: [Participant] -> EPPEnv -> Int -> ILTail -> EPPRes
epp_it_ctc ps γ hn0 it = case it of
  IL_Ret _ ->
    error "EPP: CTC cannot return"
  IL_If ca tt ft -> (C_If cca' ctt' cft', ts3, hn3, hs3)
    where cca' = epp_expect (AT_Bool, Public) $ epp_arg γ RoleContract ca
          (ctt', ts1, hn1, hs1) = epp_it_ctc ps γ hn0 tt
          (cft', ts2, hn2, hs2) = epp_it_ctc ps γ hn1 ft
          hn3 = hn2
          hs3 = hs1 ++ hs2
          ts3 = M.fromList $ map mkt ps
          mkt p = (p, EP_If ca' tt' ft')
            where ca' = epp_expect (AT_Bool, Public) $ epp_arg γ (RolePart p) ca
                  tt' = ts1 M.! p
                  ft' = ts2 M.! p
  IL_Let RoleContract what how next -> (C_Let what' how_ctc next', ts2, hn2, hs2)
    where (next', ts1, hn1, hs1) = epp_it_ctc ps γ' hn0 next
          hn2 = hn1
          hs2 = hs1
          (st, how_ctc) = epp_e_ctc γ how
          (et, _) = st
          (what', what'env) = case what of
                    Nothing -> (Nothing, M.empty)
                    Just (n, s) -> (Just (n, s, et), M.singleton (n,s) st)
          γ' = M.map (M.union what'env) γ
          how_ep = epp_e_ctc2loc how_ctc
          ts2 = M.map (EP_Let what' how_ep) ts1
  IL_Let (RolePart _) _ _ _ ->
    error "EPP: Cannot perform local binding in consensus"
  IL_ToConsensus _ _ _ _ ->
    error "EPP: Cannot transitions to consensus from consensus"
  IL_FromConsensus bt -> epp_it_loc ps γ hn0 bt

mep :: Role -> Role -> Bool
mep _ RoleContract = True
mep RoleContract _ = False
mep (RolePart x) (RolePart y) = x == y

epp_it_loc :: [Participant] -> EPPEnv -> Int -> ILTail -> EPPRes
epp_it_loc ps γ hn0 it = case it of
  IL_Ret al -> ( C_Halt, ts, hn0, [] )
    where ts = M.fromList $ map mkt ps
          mkt p = (p, EP_Ret $ map fst $ epp_args γ (RolePart p) al)
  IL_If _ _ _ ->
    error "EPP: Ifs must be consensual"
  IL_Let who what how next -> (ct1, ts2, hn1, hs1)
    where (ct1, ts1, hn1, hs1) = epp_it_loc ps γ' hn0 next
          γ' = case what of
                 Nothing -> γ
                 Just iv ->
                   M.mapWithKey addwhat γ
                   where addwhat r env =
                           if mep r who then
                             M.insert iv lst env
                           else
                             env
          lst = case fmst of
            Nothing -> error "EPP: Let not local to any participant"
            Just v -> v
          (fmst, ts2) = M.foldrWithKey addhow (Nothing, M.empty) ts1
          addhow p t (mst, ts) =
            if not (mep (RolePart p) who) then
              (mst, M.insert p t ts)
            else
              (mst', M.insert p t' ts)
              where t' = EP_Let mbv how' t
                    mst' = Just st
                    (st, how') = epp_e_loc γ p how
                    (et, _) = st
                    mbv = case what of
                      Nothing -> Nothing
                      Just (n, s) -> Just (n, s, et)
  IL_ToConsensus from what howmuch next -> (ct2, ts2, hn2, hs2)
    where fromr = RolePart from
          what' = map fst $ map must_be_public $ epp_vars γ fromr what
          howmuch' = epp_expect (AT_Int, Public) $ epp_arg γ fromr howmuch
          what'env = M.fromList $ map (\(n, s, et) -> ((n,s),(et,Public))) what'
          γ' = M.map (M.union what'env) γ
          hn1 = hn0 + 1
          (ct1, ts1, hn2, hs1) = epp_it_ctc ps γ' hn1 next
          nh = C_Handler what' ct1
          hs2 = nh : hs1
          ts2 = M.mapWithKey addTail ts1
          ct2 = C_Wait hn0
          es = EP_Send hn0 what' howmuch'
          addTail p pt1 = pt3
            where pt2 = EP_Recv hn0 what' pt1
                  pt3 = if p /= from then pt2
                        else EP_Let Nothing es pt2
  IL_FromConsensus _ ->
    error "EPP: Cannot transition to local from local"

epp :: ILProgram -> BLProgram
epp (IL_Prog ips it) = BL_Prog bps cp
  where cp = C_Prog chs
        ps = M.keys ips
        bps = M.mapWithKey mkep ets
        mkep p ept = EP_Prog args ept
          where args = map (\((n, s), et) -> (n,s,et)) $ ips M.! p
        (_, ets, _, chs) = epp_it_loc ps γ 0 it
        γi = M.fromList $ map initγ $ M.toList ips
        initγ (p, args) = (RolePart p, M.fromList $ map initarg args)
        initarg ((n, s), et) = ((n, s), (et, Secret))
        γ = M.insert RoleContract M.empty γi

{- Compilation to Solidity

   The handler program becomes a contract factory where the first
   interaction is tied into the contract creation. The contract has a
   different function for each consensus block. The arguments are the
   input variables. At the end, the output variables are emitted via
   an event.

   In the future, when we compile to EVM directly, it won't work that
   way though. Instead, we'll do dispatch ourselves.
 -}

emit_sol :: BLProgram -> Doc ann
emit_sol _ = pretty "pragma solidity ^0.5.2;" -- error $ "Solidity output is not implemented"

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

primZ3Runtime :: Z3.Z3 Z3.AST
primZ3Runtime =
  parseSMTLib2String $(embedStringFile "../../z3/z3-runtime.smt2") [] [] [] []

exprTypeZ3 :: ExprType -> Z3.Z3 Z3.Sort
exprTypeZ3 (TY_Con AT_Int) = Z3.mkIntSort
exprTypeZ3 (TY_Con AT_Bool) = Z3.mkBoolSort
exprTypeZ3 (TY_Con AT_Bytes) = error "z3 sort `Bytes`"
exprTypeZ3 (TY_Var _) = error "type variables not yet supported"

cPrimZ3NoargOp :: (Z3.Z3 Z3.AST) -> ([Z3.AST] -> Z3.Z3 Z3.AST)
cPrimZ3NoargOp op [] = op
cPrimZ3NoargOp _ lst =
  error ("no-arg operation: expected 0 arguments, received " ++ show (length lst))

cPrimZ3BinOp :: (Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST) -> ([Z3.AST] -> Z3.Z3 Z3.AST)
cPrimZ3BinOp op [a, b] = op a b
cPrimZ3BinOp _ lst =
  error ("binary operation: expected 2 arguments, received " ++ show (length lst))

cPrimZ3TernOp :: (Z3.AST -> Z3.AST -> Z3.AST -> Z3.Z3 Z3.AST) -> ([Z3.AST] -> Z3.Z3 Z3.AST)
cPrimZ3TernOp op [a, b, c] = op a b c
cPrimZ3TernOp _ lst =
  error ("ternary operation: expected 3 arguments, received " ++ show (length lst))

cPrimZ3Fun :: String -> FunctionType -> ([Z3.AST] -> Z3.Z3 Z3.AST)
cPrimZ3Fun fstr (TY_Arrow intys outty) ins =
  do fsym <- Z3.mkStringSymbol fstr
     insorts <- mapM exprTypeZ3 intys
     outsort <- exprTypeZ3 outty
     f <- Z3.mkFuncDecl fsym insorts outsort
     Z3.mkApp f ins
cPrimZ3Fun _ (TY_Forall _ _) _ =
  error "forall not yet supported"

cPrimZ3 :: C_Prim -> Bool -> [Z3.AST] -> Z3.Z3 Z3.AST
-- cPrimZ3 op dishon ins = out
-- relies on the declarations from `primZ3Runtime` being available
cPrimZ3 ADD _ = mkAdd
cPrimZ3 SUB _ = mkSub
cPrimZ3 MUL _ = mkMul
cPrimZ3 DIV _ = cPrimZ3BinOp mkDiv
cPrimZ3 MOD _ = cPrimZ3BinOp mkMod
cPrimZ3 PLT _ = cPrimZ3BinOp mkLt
cPrimZ3 PLE _ = cPrimZ3BinOp mkLe
cPrimZ3 PEQ _ = cPrimZ3BinOp mkEq
cPrimZ3 PGE _ = cPrimZ3BinOp mkGe
cPrimZ3 PGT _ = cPrimZ3BinOp mkGt
cPrimZ3 IF_THEN_ELSE _ = cPrimZ3TernOp mkIte
cPrimZ3 INT_TO_BYTES _ = cPrimZ3Fun "integer->integer-bytes" (primType (CP INT_TO_BYTES))
cPrimZ3 DIGEST _ = cPrimZ3Fun "digest" (primType (CP DIGEST))
cPrimZ3 BYTES_EQ _ = cPrimZ3BinOp mkEq
cPrimZ3 BYTES_LEN _ = cPrimZ3Fun "bytes-length" (primType (CP BYTES_LEN))
cPrimZ3 BCAT _ = cPrimZ3Fun "msg-cat" (primType (CP BCAT))
cPrimZ3 BCAT_LEFT _ = cPrimZ3Fun "msg-left" (primType (CP BCAT_LEFT))
cPrimZ3 BCAT_RIGHT _ = cPrimZ3Fun "msg-right" (primType (CP BCAT_RIGHT))
cPrimZ3 DISHONEST dishon = cPrimZ3NoargOp (mkBool dishon)

primZ3 :: EP_Prim -> Bool -> [Z3.AST] -> Z3.AST -> Z3.Z3 ()
-- primZ3 op dishon ins out = assertion
-- relies on the declarations from `primZ3Runtime` being available
primZ3 (CP op) dishon ins out =
  do op_call <- cPrimZ3 op dishon ins
     eq_call <- Z3.mkEq op_call out
     Z3.assert eq_call
primZ3 RANDOM _ [] _ =
  -- the arguments don't specify any constraint
  return ()
primZ3 RANDOM _ [x] o =
  -- the arguments specify 0 <= o < x
  do z <- Z3.mkInteger 0
     zleo <- Z3.mkLe z o
     oltx <- Z3.mkLt o x
     zleoltx <- Z3.mkAnd [zleo, oltx]
     Z3.assert zleoltx
primZ3 INTERACT _ [_] _ =
  -- no constraint
  return ()
primZ3 _ _ _ _ = error "XXX fill in Z3 primitives"

emit_z3 :: BLProgram -> Z3.Z3 [String]
emit_z3 _
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
  writeFile (srcp ++ ".xl") (show (pretty xlp))
  let xilp = inline xlp
  writeFile (srcp ++ ".xil") (show (pretty xilp))
  let ilp = anf xilp
  writeFile (srcp ++ ".il") (show (pretty ilp))
  let blp = epp ilp
  writeFile (srcp ++ ".bl") (show (pretty blp))
  z3res <- evalZ3 (emit_z3 blp)
  case z3res of
    [] -> do
      writeFile (srcp ++ ".sol") (show (emit_sol blp))
      writeFile (srcp ++ ".js") (Alacrity.EmitJS.emit_js blp)
      exitSuccess
    ps -> do
      mapM_ (\x -> putStrLn $ ("Z3 error:" ++ x)) ps
      die "Z3 failed to verify!"


