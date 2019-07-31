{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Alacrity.VerifyZ3 where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Extra
import SimpleSMT --- Maybe use Language.SMTLib2 in future
import System.IO
import System.Exit
import Data.Text.Prettyprint.Doc
import Data.Digest.CRC32

import Alacrity.AST
import Alacrity.Util

{- Recover types for IL variables from BL Program

   The structure of IL is better for theory generation, but it isn't
   type-checked and we don't know the types of all the variables. So,
   we look at the BL to figure the types out and go from there. If the
   IL program would fail in EPP, then the BL theorem will be false (or
   meaningless.)

 -}

type ILTypeMapm = M.Map ILVar BaseType
newtype ILTypeMap = ITM ILTypeMapm

instance Semigroup ILTypeMap where
  (ITM m1) <> (ITM m2) = ITM (M.union m1 m2)

instance Monoid ILTypeMap where
  mempty = ITM M.empty

class RecoverTypes a where
  rts :: a -> ILTypeMap

instance (Foldable t, RecoverTypes a) => RecoverTypes (t a) where
  rts = foldMap rts

instance {-# OVERLAPPING #-} RecoverTypes BLVar where
  rts (n, s, bt) = ITM (M.singleton (n,s) bt)

instance RecoverTypes BLArg where
  rts (BL_Con _) = mempty
  rts (BL_Var bv) = rts bv

instance RecoverTypes EPExpr where
  rts (EP_Arg a) = rts a
  rts (EP_PrimApp _ al) = rts al

instance RecoverTypes EPStmt where
  rts (EP_Claim _ a) = rts a
  rts (EP_Send _ svs msg am) = rts svs <> rts msg <> rts am

instance RecoverTypes EPTail where
  rts (EP_Ret al) = rts al
  rts (EP_If ca tt ft) = rts ca <> rts tt <> rts ft
  rts (EP_Let bv ce ct) = rts bv <> rts ce <> rts ct
  rts (EP_Do cs ct) = rts cs <> rts ct
  rts (EP_Recv _ _ svs msg kt) = rts svs <> rts msg <> rts kt
  rts (EP_Loop _ loopv inita kt) = rts loopv <> rts inita <> rts kt
  rts (EP_Continue a) = rts a

instance RecoverTypes EProgram where
  rts (EP_Prog vs et) = rts vs <> rts et

instance RecoverTypes CExpr where
  rts (C_PrimApp _ vs) = rts vs

instance RecoverTypes CStmt where
  rts (C_Claim _ a) = rts a
  rts (C_Transfer _ a) = rts a

instance RecoverTypes CTail where
  rts (C_Halt) = mempty
  rts (C_Wait _ vs) = rts vs
  rts (C_If ca tt ft) = rts ca <> rts tt <> rts ft
  rts (C_Let bv ce ct) = rts bv <> rts ce <> rts ct
  rts (C_Do cs ct) = rts cs <> rts ct
  rts (C_Jump _ vs arg) = rts vs <> rts arg

instance RecoverTypes CHandler where
  rts (C_Handler _ svs msg ct) = rts svs <> rts msg <> rts ct
  rts (C_Loop svs arg ct) = rts svs <> rts arg <> rts ct

instance RecoverTypes CProgram where
  rts (C_Prog _ chs) = rts chs

instance RecoverTypes BLProgram where
  rts (BL_Prog bps cp) = rts bps <> rts cp

{- Z3 Printing -}

z3_sortof :: BaseType -> SExpr
z3_sortof AT_UInt256 = Atom "Int"
z3_sortof AT_Bool = Atom "Bool"
z3_sortof AT_Bytes = Atom "Bytes"

z3Apply :: String -> [SExpr] -> SExpr
z3Apply f args = List (Atom f : args)

z3Eq :: SExpr -> SExpr -> SExpr
z3Eq x y = z3Apply "=" [ x, y ]

z3Var :: ILVar -> String
z3Var (n, _) = "v" ++ show n

z3VarRef :: ILVar -> SExpr
z3VarRef v = Atom $ z3Var v

z3CTCBalance :: Int -> String
z3CTCBalance i = "ctc_balance" ++ show i

z3CTCBalanceRef :: Int -> SExpr
z3CTCBalanceRef i = Atom $ z3CTCBalance i

z3TxnValue :: Int -> String
z3TxnValue i = "txn_value" ++ show i

z3TxnValueRef :: Int -> SExpr
z3TxnValueRef i = Atom $ z3TxnValue i

z3IntSort :: SExpr
z3IntSort = z3_sortof AT_UInt256

{- Model Rendering -}

pretty_se :: SExpr -> Doc a
pretty_se (List l) = group $ parens $ hsep $ map pretty_se l
pretty_se (Atom a) = pretty a

pretty_se_top :: SExpr -> Doc a
pretty_se_top (List l) = group $ parens $ nest 2 $ vsep $ map pretty_se l
pretty_se_top (Atom a) = pretty a

{- Z3 Interaction -}

z3_verify1 :: Solver -> (Bool, Role, TheoremKind) -> SExpr -> IO VerifyResult
z3_verify1 z3 (_honest, _r, _tk) a = inNewScope z3 $ do
  assert z3 (z3Apply "not" [ a ])
  r <- check z3
  case r of
    Unknown -> error "Z3 inconclusive result"
    Unsat -> return $ VR 1 0
    Sat -> do
      putStrLn $ "Failed to verify! " ++ showsSExpr a ":"
      m <- command z3 $ List [ Atom "get-model" ]
      putStrLn $ show $ pretty_se_top m
      return $ VR 0 1

z3_sat1 :: Solver -> (Bool, Role, TheoremKind) -> SExpr -> IO VerifyResult
z3_sat1 z3 (_honest, _r, _tk) a = inNewScope z3 $ do
  assert z3 a
  r <- check z3
  case r of
    Unknown -> error "Z3 inconclusive result"
    Sat -> return $ VR 1 0
    Unsat -> do
      putStrLn $ "Failed to verify! " ++ showsSExpr a ":"
      uc <- getUnsatCore z3
      mapM_ putStrLn uc
      return $ VR 0 1

{- Z3 Theory Generation

   The Z3 theory has to prove a few different things.

   1. The balance of CTC at the end of the protocol is 0. It will have
   to do this by employing something like the State monad to represent
   all the various modifications to the CTC value overtime and assert
   that it is 0 at the end. This ensures that the protocol doesn't
   "leave anything on the table".

   2. Verify claims (see ClaimType for details)

   SMT-LIB is documented here:
   http://smtlib.cs.uiowa.edu/standard.shtml

 -}

lookie :: (Show k, Ord k) => String -> k -> M.Map k a -> a
lookie err k m = case M.lookup k m of
  Nothing -> error $ err ++ ": " ++ show k ++ " not in map"
  Just v -> v

z3CPrim :: Int -> C_Prim -> [SExpr] -> SExpr
z3CPrim cbi cp =
  case cp of
    ADD -> app "+"
    SUB -> app "-"
    MUL -> app "*"
    DIV -> app "div"
    MOD -> app "mod"
    PLT -> app "<"
    PLE -> app "<="
    PEQ -> app "="
    PGE -> app ">="
    PGT -> app ">"
    IF_THEN_ELSE -> app "ite"
    UINT256_TO_BYTES -> app "uint256->bytes"
    DIGEST -> app "digest"
    BYTES_EQ -> app "="
    BYTES_LEN -> app "bytes-length"
    BCAT -> app "msg-cat"
    BCAT_LEFT -> app "msg-left"
    BCAT_RIGHT -> app "msg-right"
    BALANCE -> \[] -> z3CTCBalanceRef cbi
    TXN_VALUE -> \[] -> z3TxnValueRef cbi
  where app n = z3Apply n

z3PrimEq :: Solver -> Int -> EP_Prim -> [SExpr] -> ILVar -> IO ()
z3PrimEq z3 cbi pr alt out = case pr of
  CP cp -> assert z3 (z3Eq (z3VarRef out) (z3CPrim cbi cp alt))
  RANDOM -> return ()
  INTERACT -> return ()

data TheoremKind
  = TAssert
  | TRequire
  | TPossible
  | TBalanceZero
  deriving (Show)

type Theorem = (Bool, Role, TheoremKind)

data VerifyResult = VR Int Int

instance Semigroup VerifyResult where
  (VR s1 f1) <> (VR s2 f2) = VR (s1 + s2) (f1 + f2)

instance Monoid VerifyResult where
  mempty = VR 0 0

emit_z3_con :: Constant -> SExpr
emit_z3_con (Con_I i) = Atom $ show i
emit_z3_con (Con_B True) = Atom "true"
emit_z3_con (Con_B False) = Atom "false"
emit_z3_con (Con_BS bs) = z3Apply "bytes-literal" [ Atom (show $ crc32 bs) ]

emit_z3_arg :: ILArg -> SExpr
emit_z3_arg (IL_Con c) = emit_z3_con c
emit_z3_arg (IL_Var v) = z3VarRef v

z3_vardecl :: Solver -> ILTypeMapm -> ILVar -> IO ()
z3_vardecl z3 tm iv = void $ declare z3 (z3Var iv) s
  where bt = lookie "ILTypeMap" iv tm
        s = z3_sortof bt

z3_expr :: Solver -> Int -> ILVar -> ILExpr -> IO ()
z3_expr z3 cbi out how = case how of
  IL_Declassify a ->
    assert z3 (z3Eq (z3VarRef out) (emit_z3_arg a))
  IL_PrimApp pr al -> z3PrimEq z3 cbi pr alt out
    where alt = map emit_z3_arg al

z3_stmt :: Solver -> Bool -> Role -> Int -> ILStmt -> IO (Int, VerifyResult)
z3_stmt z3 honest r cbi how =
  case how of
    IL_Transfer _ amount -> do void $ define z3 cb' z3IntSort (z3Apply "-" [ (z3CTCBalanceRef cbi), amountt ])
                               return (cbi', mempty)
      where cbi' = cbi + 1
            cb' = z3CTCBalance cbi'
            amountt = emit_z3_arg amount
    IL_Claim CT_Possible a -> do vr <- z3_sat1 z3 (honest, r, TPossible) at
                                 return ( cbi, vr )
      where at = emit_z3_arg a
    IL_Claim ct a -> do vr <- this_check
                        assert z3 at
                        return ( cbi, vr )
      where at = emit_z3_arg a
            this_check = case mct of
              Just tk -> z3_verify1 z3 (honest, r, tk) at
              Nothing -> return mempty
            mct = case ct of
              CT_Assert -> Just TAssert
              CT_Assume -> Nothing
              CT_Require | honest -> Just TRequire
              CT_Require -> Nothing
              CT_Possible -> error "Impossible"

z3_it_top :: Solver -> ILTypeMapm -> ILTail -> (Bool, Role) -> IO VerifyResult
z3_it_top z3 tm it_top (honest, me) = inNewScope z3 $ do
  putStrLn $ "Verifying with honest = " ++ show honest ++ "; role = " ++ show me
  void $ define z3 cb0 z3IntSort zero
  iter 0 it_top
  where zero = emit_z3_con (Con_I 0)
        cb0 = z3CTCBalance 0
        iter :: Int -> ILTail -> IO VerifyResult
        iter cbi it = case it of
          IL_Ret _ ->
            z3_verify1 z3 (honest, me, TBalanceZero) (z3Eq (z3CTCBalanceRef cbi) zero)
          IL_If ca tt ft ->
            mconcatMapM (inNewScope z3 . f) (zip [True, False] [tt, ft])
            where ca' = emit_z3_arg ca
                  f (v, kt) = do assert z3 (z3Eq ca' cav)
                                 iter cbi kt
                    where cav = emit_z3_con (Con_B v)
          IL_Let who what how kt ->
            do z3_vardecl z3 tm what
               when (honest || role_me me who) $ z3_expr z3 cbi what how
               iter cbi kt
          IL_Do who how kt ->
            if (honest || role_me me who) then
              do (cbi', vr) <- z3_stmt z3 honest me cbi how
                 vr' <- iter cbi' kt
                 return $ vr <> vr'
            else
              iter cbi kt
          IL_ToConsensus _who _msg amount kt ->
            do void $ declare z3 pvv z3IntSort
               void $ define z3 cb'v z3IntSort (z3Apply "+" [cbr, pvr])
               assert z3 thisc
               iter cbi' kt
            where cbi' = cbi + 1
                  cb'v = z3CTCBalance cbi'
                  cbr = z3CTCBalanceRef cbi
                  amountt = emit_z3_arg amount
                  pvv = z3TxnValue cbi'
                  pvr = z3TxnValueRef cbi'
                  thisc = if honest then
                            z3Eq pvr amountt
                          else
                            z3Apply "<=" [ zero, pvr ]
          IL_FromConsensus kt -> iter cbi kt
          IL_While _loopv _inita _untilt _invt _bodyt _kt ->
            error $ "XXX VerifyZ3 IL_While not implemented"
          IL_Continue _newa ->
            error $ "XXX VerifyZ3 IL_Continue not implemented"

z3StdLib :: String
z3StdLib = "../../z3/z3-runtime.smt2"

_verify_z3 :: Solver -> ILProgram -> BLProgram -> IO ExitCode
_verify_z3 z3 tp bp = do
  loadFile z3 z3StdLib
  mapM_ (mapM_ (z3_vardecl z3 tm . fst) . snd) $ M.toList ipi
  VR ss fs <- mconcatMapM (z3_it_top z3 tm it) (liftM2 (,) [True, False] ps)
  putStr $ "Checked " ++ (show $ ss + fs) ++ " theorems;"
  (if ( fs == 0 ) then
      do putStrLn $ " No failures!"
         return ExitSuccess
   else
      do putStrLn $ " " ++ show fs ++ " failures. :'("
         return $ ExitFailure 1)
  where (ITM tm) = rts bp
        IL_Prog ipi it = tp
        ps = RoleContract : (map RolePart $ M.keys ipi)

newFileLogger :: String -> IO (IO (), Logger)
newFileLogger p = do
  logh <- openFile p WriteMode
  let logLevel = return 0
      logSetLevel _ = return ()
      logTab = return ()
      logUntab = return ()
      logMessage m = do
        hPutStrLn logh m
        hFlush logh
      close = hClose logh
  return (close, Logger { .. })

verify_z3 :: String -> ILProgram -> BLProgram -> IO ()
verify_z3 logp tp bp = do
  (close, logpl) <- newFileLogger logp
  z3 <- newSolver "z3" ["-smt2", "-in"] (Just logpl)
  unlessM (produceUnsatCores z3) $ error "Prover doesn't support possible?"
  vec <- _verify_z3 z3 tp bp
  zec <- stop z3
  close
  maybeDie $ return zec
  maybeDie $ return vec
