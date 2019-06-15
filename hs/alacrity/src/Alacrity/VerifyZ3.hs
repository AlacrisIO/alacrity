{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Alacrity.VerifyZ3 where

import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Extra
import Z3.Monad as Z3
import System.Exit

import Alacrity.AST

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

--- XXX Find some existing Haskell class that we can derive to
--- automatically generate this stuff... maybe Data?
instance {-# OVERLAPPING #-} RecoverTypes BLVar where
  rts (n, s, bt) = ITM (M.singleton (n,s) bt)

instance RecoverTypes BLArg where
  rts (BL_Con _) = mempty
  rts (BL_Var bv) = rts bv

instance RecoverTypes EPExpr where
  rts (EP_Arg a) = rts a
  rts (EP_PrimApp _ al) = rts al

instance RecoverTypes EPStmt where
  rts (EP_Assert a) = rts a
  rts (EP_Send _ svs msg am) = rts svs <> rts msg <> rts am

instance RecoverTypes EPTail where
  rts (EP_Ret al) = rts al
  rts (EP_If ca tt ft) = rts ca <> rts tt <> rts ft
  rts (EP_Let bv ce ct) = rts bv <> rts ce <> rts ct
  rts (EP_Do cs ct) = rts cs <> rts ct
  rts (EP_Recv _ svs msg kt) = rts svs <> rts msg <> rts kt

instance RecoverTypes EProgram where
  rts (EP_Prog vs et) = rts vs <> rts et

instance RecoverTypes CExpr where
  rts (C_PrimApp _ vs) = rts vs

instance RecoverTypes CStmt where
  rts (C_Assert a) = rts a
  rts (C_Transfer _ a) = rts a

instance RecoverTypes CTail where
  rts (C_Halt) = mempty
  rts (C_Wait _ vs) = rts vs
  rts (C_If ca tt ft) = rts ca <> rts tt <> rts ft
  rts (C_Let bv ce ct) = rts bv <> rts ce <> rts ct
  rts (C_Do cs ct) = rts cs <> rts ct

instance RecoverTypes CHandler where
  rts (C_Handler _ svs msg ct) = rts svs <> rts msg <> rts ct

instance RecoverTypes CProgram where
  rts (C_Prog _ chs) = rts chs

instance RecoverTypes BLProgram where
  rts (BL_Prog bps cp) = rts bps <> rts cp

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

type Z3StdLib = M.Map String Z3.FuncDecl
type Z3SortMap = BaseType -> Z3.Z3 Z3.Sort

lookie :: (Show k, Ord k) => String -> k -> M.Map k a -> a
lookie err k m = case M.lookup k m of
  Nothing -> error $ err ++ ": " ++ show k ++ " not in map"
  Just v -> v

z3_cprim :: Z3StdLib -> Bool -> C_Prim -> [Z3.AST] -> Z3.Z3 Z3.AST
z3_cprim σ honest cp =
  case cp of 
    ADD -> Z3.mkAdd
    SUB -> Z3.mkSub
    MUL -> Z3.mkMul
    DIV -> bin Z3.mkDiv
    MOD -> bin Z3.mkMod
    PLT -> bin Z3.mkLt
    PLE -> bin Z3.mkLe
    PEQ -> bin Z3.mkEq
    PGE -> bin Z3.mkGe
    PGT -> bin Z3.mkGt
    IF_THEN_ELSE -> ter Z3.mkIte
    INT_TO_BYTES -> app "integer->integer-bytes"
    DIGEST -> app "digest"
    BYTES_EQ -> bin Z3.mkEq
    BYTES_LEN -> app "bytes-length"
    BCAT -> app "msg-cat"
    BCAT_LEFT -> app "msg-left"
    BCAT_RIGHT -> app "msg-right"
    DISHONEST -> nop (Z3.mkBool (not honest))
  where nop op [] = op
        nop _ _ = error "impossible"
        bin op [a, b] = op a b
        bin _ _ = error "impossible"
        ter op [a, b, c] = op a b c
        ter _ _ = error "impossible"
        app n = Z3.mkApp (lookie "Z3StdLib" n σ)

z3_primeq :: Z3StdLib -> Bool -> EP_Prim -> [Z3.AST] -> Z3.AST -> Z3.Z3 ()
z3_primeq σ honest pr alt out = case pr of
  CP cp -> do
    call <- z3_cprim σ honest cp alt
    z3_eq call out
  RANDOM -> case alt of
    [] -> return ()
    [ x ] -> do
      z <- Z3.mkInteger 0
      zleo <- Z3.mkLe z out
      oltx <- Z3.mkLt out x
      zleoltx <- Z3.mkAnd [zleo, oltx]
      Z3.assert zleoltx
    _ -> error "impossible"
  INTERACT -> return ()
  

data TheoremKind
  = TAssert
  | TBalanceZero
  deriving (Show)

type Theorem = (Bool, Role, TheoremKind)

data TheoremRes
  = TRSucc
  | TRFail (Maybe String)

type EmitRes = Z3.Z3 [(Theorem, TheoremRes)]

type Z3Rename = M.Map ILVar Z3.AST

emit_z3_con :: Z3StdLib -> Constant -> Z3.Z3 (Z3.AST)
emit_z3_con _ (Con_I i) = Z3.mkInteger i
emit_z3_con _ (Con_B True) = Z3.mkTrue
emit_z3_con _ (Con_B False) = Z3.mkFalse
emit_z3_con σ (Con_BS _) = Z3.mkApp (lookie "Z3StdLib" "raw-bytes0" σ) []

emit_z3_arg :: Z3StdLib -> Z3Rename -> ILArg -> Z3.Z3 (Z3.AST)
emit_z3_arg σ _ (IL_Con c) = emit_z3_con σ c
emit_z3_arg _ ρ (IL_Var v) = return $ lookie "Z3Rename" v ρ

emit_z3_vardecl :: Z3SortMap -> ILTypeMapm -> Z3Rename -> ILVar -> Z3.Z3 (Z3Rename, Z3.AST)
emit_z3_vardecl z3_sortof tm ρ iv = do
  let bt = lookie "ILTypeMap" iv tm
  s <- z3_sortof bt
  t <- Z3.mkFreshConst (show (prettyILVar iv)) s
  let ρ' = M.insert iv t ρ
  return (ρ', t)

emit_z3_expr :: Z3StdLib -> Bool -> Z3Rename -> Z3.AST -> ILExpr -> Z3.Z3 ()
emit_z3_expr σ honest ρ out how = case how of
  IL_Declassify a -> do
    at <- emit_z3_arg σ ρ a
    z3_eq at out
  IL_PrimApp pr al -> do
    alt <- mapM (emit_z3_arg σ ρ) al
    z3_primeq σ honest pr alt out

emit_z3_stmt :: Z3StdLib -> Bool -> Role -> Z3Rename -> Z3.AST -> ILStmt -> Z3.Z3 (Z3.AST, [(Theorem, TheoremRes)])
emit_z3_stmt σ honest r ρ cb how =
  case how of
    IL_Transfer _ amount -> do
      cb' <- Z3.mkFreshIntVar "ctc_balance"
      amountt <- emit_z3_arg σ ρ amount
      sub <- Z3.mkSub [ cb, amountt ]
      z3_eq cb' sub
      return (cb', [])
    IL_Assert a -> do
      at <- emit_z3_arg σ ρ a
      res <- z3_verify1 (honest, r, TAssert) at --- XXX Add more information
      Z3.assert at
      return (cb, res)

emit_z3_it_top :: Z3StdLib -> Z3SortMap -> ILTypeMapm -> Z3Rename -> ILTail -> (Bool, Role) -> EmitRes
emit_z3_it_top σ z3_sortof tm ρ_top it_top (honest, me) = do
  ctc_balance0 <- mkFreshIntVar "ctc_balance"
  iter ctc_balance0 ρ_top it_top
  where iter :: Z3.AST -> Z3Rename -> ILTail -> EmitRes
        iter cb ρ it = case it of
          IL_Ret _ -> do
            zero <- emit_z3_con σ (Con_I 0)
            cb_is_zero <- Z3.mkEq cb zero
            z3_verify1 (honest, me, TBalanceZero) cb_is_zero
          IL_If ca tt ft -> do
            cat <- emit_z3_arg σ ρ ca
            let f (v, kt) = do
                  cav <- emit_z3_con σ (Con_B v)
                  z3_eq cat cav
                  iter cb ρ kt
            concatMapM (local . f) (zip [True, False] [tt, ft])
          IL_Let who what how kt -> do
            (ρ', whatt) <- emit_z3_vardecl z3_sortof tm ρ what
            when (honest || role_me me who) (emit_z3_expr σ honest ρ whatt how)
            iter cb ρ' kt
          IL_Do who how kt ->
            if (honest || role_me me who) then
              do
                (cb', res) <- emit_z3_stmt σ honest me ρ cb how
                res' <- iter cb' ρ kt
                return $ res ++ res'
            else
              iter cb ρ kt
          IL_ToConsensus _who _msg amount kt -> do
            cb' <- Z3.mkFreshIntVar "ctc_balance"
            amountt <- emit_z3_arg σ ρ amount
            add <- Z3.mkAdd [ cb, amountt ]
            z3_eq cb' add
            iter cb' ρ kt
          IL_FromConsensus kt -> iter cb ρ kt

init_z3 :: Z3.Z3 (Z3SortMap, Z3StdLib)
init_z3 = do
  --- XXX It would be nice if we could do this by parsing a string,
  --- but I can't find a way to inspect the AST that comes out of the
  --- parsing.
  ---
  --- XXX This code is DYING for a library
  raw_sym <- Z3.mkStringSymbol "raw"
  raw_sort <- Z3.mkUninterpretedSort raw_sym
  raw_bytes_sym <- Z3.mkStringSymbol "raw-bytes"
  raw_bytes_huh_sym <- Z3.mkStringSymbol "raw-bytes?"
  raw_bytes_val_sym <- Z3.mkStringSymbol "raw-bytes-value"
  raw_bytes_con <- Z3.mkConstructor raw_bytes_sym raw_bytes_huh_sym [ (raw_bytes_val_sym, Just raw_sort, 0) ]
  int_sort <- Z3.mkIntSort
  int2bytes_sym <- Z3.mkStringSymbol "integer->integer-bytes"
  int2bytes_huh_sym <- Z3.mkStringSymbol "integer-bytes?"
  int2bytes_val_sym <- Z3.mkStringSymbol "integer-bytes-int"
  int2bytes_con <- Z3.mkConstructor int2bytes_sym int2bytes_huh_sym [ (int2bytes_val_sym, Just int_sort, 0) ]
  digest_sym <- Z3.mkStringSymbol "digest"
  digest_huh_sym <- Z3.mkStringSymbol "digest?"
  digest_val_sym <- Z3.mkStringSymbol "digest-value"
  digest_con <- Z3.mkConstructor digest_sym digest_huh_sym [ (digest_val_sym, Nothing, 0) ]
  msgcat_sym <- Z3.mkStringSymbol "msg-cat"
  msgcat_huh_sym <- Z3.mkStringSymbol "msg-cat?"
  msgcat_left_sym <- Z3.mkStringSymbol "msg-cat-left"
  msgcat_right_sym <- Z3.mkStringSymbol "msg-cat-right"
  msgcat_con <- Z3.mkConstructor msgcat_sym msgcat_huh_sym [ (msgcat_left_sym, Nothing, 0), (msgcat_right_sym, Nothing, 0) ]
  let bytes_cons = [ raw_bytes_con, digest_con, int2bytes_con, msgcat_con ]
  bytes_sym <- Z3.mkStringSymbol "bytes"
  bytes_sort <- Z3.mkDatatype bytes_sym bytes_cons
  bytes_funcs <- Z3.getDatatypeSortConstructors bytes_sort
  let [ _raw_bytes_func, digest_func, int2bytes_func, msgcat_func ] = bytes_funcs
      σ = M.fromList [ ("digest",digest_func)
                     , ("integer->integer-bytes",int2bytes_func)
                     , ("msg-cat",msgcat_func)
                     --- XXX , ("msg-left",msgleft_func)
                     --- XXX , ("msg-right",msgright_func)
                     --- XXX , ("bytes-length",bytes_len_func)
                     --- XXX , ("raw-bytes0",raw_bytes0_func)
                     ]
      z3_sortof :: BaseType -> Z3.Z3 Z3.Sort
      z3_sortof AT_Int = Z3.mkIntSort
      z3_sortof AT_Bool = Z3.mkBoolSort
      z3_sortof AT_Bytes = return bytes_sort
  return (z3_sortof, σ)

emit_z3 :: ILTypeMapm -> ILProgram -> EmitRes
emit_z3 tm tp = do
  (z3_sortof, σ) <- init_z3
  let parg :: Z3Rename -> (ILVar, BaseType) -> Z3.Z3 Z3Rename
      parg ρ (iv, _) = liftM fst $ emit_z3_vardecl z3_sortof tm ρ iv
  ρ <- foldM (foldM parg) M.empty ipi
  concatMapM (local . emit_z3_it_top σ z3_sortof tm ρ it) (liftM2 (,) [True, False] ps)
  where IL_Prog ipi it = tp
        ps = RoleContract : (map RolePart $ M.keys ipi)

z3_eq :: Z3.AST -> Z3.AST -> Z3 ()
z3_eq x y = do
  eq <- Z3.mkEq x y
  Z3.assert eq

z3_verify1 :: Theorem -> Z3.AST -> EmitRes
z3_verify1 t a = do  
  Z3.push
  f <- Z3.mkNot a
  Z3.assert f
  (res, mm) <- Z3.solverCheckAndGetModel
  Z3.pop 1
  case res of
    Z3.Unsat -> return [(t, TRSucc)]
    Z3.Sat -> do
      ms <- forM mm modelToString
      return [(t, TRFail ms)]
    Z3.Undef -> error "Z3: Undefined"

check_z3 :: Z3.Z3 [(Theorem, TheoremRes)] -> IO ()
check_z3 z3p = do
  putStrLn ""
  putStrLn "Verifying..."
  z3res <- evalZ3 z3p
  (ss, fs) <- foldM f (0,0) z3res
  putStrLn $ "Checked " ++ show (ss + fs) ++ " theorem(s); " ++ show fs ++ " failure(s)"
  when (fs > 0) $ die "Failed to verify!"
  return ()
  where f :: (Integer,Integer) -> (Theorem, TheoremRes) -> IO (Integer,Integer)
        f (ss, fs) (t, TRSucc) = do
          putStrLn $ "Verified: " ++ show t
          return (ss+1, fs)
        f (ss, fs) (t, TRFail ms) = do
          putStrLn $ "Falsified: " ++ show t ++ " with " ++ show ms
          return (ss, fs+1)

verify_z3 :: ILProgram -> BLProgram -> IO ()
verify_z3 ip bp = check_z3 (emit_z3 itm ip)
  where (ITM itm) = rts bp
