{-# LANGUAGE TemplateHaskell #-}
module Alacrity.VerifyZ3 where

import Data.FileEmbed
import Z3.Monad as Z3
import System.Exit

import Alacrity.AST

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

emit_z3 :: ILProgram -> Z3.Z3 [String]
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

check_z3 :: Z3.Z3 [String] -> IO ()
check_z3 z3p = do
  z3res <- evalZ3 z3p
  case z3res of
    [] -> return ()
    ps -> do
      mapM_ (\x -> putStrLn $ ("Z3 error:" ++ x)) ps
      die "Z3 failed to verify!"


verify_z3 :: ILProgram -> IO ()
verify_z3 p = check_z3 (emit_z3 p)
