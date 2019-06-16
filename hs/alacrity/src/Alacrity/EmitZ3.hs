{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
module Alacrity.EmitZ3 where

import qualified Data.Map.Strict as M
import Data.FileEmbed
import Data.List (intersperse)
import Data.Text.Prettyprint.Doc
import Control.Monad

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

{- Z3 Printing
 -}

z3Apply :: String -> [Doc a] -> Doc a
z3Apply f [] = parens $ pretty f
z3Apply f args = parens $ pretty f <+> hsep args

z3Assert :: Doc a -> Doc a
z3Assert a = z3Apply "assert" [ a ]

z3Eq :: Doc a -> Doc a -> Doc a
z3Eq x y = z3Apply "=" [ x, y ]

z3Add :: Doc a -> Doc a -> Doc a
z3Add x y = z3Apply "+" [ x, y ]

z3Sub :: Doc a -> Doc a -> Doc a
z3Sub x y = z3Apply "-" [ x, y ]

z3Var :: ILVar -> Doc a
z3Var (n, _) = pretty $ "v" ++ show n

z3Declare :: Doc a -> Doc a -> Doc a
z3Declare v s = z3Apply "declare-const" [ v, s ]

z3DeclareEq :: Doc a -> Doc a -> Doc a -> Doc a
z3DeclareEq v s e = vsep [ z3Declare v s , z3Assert (z3Eq v e) ]

z3String :: String -> Doc a
z3String s = dquotes $ pretty $ filter (\c->c /= '"') s

z3Int :: Int -> Doc a
z3Int i = pretty i

z3Echo :: String -> Doc a
z3Echo s = z3Apply "echo" [ z3String s ]

z3Local :: Doc a -> Doc a
z3Local s = z3Apply "push" [] <>
            (nest 2 $ hardline <> s) <>
            hardline <> z3Apply "pop" []

z3IntSort :: Doc a
z3IntSort = z3_sortof AT_UInt256

z3_sortof :: BaseType -> Doc a
z3_sortof AT_UInt256 = pretty "Int"
z3_sortof AT_Bool = pretty "Bool"
z3_sortof AT_Bytes = pretty "Bytes"

z3_verify1 :: (Bool, Role, TheoremKind) -> Doc a -> Doc a
z3_verify1 (honest, r, tk) a = z3Local check
  where check = vsep
          [ z3Assert (z3Apply "not" [ a ])
          , z3Echo $ "Verifying honest = " ++ show honest ++ ", role = " ++ show r ++ ", tk = " ++ show tk ++ ", a = " ++ show a
          , z3Apply "check-sat" []
          , z3Apply "get-model" [] ]

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

   When #1 has to be done in both #2 and #3 modes, because the
   transfer amounts may rely on participant data.

 -}

lookie :: (Show k, Ord k) => String -> k -> M.Map k a -> a
lookie err k m = case M.lookup k m of
  Nothing -> error $ err ++ ": " ++ show k ++ " not in map"
  Just v -> v

z3CTCBalance :: Int -> Doc a
z3CTCBalance i = pretty $ "ctc_balance" ++ show i

z3CPrim :: Bool -> C_Prim -> [Doc a] -> Doc a
z3CPrim honest cp =
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
    DISHONEST -> \_ -> emit_z3_con (Con_B (not honest))
  where app n = z3Apply n

z3PrimEq :: Bool -> EP_Prim -> [Doc a] -> ILVar -> Doc a
z3PrimEq honest pr alt out = case pr of
  CP cp -> z3Assert (z3Eq (z3Var out) (z3CPrim honest cp alt))
  RANDOM -> emptyDoc
  INTERACT -> emptyDoc

data TheoremKind
  = TAssert
  | TBalanceZero
  deriving (Show)

type Theorem = (Bool, Role, TheoremKind)

emit_z3_con :: Constant -> Doc a
emit_z3_con (Con_I i) = pretty i
emit_z3_con (Con_B True) = pretty "true"
emit_z3_con (Con_B False) = pretty "false"
emit_z3_con (Con_BS _) = z3Apply "raw-bytes0" []

emit_z3_arg :: ILArg -> Doc a
emit_z3_arg (IL_Con c) = emit_z3_con c
emit_z3_arg (IL_Var v) = z3Var v

emit_z3_vardecl :: ILTypeMapm -> ILVar -> Doc a
emit_z3_vardecl tm iv = z3Declare (z3Var iv) s
  where bt = lookie "ILTypeMap" iv tm
        s = z3_sortof bt

emit_z3_expr :: Bool -> ILVar -> ILExpr -> Doc a
emit_z3_expr honest out how = case how of
  IL_Declassify a -> z3Assert (z3Eq (z3Var out) (emit_z3_arg a))
  IL_PrimApp pr al -> z3PrimEq honest pr alt out
    where alt = map emit_z3_arg al

emit_z3_stmt :: Bool -> Role -> Int -> ILStmt -> (Int, Doc a)
emit_z3_stmt honest r cbi how =
  case how of
    IL_Transfer _ amount -> (cbi', this)
      where cbi' = cbi + 1
            cb' = z3CTCBalance cbi'
            amountt = emit_z3_arg amount
            this = z3DeclareEq cb' z3IntSort (z3Sub (z3CTCBalance cbi) amountt)
    IL_Assert a -> (cbi, this)
      where at = emit_z3_arg a
            check = z3_verify1 (honest, r, TAssert) at --- XXX Add more information
            assert = z3Assert at
            this = vsep [ check, assert ]

emit_z3_it_top :: ILTypeMapm -> ILTail -> (Bool, Role) -> Doc a
emit_z3_it_top tm it_top (honest, me) =
  vsep [ z3Echo $ "Verifying with honest = " ++ show honest ++ "; role = " ++ show me
       , z3DeclareEq cb0 z3IntSort (z3Int 0)
       , iter 0 it_top ]
  where cb0 = z3CTCBalance 0
        iter :: Int -> ILTail -> Doc a
        iter cbi it = case it of
          IL_Ret _ ->
            z3_verify1 (honest, me, TBalanceZero) (z3Eq (z3CTCBalance cbi) (z3Int 0))
          IL_If ca tt ft ->
            vsep_with_blank $ map (z3Local . f) (zip [True, False] [tt, ft])
            where ca' = emit_z3_arg ca
                  f (v, kt) =
                    vsep [ z3Assert (z3Eq ca' cav)
                         , iter cbi kt ]
                    where cav = emit_z3_con (Con_B v)
          IL_Let who what how kt -> whatp <> this <> iter cbi kt
            where whatp = emit_z3_vardecl tm what <> hardline
                  this =
                    if (honest || role_me me who) then
                      emit_z3_expr honest what how <> hardline
                    else
                      emptyDoc
          IL_Do who how kt ->
            if (honest || role_me me who) then
              let (cbi', this) = emit_z3_stmt honest me cbi how in
                vsep [ this, iter cbi' kt ]
            else
              iter cbi kt
          IL_ToConsensus _who _msg amount kt ->
            vsep [ this, iter cbi' kt ]
            where cbi' = cbi + 1
                  cb' = z3CTCBalance cbi'
                  amountt = emit_z3_arg amount
                  this = z3DeclareEq cb' z3IntSort (z3Add (z3CTCBalance cbi) amountt)
          IL_FromConsensus kt -> iter cbi kt

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l

z3StdLib :: String
z3StdLib = $(embedStringFile "../../z3/z3-runtime.smt2")

emit_z3 :: ILProgram -> BLProgram -> Doc a
emit_z3 tp bp =
  vsep_with_blank $ pretty z3StdLib : pargs ++ map (z3Local . emit_z3_it_top tm it) (liftM2 (,) [True, False] ps)
  where (ITM tm) = rts bp
        IL_Prog ipi it = tp
        ps = RoleContract : (map RolePart $ M.keys ipi)
        pargs = concatMap (map (emit_z3_vardecl tm . fst) . snd) $ M.toList ipi
