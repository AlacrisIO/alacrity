module Alacrity.EmitSol where

import Data.List (intersperse)
import Data.Text.Prettyprint.Doc

import Alacrity.AST

{- Compilation to Solidity

   The handler program becomes a contract factory where the first
   interaction is tied into the contract creation. The contract has a
   different function for each consensus block. The arguments are the
   input variables. At the end, the output variables are emitted via
   an event.

   In the future, when we compile to EVM directly, it won't work that
   way though. Instead, we'll do dispatch ourselves.
 -}

solArgType :: BaseType -> String
solArgType AT_Unit = error "unit type cannot be directly emitted in Solidity"
solArgType AT_Int = "uint256"
solArgType AT_Bool = "bool"
solArgType AT_Bytes = "bytes calldata"

solVarType :: BaseType -> String
solVarType AT_Unit = error "unit type cannot be directly emitted in Solidity"
solVarType AT_Bool = "bool"
solVarType AT_Int = "uint256"
solVarType AT_Bytes = "bytes memory"

solBraces :: Doc a -> Doc a
solBraces body = braces (nest 2 $ hardline <> body <> space)

solFunction :: String -> [Doc a] -> Doc a -> Doc a -> Doc a
solFunction name args ret body =
  pretty "function" <+> solApply name args <+> ret <+> solBraces body

solEvent :: String -> [Doc a] -> Doc a
solEvent name args =
  pretty "event" <+> solApply name args <> semi

solDecl :: String -> Doc a -> Doc a
solDecl ty n = pretty ty <+> n

solVar :: BLVar -> Doc a
solVar (n, _, _) = pretty $ "v" ++ show n

solCon :: Constant -> Doc a
solCon (Con_I i) = pretty i
solCon (Con_B True) = pretty "true"
solCon (Con_B False) = pretty "false"
solCon (Con_BS s) = pretty $ "\"" ++ show s ++ "\""

solArg :: BLArg -> Doc a
solArg (BL_Var v) = solVar v
solArg (BL_Con c) = solCon c

solPartVar :: Participant -> Doc a
solPartVar p = pretty $ "p" ++ p

solFieldDecl :: BLVar -> Doc a
solFieldDecl bv@(_, _, bt) = solDecl (solType bt) (solVar bv)

solArgDecl :: BLVar -> Doc a
solArgDecl bv@(_, _, bt) = solDecl (solArgType bt) (solVar bv)

solVarDecl :: BLVar -> Doc a
solVarDecl bv@(_, _, bt) = solDecl (solVarType bt) (solVar bv)

solPartDecl :: Participant -> Doc a
solPartDecl p = solDecl "address payable" (solPartVar p)

solContract :: String -> Doc a -> Doc a
solContract s body = pretty "contract" <+> pretty s <+> solBraces body

solVersion :: Doc a
solVersion = pretty "pragma solidity ^0.5.2;"

solStdLib :: Doc a
solStdLib = pretty "import \"../sol/stdlib.sol\";"

solApply :: String -> [Doc a] -> Doc a
solApply f args = pretty f <> parens (hcat $ intersperse (comma <> space) args)

solRequire :: Doc a -> Doc a
solRequire a = solApply "require" [ a ]

solBinOp :: String -> Doc a -> Doc a -> Doc a
solBinOp o l r = l <+> pretty o <+> r

solEq :: Doc a -> Doc a -> Doc a
solEq = solBinOp "=="

solSet :: Doc a -> Doc a -> Doc a
solSet = solBinOp "="

solHash :: [Doc a] -> Doc a
solHash a = pretty "uint256(keccak256(" <+> solApply "abi.encode" a <+> pretty "))"

solHashState :: Int -> [Participant] -> [BLVar] -> Doc a
solHashState i ps svs = solHash $ (pretty (show i)) : (map solPartVar ps) ++ (map solVar svs)

solHandler :: [Participant] -> Int -> CHandler -> Doc a
solHandler ps i (C_Handler from svs msg body) = vsep [ evtp, funp ]
  where msgi = "msg" ++ show i
        msg_rs = map solVar msg
        msg_ds = map solArgDecl msg
        msg_eds = map solFieldDecl msg
        arg_ds = map solPartDecl ps ++ map solArgDecl svs ++ msg_ds
        evts = msgi ++ "_evt"
        evtp = solEvent evts msg_eds
        funp = solFunction (msgi ++ "_m") arg_ds retp bodyp
        retp = pretty "external payable"
        emitp = pretty "emit" <+> solApply evts msg_rs <> semi
        bodyp = vsep [ (solRequire $ solEq (pretty "current_state") (solHashState i ps svs)) <> semi,
                       solRequireSender from <> semi,
                       solCTail ps emitp body ]

solHandlers :: [Participant] -> [CHandler] -> Doc a
solHandlers ps hs = vsep $ intersperse emptyDoc $ zipWith (solHandler ps) [0..] hs

solRequireSender :: Participant -> Doc a
solRequireSender from = solRequire $ solEq (pretty "msg.sender") (solPartVar from)

solPrimApply :: C_Prim -> [Doc a] -> Doc a
solPrimApply pr args =
  case pr of
    ADD -> binOp "+"
    SUB -> binOp "-"
    MUL -> binOp "*"
    DIV -> binOp "/"
    MOD -> binOp "%"
    PLT -> binOp "<"
    PLE -> binOp "<="
    PEQ -> binOp "=="
    PGE -> binOp ">="
    PGT -> binOp ">"
    IF_THEN_ELSE -> case args of
                      [ c, t, f ] -> c <+> pretty "?" <+> t <+> pretty ":" <+> f
                      _ -> spa_error ()
    INT_TO_BYTES -> solApply "abi.encode" args
    DIGEST -> case args of
                [ a ] -> solHash [a]
                _ -> spa_error ()
    BYTES_EQ -> binOp "=="
    BYTES_LEN -> case args of
                   [ a ] -> a <> pretty ".length"
                   _ -> spa_error ()
    BCAT -> solApply "abi.encode" args
    BCAT_LEFT -> solApply "ALA_BCAT_LEFT" args -- XXX doesn't actually work!
    BCAT_RIGHT -> solApply "ALA_BCAT_RIGHT" args -- XXX doesn't actually work!
    DISHONEST -> case args of
                   [] -> solCon (Con_B True)
                   _ -> spa_error ()
  where binOp op = case args of
          [ l, r ] -> solBinOp op l r
          _ -> spa_error ()
        spa_error () = error "solPrimApply"

solCExpr :: CExpr -> Doc a
solCExpr (C_Assert a) = solRequire $ solArg a
solCExpr (C_Transfer p a) = solPartVar p <> pretty "." <> solApply "transfer" [ solArg a ]
solCExpr (C_PrimApp pr al) = solPrimApply pr $ map solArg al

solCTail :: [Participant] -> Doc a -> CTail -> Doc a
solCTail _ emitp (C_Halt) = vsep [ emitp,
                             solSet (pretty "current_state") (pretty "0x0") <> semi,
                             solApply "selfdestruct" [ solApply "address" [ pretty alacrisAddress ] ] <> semi ]
solCTail ps emitp (C_Wait i svs) = vsep [ emitp, (solSet (pretty "current_state") (solHashState i ps svs)) <> semi ]
solCTail ps emitp (C_If ca tt ft) =
  pretty "if" <+> parens (solArg ca) <> bp tt <> hardline <> pretty "else" <> bp ft
  where bp at = solBraces $ solCTail ps emitp at
solCTail ps emitp (C_Let Nothing ce ct) = vsep [ solCExpr ce <> semi, solCTail ps emitp ct ];
solCTail ps emitp (C_Let (Just bv) ce ct) = vsep [ solVarDecl bv <+> pretty "=" <+> solCExpr ce <> semi, solCTail ps emitp ct ];

emit_sol :: BLProgram -> Doc a
emit_sol (BL_Prog _ (C_Prog _ [])) =
  error "emit_sol: Cannot create contract with no consensus"
emit_sol (BL_Prog _ (C_Prog ps hs@(h1 : _))) =
  vsep $ [ solVersion, emptyDoc, --- XXX solStdLib,
           emptyDoc, factoryp, emptyDoc, ctcp ]
  where factoryp = solContract "ALAFactory" $ vsep [ createp ]
        ctcp = solContract "ALAContract" -- " is Stdlib"
          $ ctcbody
        ctcbody = vsep $ [state_defn, emptyDoc, consp, emptyDoc, solHandlers ps hs]
        consp = solApply "constructor" p_ds <+> pretty "public payable" <+> solBraces consbody
        consbody = solCTail ps emptyDoc (C_Wait 0 [])
        state_defn = pretty "uint256 current_state;"
        C_Handler _ _ msg _ = h1
        createp = solFunction "make" (p_ds ++ map solVarDecl msg) create_ret create_body
        create_ret = pretty "public payable returns (ALAContract _ctc)"
        p_ds = map solPartDecl ps
        p_rs = map solPartVar ps
        create_body =
          vsep [ pretty "ALAContract ctc = new " <> solApply "ALAContract" p_rs <> semi,
                 pretty "ctc." <> solApply "msg0_m.value(msg.value)" (p_rs ++ map solVar msg) <> semi,
                 pretty "return ctc;" ]
