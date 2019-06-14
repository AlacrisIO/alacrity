module Alacrity.EmitSol where

import Data.List (intersperse)
import Data.Text.Prettyprint.Doc
import qualified Data.Map.Strict as M

import Alacrity.AST

{- AST add-ons
 -}

solMsg_evt :: Show i => i -> String
solMsg_evt i = "msg" ++ show i ++ "_evt"

solMsg_fun :: Show i => i -> String
solMsg_fun i = "msg" ++ show i ++ "_m"

solType :: BaseType -> String
solType AT_Int = "uint256"
solType AT_Bool = "bool"
solType AT_Bytes = "bytes"

alacrisAddress :: String
alacrisAddress = "0x02B463784Bc1a49f1647B47a19452aC420DFC65A"


{- De-ANF information
 -}

type CCounts = M.Map BLVar Int

cmerge :: CCounts -> CCounts -> CCounts
cmerge m1 m2 = M.unionWith (+) m1 m2

cmerges :: [CCounts] -> CCounts
cmerges [] = M.empty
cmerges (m1:ms) = cmerge m1 $ cmerges ms

usesBLArg :: BLArg -> CCounts
usesBLArg (BL_Con _) = M.empty
usesBLArg (BL_Var bv) = M.singleton bv 1

usesCExpr :: CExpr -> CCounts
usesCExpr (C_PrimApp _ al) = cmerges $ map usesBLArg al

usesCStmt :: CStmt -> CCounts
usesCStmt (C_Assert a) = usesBLArg a
usesCStmt (C_Transfer _ a) = usesBLArg a

usesCTail :: CTail -> (CCounts, CTail)
usesCTail ct@(C_Halt) = (M.empty, ct)
usesCTail ct@(C_Wait _ vs) = (cs, ct)
  where cs = M.fromList $ map (\v->(v,1)) vs
usesCTail (C_If ca tt ft) = (cmerges [ cs1, cs2, cs3 ], (C_If ca tt' ft'))
  where cs1 = usesBLArg ca
        (cs2, tt') = usesCTail tt
        (cs3, ft') = usesCTail ft
usesCTail (C_Let bv ce _ kt) = (cs3, (C_Let bv ce (M.lookup bv cs2) kt'))
  where cs1 = usesCExpr ce
        (cs2, kt') = usesCTail kt
        cs3 = cmerge cs1 (M.delete bv cs2)
usesCTail (C_Do cs kt) = (cs3, (C_Do cs kt'))
  where cs1 = usesCStmt cs
        (cs2, kt') = usesCTail kt
        cs3 = cmerge cs1 cs2
  
usesCHandler :: CHandler -> CHandler
usesCHandler (C_Handler p svs msg ct) = C_Handler p svs msg ct'
  where (_, ct') = usesCTail ct

usesCProgram :: CProgram -> CProgram
usesCProgram (C_Prog ps hs) = C_Prog ps $ map usesCHandler hs

{- Compilation to Solidity

   The handler program becomes a contract factory where the first
   interaction is tied into the contract creation. The contract has a
   different function for each consensus block. The arguments are the
   input variables. At the end, the output variables are emitted via
   an event.

   In the future, when we compile to EVM directly, it won't work that
   way though. Instead, we'll do dispatch ourselves.
 -}

type SolRenaming a = M.Map BLVar (Doc a)

solArgType :: BaseType -> String
solArgType AT_Bytes = "bytes calldata"
solArgType t = solType t

solVarType :: BaseType -> String
solVarType AT_Bytes = "bytes memory"
solVarType t = solType t

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

solRawVar :: BLVar -> Doc a
solRawVar (n, _, _) = pretty $ "v" ++ show n

solVar :: SolRenaming a -> BLVar -> Doc a
solVar ρ bv = p
  where p = case M.lookup bv ρ of
              Nothing -> solRawVar bv
              Just v -> v

solCon :: Constant -> Doc a
solCon (Con_I i) = pretty i
solCon (Con_B True) = pretty "true"
solCon (Con_B False) = pretty "false"
solCon (Con_BS s) = pretty $ "\"" ++ show s ++ "\""

solArg :: SolRenaming a -> BLArg -> Doc a
solArg ρ (BL_Var v) = solVar ρ v
solArg _ (BL_Con c) = solCon c

solPartVar :: Participant -> Doc a
solPartVar p = pretty $ "p" ++ p

solFieldDecl :: BLVar -> Doc a
solFieldDecl bv@(_, _, bt) = solDecl (solType bt) (solRawVar bv)

solArgDecl :: BLVar -> Doc a
solArgDecl bv@(_, _, bt) = solDecl (solArgType bt) (solRawVar bv)

solVarDecl :: BLVar -> Doc a
solVarDecl bv@(_, _, bt) = solDecl (solVarType bt) (solRawVar bv)

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

solHashState :: SolRenaming a -> Int -> [Participant] -> [BLVar] -> Doc a
solHashState ρ i ps svs = solHash $ (pretty (show i)) : (map solPartVar ps) ++ (map (solVar ρ) svs)

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
    BCAT -> solApply "abi.encode" args --- XXX switch to ALA_BCAT after below
    BCAT_LEFT -> solApply "ALA_BCAT_LEFT" args
    BCAT_RIGHT -> solApply "ALA_BCAT_RIGHT" args
    DISHONEST -> case args of
                   [] -> solCon (Con_B True)
                   _ -> spa_error ()
  where binOp op = case args of
          [ l, r ] -> solBinOp op l r
          _ -> spa_error ()
        spa_error () = error "solPrimApply"

solCExpr :: SolRenaming a -> CExpr -> Doc a
solCExpr ρ (C_PrimApp pr al) = solPrimApply pr $ map (solArg ρ) al

solCStmt :: SolRenaming a -> CStmt -> Doc a
solCStmt ρ (C_Assert a) = solRequire $ solArg ρ a
solCStmt ρ (C_Transfer p a) = solPartVar p <> pretty "." <> solApply "transfer" [ solArg ρ a ]

solCTail :: [Participant] -> Doc a -> SolRenaming a -> CTail -> Doc a
solCTail _ emitp _ (C_Halt) = vsep [ emitp,
                             solSet (pretty "current_state") (pretty "0x0") <> semi,
                             solApply "selfdestruct" [ solApply "address" [ pretty alacrisAddress ] ] <> semi ]
solCTail ps emitp ρ (C_Wait i svs) = vsep [ emitp, (solSet (pretty "current_state") (solHashState ρ i ps svs)) <> semi ]
solCTail ps emitp ρ (C_If ca tt ft) =
  pretty "if" <+> parens (solArg ρ ca) <> bp tt <> hardline <> pretty "else" <> bp ft
  where bp at = solBraces $ solCTail ps emitp ρ at
solCTail _ _ _ (C_Let _ _ Nothing _) = error "emit_sol: requires use counts to de-ANF"
solCTail ps emitp ρ (C_Let _ _ (Just 0) ct) = solCTail ps emitp ρ ct
solCTail ps emitp ρ (C_Let bv ce (Just 1) ct) = solCTail ps emitp ρ' ct
  where ρ' = M.insert bv (parens (solCExpr ρ ce)) ρ 
solCTail ps emitp ρ (C_Let bv ce _ ct) = vsep [ solVarDecl bv <+> pretty "=" <+> solCExpr ρ ce <> semi, solCTail ps emitp ρ ct ]
solCTail ps emitp ρ (C_Do cs ct) = vsep [ solCStmt ρ cs <> semi, solCTail ps emitp ρ ct ]

solHandler :: [Participant] -> Int -> CHandler -> Doc a
solHandler ps i (C_Handler from svs msg body) = vsep [ evtp, funp ]
  where msg_rs = map solRawVar msg
        msg_ds = map solArgDecl msg
        msg_eds = map solFieldDecl msg
        arg_ds = map solPartDecl ps ++ map solArgDecl svs ++ msg_ds
        evts = solMsg_evt i
        evtp = solEvent evts msg_eds
        funp = solFunction (solMsg_fun i) arg_ds retp bodyp
        retp = pretty "external payable"
        emitp = pretty "emit" <+> solApply evts msg_rs <> semi
        bodyp = vsep [ (solRequire $ solEq (pretty "current_state") (solHashState M.empty i ps svs)) <> semi,
                       solRequireSender from <> semi,
                       solCTail ps emitp M.empty body ]

solHandlers :: [Participant] -> [CHandler] -> Doc a
solHandlers ps hs = vsep $ intersperse emptyDoc $ zipWith (solHandler ps) [0..] hs

emit_sol :: BLProgram -> Doc a
emit_sol (BL_Prog _ (C_Prog _ [])) =
  error "emit_sol: Cannot create contract with no consensus"
emit_sol (BL_Prog _ cp) =
  vsep $ [ solVersion, --- XXX emptyDoc, solStdLib,
           emptyDoc, factoryp, emptyDoc, ctcp ]
  where (C_Prog ps hs@(h1 : _)) = usesCProgram cp
        factoryp = solContract "ALAFactory" $ vsep [ createp ]
        ctcp = solContract "ALAContract" -- " is Stdlib"
          $ ctcbody
        ctcbody = vsep $ [state_defn, emptyDoc, consp, emptyDoc, solHandlers ps hs]
        consp = solApply "constructor" p_ds <+> pretty "public payable" <+> solBraces consbody
        consbody = solCTail ps emptyDoc M.empty (C_Wait 0 [])
        state_defn = pretty "uint256 current_state;"
        C_Handler _ _ msg _ = h1
        createp = solFunction "make" (p_ds ++ map solVarDecl msg) create_ret create_body
        create_ret = pretty "public payable returns (ALAContract _ctc)"
        p_ds = map solPartDecl ps
        p_rs = map solPartVar ps
        create_body =
          vsep [ pretty "ALAContract ctc = new " <> solApply "ALAContract" p_rs <> semi,
                 pretty "ctc." <> solApply "msg0_m.value(msg.value)" (p_rs ++ map solRawVar msg) <> semi,
                 pretty "return ctc;" ]
