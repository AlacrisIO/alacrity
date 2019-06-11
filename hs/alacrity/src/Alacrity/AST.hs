module Alacrity.AST where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Data.Text.Prettyprint.Doc

-- Shared types

data BaseType
  = AT_Int
  | AT_Bool
  | AT_Bytes
  deriving (Show,Eq)
data ExprType
  = TY_Var String
  | TY_Con BaseType
  deriving (Show,Eq)
data FunctionType
  = TY_Arrow [ExprType] ExprType
  | TY_Forall [String] FunctionType
  deriving (Show,Eq)

tBool :: ExprType
tInt :: ExprType
tBytes :: ExprType
(-->) :: [ExprType] -> ExprType -> FunctionType
tBool = TY_Con AT_Bool
tInt = TY_Con AT_Int
tBytes = TY_Con AT_Bytes
ins --> out = TY_Arrow ins out

data Constant
  = Con_I Integer
  | Con_B Bool
  | Con_BS B.ByteString
  deriving (Show,Eq)

-- -- Primitives are divided into ones that the contract can do and
-- -- ones that endpoints can do.

data C_Prim
  = ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | PLT
  | PLE
  | PEQ
  | PGE
  | PGT
  | IF_THEN_ELSE
  | INT_TO_BYTES
  | DIGEST
  | BYTES_EQ
  | BYTES_LEN
  | BCAT
  | BCAT_LEFT
  | BCAT_RIGHT
  | DISHONEST
  deriving (Show,Eq)

data EP_Prim
  = CP C_Prim
  | RANDOM
  | INTERACT
  deriving (Show,Eq)

primType :: EP_Prim -> FunctionType
primType (CP ADD) = [tInt, tInt] --> tInt
primType (CP SUB) = [tInt, tInt] --> tInt
primType (CP MUL) = [tInt, tInt] --> tInt
primType (CP DIV) = [tInt, tInt] --> tInt
primType (CP MOD) = [tInt, tInt] --> tInt
primType (CP PLT) = [tInt, tInt] --> tBool
primType (CP PLE) = [tInt, tInt] --> tBool
primType (CP PEQ) = [tInt, tInt] --> tBool
primType (CP PGE) = [tInt, tInt] --> tBool
primType (CP PGT) = [tInt, tInt] --> tBool
primType (CP IF_THEN_ELSE) = TY_Forall ["a"] ([tBool, TY_Var "a", TY_Var "a"] --> TY_Var "a")
primType (CP INT_TO_BYTES) = [tInt] --> tBytes
primType (CP DIGEST) = ([tBytes] --> tBytes)
primType (CP BYTES_EQ) = [tBytes, tBytes] --> tBool
primType (CP BYTES_LEN) = [tBytes] --> tInt
primType (CP BCAT)       = ([tBytes, tBytes] --> tBytes)
primType (CP BCAT_LEFT)  = ([tBytes] --> tBytes)
primType (CP BCAT_RIGHT) = ([tBytes] --> tBytes)
primType (CP DISHONEST) = ([] --> tBool)
primType RANDOM = ([] --> tInt)
primType INTERACT = ([tBytes] --> tBytes)

type Participant = String

data Role
  = RolePart Participant
  | RoleContract
  deriving (Show,Eq,Ord)

{- Surface Language

 -}

{- Expanded Language (the language after expansion)

   There are some extensions we need to add in the future:

 -}

type XLVar = String

data XLExpr
  = XL_Con Constant
  | XL_Var XLVar
  | XL_PrimApp EP_Prim [XLExpr]
  | XL_If Bool XLExpr XLExpr XLExpr
  | XL_Assert XLExpr
  --- A ToConsensus transfers control to the contract. The arguments
  --- are (initiator, message, pay expression, contract body). The
  --- message is a sequence of variables, because it binds these in
  --- the contract body. The contract body is expected to end in a
  --- FromConsensus that will switch back.
  | XL_ToConsensus Participant [XLVar] XLExpr XLExpr
  --- A FromConsensus expression is a terminator inside of a contract
  --- block that switches the context back away from the consensus,
  --- while still retaining all of the bindings established during the
  --- consensus execution.
  | XL_FromConsensus XLExpr
  | XL_Values [XLExpr]
  --- Transfer expressions are always from the contract to another
  --- role. In the future, we could make something like mutable state
  --- on a local side of a transaction that collects all the transfers
  --- and puts them in the pay position.
  | XL_Transfer Role XLExpr
  | XL_Declassify XLExpr
  --- Where x Vars x Expression x Body
  | XL_LetValues (Maybe Participant) (Maybe [XLVar]) XLExpr XLExpr
  --- Impossible in inlined
  | XL_FunApp XLVar [XLExpr]
  deriving (Show,Eq)

data XLDef
  = XL_DefineValues [XLVar] XLExpr
  | XL_DefineFun XLVar [XLVar] XLExpr
  deriving (Show,Eq)

type XLPartInfo = (M.Map Participant [(XLVar, ExprType)])

data XLProgram =
  XL_Prog [XLDef] XLPartInfo XLExpr
  deriving (Show,Eq)

data XLInlinedProgram =
  XL_InlinedProg XLPartInfo XLExpr
  deriving (Show,Eq)

{- Intermediate Language

   This language is the result of ANF. It is stratified so that all
   expressions receive simple arguments (constants or variables). The
   most complex thing is that consensus blocks have been transformed
   into binding by observing their continuation and embedding
   it.

   Another subtlety is that IF blocks embed their continuation, which
   means if an IF is not in tail position, then it duplicates the
   continuation. The ANF transform should track whether expressions
   are pure and turn them into ITE expressions to limit how much this
   occurs.

   This language is NOT guaranteed to be type-correct.

   It is essential that all participants agree on the number of times
   consensus is reached. This means that ANF has to do another complex
   job: it must ensure that IFs are consensual. 
 -}

--- The string is just for debugging, it tracks where the variable was
--- created.
type ILVar = (Int, String)

data ILArg
  = IL_Con Constant
  | IL_Var ILVar
  deriving (Show,Eq)

data ILExpr
  = IL_PrimApp EP_Prim [ILArg]
  | IL_Declassify ILArg
  | IL_Transfer Role ILArg
  | IL_Assert ILArg
  deriving (Show,Eq)

data ILTail
  = IL_Ret [ILArg]
  | IL_If ILArg ILTail ILTail
  --- This role represents where the action happens. If it is
  --- RoleContract, then this means that everyone does it.
  | IL_Let Role (Maybe ILVar) ILExpr ILTail
  --- As in XL, a ToConsensus is a transfer to the contract with
  --- (initiator, message, pay amount). The tail is inside of the
  --- contract.
  | IL_ToConsensus Participant [ILVar] ILArg ILTail
  --- A FromConsensus moves back from the consensus; the tail is
  --- "local" again.
  | IL_FromConsensus ILTail
  deriving (Show,Eq)

type ILPartInfo = (M.Map Participant [(ILVar, ExprType)])

data ILProgram =
  IL_Prog ILPartInfo ILTail
  deriving (Show,Eq)

{- Backend Language

   These languages are the result of EPP. Like ANF, they are stratified,
   but there are two categories: one for the end-points (the
   participants) and one for the contract.

   Consensus blocks in IL are transformed into
   1. A receive from the contract in all participants.
   2. A send to the contract in the originator.
   3. A handler block in the contract.

   During EPP, the total number of consensus blocks are tracked and
   tagged with integers, so that all participants can agree on which
   block is going to run.

   -}

type BLVar = (Int, String, ExprType)

data BLArg
  = BL_Con Constant
  | BL_Var BLVar
  deriving (Show,Eq)

-- -- End-Points
data EPExpr
  = EP_PrimApp EP_Prim [BLArg]
  | EP_Assert BLArg
  | EP_Send Int [BLVar] BLArg
  deriving (Show,Eq)

data EPTail
  = EP_Ret [BLArg]
  | EP_If BLArg EPTail EPTail
  | EP_Let (Maybe BLVar) EPExpr EPTail
  {- This recv is what the sender sent; we will be doing the same
     computation as the contract. -}
  | EP_Recv Int [BLVar] EPTail
  deriving (Show,Eq)

data EProgram
  = EP_Prog [BLVar] EPTail
  deriving (Show,Eq)

-- -- Contracts
data CExpr
  = C_PrimApp C_Prim [BLArg]
  | C_Assert BLArg
  | C_Transfer Role BLArg
  deriving (Show,Eq)

data CTail
  {- This return represents what the contract sends out in response to
     this handler. In the future, I believe this is unnecessary
     because observes of this handler call can always work out what
     the behavior of the consensus was, so all that it is necessary is
     for the contract to emit that it was called and (maybe) the hash
     of the arguments? .... In fact, it may be the case that we don't
     even need the hash of the arguments, because they are listed in
     the Ethereum log somewhere. -}
  = C_Ret [BLArg]
  | C_If BLArg CTail CTail
  | C_Let (Maybe BLVar) CExpr CTail
  deriving (Show,Eq)

data CHandler
  --- Each handler has a message that it expects to receive
  = C_Handler [BLVar] CTail
  deriving (Show,Eq)

--- A contract program is just a sequence of handlers.
data CProgram
  = C_Prog [CHandler]
  deriving (Show,Eq)

-- -- Backend
type BLParts = M.Map Participant EProgram

data BLProgram
  = BL_Prog BLParts CProgram
  deriving (Show,Eq)

--- Emiting Code ---

instance Pretty BaseType where
  pretty AT_Int = pretty "int"
  pretty AT_Bool = pretty "bool"
  pretty AT_Bytes = pretty "bytes"

instance Pretty ExprType where
  pretty (TY_Con bt) = pretty bt
  pretty (TY_Var s) = viaShow s

instance Pretty Constant where
  pretty (Con_I i) = viaShow i
  pretty (Con_B b) = viaShow b
  pretty (Con_BS bs) = viaShow bs

instance Pretty C_Prim where
  pretty = viaShow

instance Pretty EP_Prim where
  pretty p = case p of
    CP cp -> pretty cp
    _ -> viaShow p

instance Pretty Role where
  pretty (RolePart p) = pretty p
  pretty RoleContract = pretty "CTC"

instance Pretty XLProgram where
  pretty = viaShow

instance Pretty XLInlinedProgram where
  pretty = viaShow

instance Pretty ILArg where
  pretty (IL_Var v) = prettyILVar v
  pretty (IL_Con c) = pretty c

prettyApp :: (Pretty p, Pretty a) => p -> [a] -> Doc ann
prettyApp p al = group $ parens $ pretty p <> ap
  where ap = case al of [] -> emptyDoc
                        _ -> space <> (hsep $ map pretty al)

prettyAssert :: Pretty a => a -> Doc ann
prettyAssert a = group $ parens $ pretty "assert!" <+> pretty a

prettyTransfer :: Pretty a => Role -> a -> Doc ann
prettyTransfer to a = group $ parens $ pretty "transfer!" <+> pretty to <+> pretty a

instance Pretty ILExpr where
  pretty (IL_PrimApp p al) = prettyApp p al
  pretty (IL_Declassify a) = group $ parens $ pretty "declassify" <+> pretty a
  pretty (IL_Transfer to a) = prettyTransfer to a
  pretty (IL_Assert a) = prettyAssert a

prettyValues :: Pretty a => [a] -> Doc ann
prettyValues [ a ] = pretty a
prettyValues [] = group $ parens $ pretty "values"
prettyValues al = group $ parens $ (pretty "values") <+> (hsep $ map pretty al)

prettyIf :: (Pretty a, Pretty b) => a -> b -> b -> Doc ann
prettyIf ca tt ft = group $ parens $ pretty "cond" <+> (nest 2 $ hardline <> vsep [(group $ brackets $ (pretty ca) <+> pretty tt), (group $ brackets $ pretty "else" <+> pretty ft)])

prettyLet :: (Pretty xe, Pretty bt) => (x -> Doc ann) -> (Doc ann -> Doc ann) -> Maybe x -> xe -> bt -> Doc ann
prettyLet prettyVar at mv e bt =
  vsep [(group $ at (parens $ ivp <> pretty e)), pretty bt]
  where ivp = case mv of
                Nothing -> emptyDoc
                Just v -> pretty "define" <+> prettyVar v <> space

instance Pretty ILTail where
  pretty (IL_Ret al) = prettyValues al
  pretty (IL_If ca tt ft) = prettyIf ca tt ft
  pretty (IL_Let r miv e bt) = prettyLet prettyILVar at miv e bt
    where at d = (group $ parens $ pretty "@" <+> pretty r <+> d)
  pretty (IL_ToConsensus p svs pa ct) =
    vsep [(group $ parens $ pretty "@" <+> pretty p <+> (nest 2 $ hardline <> vsep [svsp, pap])),
          pretty ct]
    where svsp = parens $ pretty "publish!" <+> prettyILVars svs
          pap = parens $ pretty "pay!" <+> pretty pa
  pretty (IL_FromConsensus lt) =
    vsep [(group $ parens $ pretty "return!"),
          pretty lt]

prettyILVar :: ILVar -> Doc ann
prettyILVar (n, s) = pretty n <> pretty "/" <> pretty s

prettyILVars :: [ILVar] -> Doc ann
prettyILVars vs = parens $ hsep $ map prettyILVar vs

prettyILPartArg :: (ILVar, ExprType) -> Doc ann
prettyILPartArg (v, et) = group $ brackets $ prettyILVar v <+> pretty ":" <+> pretty et

prettyILPart :: (Participant, [(ILVar, ExprType)]) -> Doc ann
prettyILPart (p, vs) =
  group $ parens $ pretty "define-participant" <+> pretty p <> body
  where pvs = map prettyILPartArg vs
        body = case vs of [] -> emptyDoc
                          _ -> (nest 2 $ hardline <> vsep pvs)

prettyILPartInfo :: ILPartInfo -> Doc ann
prettyILPartInfo ps =
  vsep $ pretty "#:participants" : (map prettyILPart (M.toList ps))

instance Pretty ILProgram where
  pretty (IL_Prog ps t) = vsep [pretty "#lang alacrity/il", emptyDoc, prettyILPartInfo ps, emptyDoc, pretty "#:main", pretty t]

instance Pretty BLArg where
  pretty (BL_Con c) = pretty c
  pretty (BL_Var v) = prettyBLVar v

instance Pretty EPExpr where
  pretty (EP_PrimApp p al) = prettyApp p al
  pretty (EP_Assert a) = prettyAssert a
  pretty (EP_Send hi vs pa) =
    group $ parens $ pretty "send!" <+> pretty hi <+> prettyBLVars vs <+> pretty pa

instance Pretty CExpr where
  pretty (C_PrimApp p al) = prettyApp p al
  pretty (C_Assert a) = prettyAssert a
  pretty (C_Transfer to a) = prettyTransfer to a

instance Pretty EPTail where
  pretty (EP_Ret al) = prettyValues al
  pretty (EP_If ca tt ft) = prettyIf ca tt ft
  pretty (EP_Let mv e bt) = prettyLet prettyBLVar (\x -> x) mv e bt
  pretty (EP_Recv hi vs bt) =
    vsep [group $ parens $ pretty "define-values" <+> prettyBLVars vs <+> (parens $ pretty "recv!" <+> pretty hi),
          pretty bt]

instance Pretty CTail where
  pretty (C_Ret al) = prettyValues al
  pretty (C_If ca tt ft) = prettyIf ca tt ft
  pretty (C_Let mv e bt) = prettyLet prettyBLVar (\x -> x) mv e bt

prettyCHandler :: Int -> CHandler -> Doc ann
prettyCHandler i (C_Handler args ct) =
  group $ brackets $ pretty i <+> prettyBLVars args <+> (nest 2 $ hardline <> pretty ct)

instance Pretty CProgram where
  pretty (C_Prog hs) = group $ parens $ pretty "define-contract" <+> (nest 2 $ hardline <> vsep hsp)
    where hsp = zipWith prettyCHandler [0..] hs

prettyBLVar :: BLVar -> Doc ann
prettyBLVar (n, s, et) = group $ brackets $ prettyILVar (n,s) <+> pretty ":" <+> pretty et

prettyBLVars :: [BLVar] -> Doc ann
prettyBLVars bs = parens $ hsep $ map prettyBLVar bs

prettyBLPart :: (Participant, EProgram) -> Doc ann
prettyBLPart (p, (EP_Prog args t)) =
  group $ parens $ pretty "define-participant" <+> pretty p <+> (nest 2 $ hardline <> vsep [argp, emptyDoc, pretty t])
  where argp = group $ parens $ vsep $ map prettyBLVar args

prettyBLParts :: BLParts -> Doc ann
prettyBLParts ps =
  vsep $ map prettyBLPart (M.toList ps)

instance Pretty BLProgram where
  pretty (BL_Prog ps ctc) = vsep [pretty "#lang alacrity/bl", emptyDoc, pretty ctc, emptyDoc, prettyBLParts ps]
