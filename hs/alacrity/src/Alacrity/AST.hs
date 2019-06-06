module Alacrity.AST where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import Numeric.Natural

-- Shared types

--- XXX Make types more general with arbitary range integers. Bool as
--- alias to [0,1]. Bytes with fixed lengths.
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

--- XXX Flesh out all of the operations
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
  deriving (Show,Eq)

{- Surface Language

 -}

{- Expanded Language (the language after expansion)

   There are some extensions we need to add in the future:

   XXX Add roles to function definitions and calls, so you can write
   generic communicators. Change Participant/Role below to a Var and
   dynamically decide.

   XXX Add exceptions, exception handling & timeouts

 -}

type XLVar = String

data XLExpr
  = XL_Con Constant
  | XL_Var XLVar
  | XL_PrimApp EP_Prim [XLExpr]
  | XL_If XLExpr XLExpr XLExpr
  | XL_Assert XLExpr
  --- Sender x Message x Contract Code
  | XL_Consensus Participant XLExpr XLExpr
  {- Notice that the consensus doesn't list the binders. We assume that it
     ends in XL_Values, which is the message that gets returned. We'll
     unwind this during ANF and turn Consensus into a binding form. -}
  | XL_Values [XLExpr]
  --- From x To x Amount
  | XL_Transfer Role Role XLExpr
  | XL_Declassify XLExpr
  --- Where x Vars x Expression x Body
  | XL_LetValues (Maybe Participant) (Maybe [XLVar]) XLExpr XLExpr
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
   job: it must ensure that IFs are consensual. (XXX: I don't know
   exactly how to do this and am only 80% sure it is necessary.)
   -}

--- The string is just for debugging, it tracks where the variable was
--- created.
type ILVar = (Natural, String)

data ILArg
  = IL_Con Constant
  | IL_Var ILVar
  deriving (Show,Eq)

data ILExpr
  = IL_PrimApp EP_Prim [ILArg]
  | IL_Declassify ILArg
  | IL_Transfer Role Role ILArg
  | IL_Assert ILArg
  deriving (Show,Eq)

data ILTail
  = IL_Ret [ILArg]
  | IL_If ILArg ILTail ILTail
  | IL_Let (Maybe Participant) (Maybe ILVar) ILExpr ILTail
  --- From x SentMsg x Contract Body x RecvMsg x K
  | IL_Consensus Participant [ILVar] ILTail [ILVar] ILTail
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

   XXX It is plausible that it is valuable to de-inline in the
   contract to save storage space on the contract implementation.

   XXX At this point, we assume that the contract remembers all
   variables defined in earlier handlers. In the future, when we hash
   the state in the contract, we need to make the handlers mostly
   stateless (except for a hash of the state), so that the
   participants communicate the required state.

   -}

type BLVar = (Natural, ExprType)

data BLArg
  = BL_Con Constant
  | BL_Var BLVar
  deriving (Show,Eq)

-- -- End-Points
data EPExpr
  = EP_PrimApp EP_Prim [BLArg]
  | EP_Assert BLArg
  | EP_Send Natural [BLVar]
  deriving (Show,Eq)

data EPTail
  = EP_Ret [BLArg]
  | EP_If BLArg EPTail EPTail
  | EP_Let (Maybe BLVar) EPExpr EPTail
  {- XXX Right now this Recv represents receiving the message from the
     consensus, but in the future it will represent the message that
     the consensus RECEIVES and we will do the computation ourselves. -}
  | EP_Recv Natural [BLVar] EPTail
  deriving (Show,Eq)

data EProgram
  = EP_Prog [BLVar] EPTail
  deriving (Show,Eq)

-- -- Contracts
data CExpr
  = C_PrimApp C_Prim [BLArg]
  | C_Assert BLArg CTail
  {- XXX In the future, these Roles should be BLArg and
     there should be a way to deal with arbitrary Addresses
     and transform Roles into addresses. -}
  | C_Transfer Role Role BLArg CTail
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
data BLProgram
  = BL_Prog (M.Map Participant EProgram) CProgram
  deriving (Show,Eq)
