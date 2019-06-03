module Lib (compile) where

import System.IO

import Data.List
import qualified Data.Map.Strict as M

import Data.Text.Prettyprint.Doc

import Z3.Monad

import Control.Monad (void)
import Control.Monad.Combinators.Expr -- from parser-combinators
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- Shared types
data Constant = Con_I Int
              | Con_B Bool

-- -- Primitives are divided into ones that the contract can do and
-- -- ones that endpoints can do.
data C_Prim = ADD | SUB | MULT | DIV | MOD
            | LT | LE | EQ | GE | GT
            | RANDOM
            | DIGEST
            | BYTES_EQ
            | BCAT | BCAT_LEFT | BCAT_RIGHT
            | IF_THEN_ELSE
data EP_Prim = CP C_Prim

type Participant = String

type Var = String

data Role = RolePart Participant
          | RoleContract

-- Surface Language
data SLExpr = SL_Con Constant
            | SL_Var Var
            | SL_PrimApp EP_Prim [SLExpr]
            | SL_If SLExpr SLExpr SLExpr
            | SL_Assert Bool SLExpr
            | SL_Consensus Var [Var] SLExpr
            | SL_Values [SLExpr]
            | SL_Transfer Var Var SLExpr
            | SL_Classify SLExpr
            | SL_Declassify SLExpr
            | SL_LetValues (Maybe Var) [Var] SLExpr SLExpr
            | SL_FunApp Var [Var] [SLExpr]

data SL_Def = SL_DefineValues [Var] SLExpr
            | SL_DefineFun Var [Var] [Var] SLExpr 

data SLProgram = SL_Prog [SL_Def] (M.Map Participant [Var]) SLExpr

-- Intermediate Language
type ILVar = Int

data ILArg = IL_Con Constant
           | IL_Var ILVar

data ILExpr = IL_PrimApp EP_Prim [ILArg]
            | IL_Classify ILArg
            | IL_Declassify ILArg

data ILTail = IL_Ret [ILArg]
            | IL_If ILArg ILTail ILTail
            | IL_Let ILVar ILExpr ILTail
            | IL_Assert Bool ILArg ILTail
            | IL_Consensus Role [ILVar] ILTail [ILVar] ILTail
            | IL_Transfer Role Role ILArg ILTail

data ILProgram = IL_Prog (M.Map Participant [ILVar]) ILTail

-- Backend Language
type BLVar = Int

data BLArg = BL_Con Constant
           | BL_Var BLVar

-- -- End-Points
data EPExpr = EP_PrimApp EP_Prim [BLArg]

data EPTail = EP_Ret [BLArg]
            | EP_If BLArg EPTail EPTail
            | EP_Let BLVar EPExpr EPTail
            | EP_Assert Bool BLArg EPTail
            | EP_Send Int [BLVar] EPTail
            | EP_Recv Int [BLVar] EPTail

data EProgram = EP_Prog [BLVar] EPTail

-- -- Contracts
data CExpr = C_PrimApp C_Prim [BLArg]

data CTail = C_Ret [BLArg]
           | C_If BLArg CTail CTail
           | C_Let Var CExpr CTail
           | C_Assert BLArg CTail
           | C_Transfer BLArg BLArg BLArg CTail

data CHandler = C_Handler [BLVar] CTail

data CProgram = C_Prog [CHandler]

-- -- Backend
data BLProgram = BL_Prog CProgram (M.Map Participant EProgram)

-- Parser
type Parser = Parsec Void String

slParser :: Parser SLProgram
slParser = error "XXX"

-- Compilation & Analysis API
anf :: SLProgram -> ILProgram
epp :: ILProgram -> BLProgram
emit_js :: BLProgram -> Doc ann
emit_sol :: BLProgram -> Doc ann
emit_z3 :: BLProgram -> Z3 String

-- Implementation
anf slp = error "XXX"

--- Performs type checking (including information-flow security)
epp ilp = error "XXX"

emit_js blp = error "XXX"

emit_sol blp = error "XXX"

emit_z3 blp = error "XXX"

compile :: FilePath -> IO ()
compile srcp = do
  let solp = srcp ++ ".sol"
      jsp = srcp ++ ".js"
  input <- readFile srcp
  case parse slParser srcp input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right slp ->
      let ilp = anf slp
          blp = epp ilp in
      do z3res <- evalZ3 (emit_z3 blp)
         putStrLn ("Z3: " ++ z3res)
         writeFile solp (show (emit_sol blp))
         writeFile jsp (show (emit_js blp))
