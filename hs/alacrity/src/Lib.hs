module Lib (compile) where

import System.IO

import Data.Maybe
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.ByteString.Char8 as B

import Data.Text.Prettyprint.Doc

import Control.Monad (void)
import Control.Monad.State.Lazy
import Control.Monad.Combinators.Expr
import Data.Void
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MPC
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Text.SExpression as SE

import Z3.Monad

import System.FilePath
import System.Directory

-- Shared types
data AVType = AT_Int | AT_Bool | AT_String | AT_Void
            deriving (Show)

data ASType = Secret | Public
            deriving (Show)

--- XXX The default is Secret
type AType = ( AVType, ASType )

data Constant = Con_I Integer
              | Con_B Bool
              | Con_S String --- XXX Change to ByteString
              | Con_Void
              deriving (Show)

-- -- Primitives are divided into ones that the contract can do and
-- -- ones that endpoints can do.
data C_Prim = ADD | SUB | MUL | DIV | MOD
            | PLT | PLE | PEQ | PGE | PGT
            | IF_THEN_ELSE
            | INT_TO_BYTES
            | DIGEST
            | BYTES_EQ | BYTES_LEN
            | BCAT | BCAT_LEFT | BCAT_RIGHT
            deriving (Show)

data EP_Prim = CP C_Prim | RANDOM | INTERACT
             deriving (Show)

type Participant = String

type Var = String

data Role = RolePart Participant
          | RoleContract
          deriving (Show)

-- Expanded Language (the language after expansion)

--- XXX Add exceptions, handling, & timeouts

data XLExpr = XL_Con Constant
            | XL_Var Var
            | XL_PrimApp EP_Prim [XLExpr]
            | XL_If XLExpr XLExpr XLExpr
            | XL_Assert Bool XLExpr
            --- XXX In the future, we could remove the annotations on
            --- this and look at the free-variables in the expression
            --- to figure out what is free, then look at the
            --- participant with the largest set of these values.
            | XL_Consensus Var [Var] XLExpr
            | XL_Values [XLExpr]
            | XL_Transfer Var Var XLExpr
            --- XXX Publish is like Declassify & Send
            | XL_Declassify XLExpr
            | XL_LetValues (Maybe Var) [Var] XLExpr XLExpr
            | XL_FunApp Var [Var] [XLExpr]
            deriving (Show)

data XLDef = XL_DefineValues [Var] XLExpr
           | XL_DefineFun Var [Var] [Var] XLExpr
           deriving (Show)

data XLProgram = XL_Prog [XLDef] (M.Map Participant [Var]) XLExpr
               deriving (Show)

-- Intermediate Language
type ILVar = Int

data ILArg = IL_Con Constant
           | IL_Var ILVar
           deriving (Show)

data ILExpr = IL_PrimApp EP_Prim [ILArg]
            deriving (Show)

data ILTail = IL_Ret [ILArg]
            | IL_If ILArg ILTail ILTail
            | IL_Let ILVar ILExpr ILTail
            | IL_Assert Bool ILArg ILTail
            --- XXX Adjacent consensus are collapsed together, so if
            --- there are still adjacent ones, it is an error.
            | IL_Consensus Role [ILVar] ILTail [ILVar] ILTail
            | IL_Transfer Role Role ILArg ILTail
            | IL_Declassify ILArg
            deriving (Show)

data ILProgram = IL_Prog (M.Map Participant [ILVar]) ILTail
               deriving (Show)

-- Backend Language
type BLVar = Int

data BLArg = BL_Con Constant
           | BL_Var BLVar
           deriving (Show)

-- -- End-Points
data EPExpr = EP_PrimApp EP_Prim [BLArg]
            deriving (Show)

data EPTail = EP_Ret [BLArg]
            | EP_If BLArg EPTail EPTail
            | EP_Let BLVar EPExpr EPTail
            | EP_Assert Bool BLArg EPTail
            | EP_Send Int [BLVar] EPTail
            | EP_Recv Int [BLVar] EPTail
            deriving (Show)

data EProgram = EP_Prog [BLVar] EPTail
              deriving (Show)

-- -- Contracts
data CExpr = C_PrimApp C_Prim [BLArg]
           deriving (Show)

data CTail = C_Ret [BLArg]
           | C_If BLArg CTail CTail
           | C_Let Var CExpr CTail
           | C_Assert BLArg CTail
           | C_Transfer BLArg BLArg BLArg CTail
           deriving (Show)

data CHandler = C_Handler [BLVar] CTail
              deriving (Show)

data CProgram = C_Prog [CHandler]
              deriving (Show)

-- -- Backend
data BLProgram = BL_Prog CProgram (M.Map Participant EProgram)
               deriving (Show)

-- Parser
decodeVar :: SE.SExpr -> Var
decodeVar (SE.Atom v) = v
decodeVar se = invalid "decodeVar" se

decodeVars :: [SE.SExpr] -> [Var]
decodeVars ses = map decodeVar ses

decodeXLOp :: String -> [XLExpr] -> XLExpr
decodeXLOp s =
  case s of
    "+" -> prim (CP ADD)
    "-" -> prim (CP SUB)
    "*" -> prim (CP MUL)
    "/" -> prim (CP DIV)
    "%" -> prim (CP MOD)
    "modulo" -> prim (CP SUB)
    "<" -> prim (CP PLT)
    "<=" -> prim (CP PLE)
    "=" -> prim (CP PEQ)
    ">=" -> prim (CP PGE)
    ">" -> prim (CP PGT)
    "?:" -> prim (CP IF_THEN_ELSE)
    "integer->integer-bytes" -> prim (CP INT_TO_BYTES)
    "digest" -> prim (CP DIGEST)
    "bytes=?" -> prim (CP BYTES_EQ)
    "bytes-length" -> prim (CP BYTES_LEN)
    "msg-cat" -> prim (CP BCAT)
    "msg-left" -> prim (CP BCAT_LEFT)
    "msg-right" -> prim (CP BCAT_RIGHT)
    "random" -> prim RANDOM
    "interact" -> prim INTERACT
    _ -> XL_FunApp s []
  where
    prim p = XL_PrimApp p

decodeXLExpr1 :: SE.SExpr -> XLExpr
decodeXLExpr1 (SE.Number i) = XL_Con (Con_I i)
decodeXLExpr1 (SE.Atom "#t") = XL_Con (Con_B True)
decodeXLExpr1 (SE.Atom "#f") = XL_Con (Con_B False)
decodeXLExpr1 (SE.String s) = XL_Con (Con_S s)
decodeXLExpr1 (SE.Atom v) = XL_Var v
decodeXLExpr1 (SE.List [SE.Atom "if", ce, te, fe]) = XL_If (decodeXLExpr1 ce) (decodeXLExpr1 te) (decodeXLExpr1 fe)
decodeXLExpr1 (SE.List [SE.Atom "cond"]) = XL_Con (Con_Void)
decodeXLExpr1 (SE.List [SE.Atom "cond", SE.List (SE.Atom "else" : answer)]) = decodeXLExpr answer
decodeXLExpr1 (SE.List (SE.Atom "cond" : SE.List (question : answer) : more)) =
  XL_If (decodeXLExpr1 question) (decodeXLExpr answer) (decodeXLExpr1 (SE.List (SE.Atom "cond" : more)))
decodeXLExpr1 (SE.List [SE.Atom "rely!", arg]) = XL_Assert True (decodeXLExpr1 arg)
decodeXLExpr1 (SE.List [SE.Atom "gurantee!", arg]) = XL_Assert False (decodeXLExpr1 arg)
decodeXLExpr1 (SE.List [SE.Atom "transfer", from, to, amt]) = XL_Transfer (decodeVar from) (decodeVar to) (decodeXLExpr1 amt)
decodeXLExpr1 (SE.List [SE.Atom "declassify", arg]) = XL_Declassify (decodeXLExpr1 arg)
decodeXLExpr1 (SE.List (SE.Atom "values" : args)) = XL_Values (map decodeXLExpr1 args)
decodeXLExpr1 (SE.List (SE.Atom op : args)) = (decodeXLOp op) (map decodeXLExpr1 args)
decodeXLExpr1 se = invalid "decodeXLExpr1" se

decodeXLExpr :: [SE.SExpr] -> XLExpr
decodeXLExpr [] = error "Empty list of definitions"
decodeXLExpr [e] = decodeXLExpr1 e
--- XXX Consensus
--- XXX Located definitions
decodeXLExpr ((SE.List [SE.Atom "define", vse, ese]) : kse) =
  XL_LetValues Nothing [(decodeVar vse)] (decodeXLExpr1 ese) (decodeXLExpr kse)
decodeXLExpr ((SE.List [SE.Atom "define-values", SE.List vses, ese]) : kse) =
  XL_LetValues Nothing (decodeVars vses) (decodeXLExpr1 ese) (decodeXLExpr kse)
decodeXLExpr ( e : kse ) =
  --- XXX Replace _ with something more robust (such as changing the definition of Var)
  XL_LetValues Nothing ["_"] (decodeXLExpr1 e) (decodeXLExpr kse)

decodeXLParts :: SE.SExpr -> (M.Map Participant [Var])
decodeXLParts (SE.List []) = M.empty
decodeXLParts (SE.List (SE.List (SE.Atom p : vses) : more)) = M.insert p vs (decodeXLParts (SE.List more))
  where vs = decodeVars vses
decodeXLParts se = invalid "decodeXLParts" se

decodeXLMain :: [SE.SExpr] -> ((M.Map Participant [Var]), XLExpr)
decodeXLMain [(SE.List (SE.Atom "define-main" : psse : bodyse))] =
  ( decodeXLParts psse, decodeXLExpr bodyse )
decodeXLMain ses = invalids "decodeXLMain" ses

decodeXLDefs :: [SE.SExpr] -> IO ([XLDef], [SE.SExpr])
decodeXLDefs [] = return ([],[])
decodeXLDefs ((SE.List [SE.Atom "require", SE.String fp]) : more) =
  do defs1 <- readXLLibrary fp
     (defs2, m) <- decodeXLDefs more
     return (defs1 ++ defs2, m)
decodeXLDefs ((SE.List (SE.Atom "define" : SE.List (fse : argse) : ese)) : more) =
  do let f = decodeVar fse
         args = decodeVars argse
         e = decodeXLExpr ese
     (defs2, m) <- decodeXLDefs more
     --- XXX Parse roles
     return ((XL_DefineFun f [] args e) : defs2, m)
decodeXLDefs ((SE.List [SE.Atom "define", SE.Atom vse, ese]) : more) =
  do let v = decodeVar (SE.Atom vse)
         e = decodeXLExpr1 ese
     (defs2, m) <- decodeXLDefs more
     return ((XL_DefineValues [v] e) : defs2, m)
decodeXLDefs ((SE.List [SE.Atom "define-values", SE.List vses, ese]) : more) =
  do let vs = decodeVars vses
         e = decodeXLExpr1 ese
     (defs2, m) <- decodeXLDefs more
     return ((XL_DefineValues vs e) : defs2, m)
decodeXLDefs ses = return ([], ses)

decodeXLProgram :: SE.SExpr -> IO XLProgram
decodeXLProgram (SE.List (SE.Atom "#lang" : SE.Atom "alacrity/exe" : body)) =
  do (defs, more) <- decodeXLDefs body
     let (ps, be) = decodeXLMain more
     return (XL_Prog defs ps be)
decodeXLProgram se = invalid "decodeXLProgram" se

decodeXLLibrary :: SE.SExpr -> IO [XLDef]
decodeXLLibrary (SE.List (SE.Atom "#lang" : SE.Atom "alacrity/lib" : body)) =
  do (defs, []) <- decodeXLDefs body
     return defs
decodeXLLibrary se = invalid "decodeXLLibrary" se

invalid :: String -> SE.SExpr -> a
invalid f se = error ("Invalid " ++ f ++ " se: " ++ show se)

invalids :: String -> [SE.SExpr] -> a
invalids f ses = invalid f (SE.List ses)

parseSExprs :: SE.Parser SE.SExpr
parseSExprs = SE.List <$> (MP.sepBy1 SE.parseSExpr sc)
  where sc = L.space MPC.space1 lineCmnt MP.empty
        lineCmnt  = L.skipLineComment ";"

readXLProgram :: FilePath -> IO XLProgram
readXLProgram fp = readSExpr fp >>= decodeXLProgram

readXLLibrary :: FilePath -> IO [XLDef]
readXLLibrary fp = readSExpr fp >>= decodeXLLibrary

readSExpr :: FilePath -> IO SE.SExpr
readSExpr srcp = do
  input <- readFile srcp
  case MP.parse parseSExprs srcp input of
    Left bundle ->
      do putStr (MP.errorBundlePretty bundle)
         error ("Failed to parse " ++ srcp)
    Right se -> return se

-- Compilation & Analysis API

-- --- The ANF Monad stores the next available variable and a list of
-- --- previously bound variables.
type ANFMonad a = State (ILVar, [(ILVar, ILExpr)]) a

runANF :: ANFMonad a -> a
runANF am = a where (a, (_,[])) = runState am (0, [])

anf :: XLProgram -> ILProgram
anf xlp = runANF xm
  where XL_Prog defs ps main = xlp
        anf_funs :: [XLDef] -> (M.Map Var ([Var], [Var], XLExpr))
        anf_funs ds = error "XXX anf_funs"
        funmap = anf_funs defs
        anf_defs :: [XLDef] -> ANFMonad (M.Map Var ILArg)
        anf_defs ds = error "XXX anf_defs"
        anf_ps :: (M.Map Participant [Var]) -> ANFMonad ((M.Map Var ILArg), (M.Map Participant [ILVar]))
        anf_ps ps = error "XXX anf_ps"
        anf_tail :: (M.Map Var ILArg) -> XLExpr -> ANFMonad ILTail
        anf_tail ρ xe = error "XXX anf_tail"
        anf_expr :: (M.Map Var ILArg) -> XLExpr -> ANFMonad ILExpr
        anf_expr ρ xe = error "XXX anf_expr"
        anf_arg :: (M.Map Var ILArg) -> XLExpr -> ANFMonad ILArg
        anf_arg ρ xe = error "XXX anf_arg"
        xm :: ANFMonad ILProgram
        xm = do ρ0 <- anf_defs defs
                (ρ1, nps) <- anf_ps ps
                let ρ2 = M.union ρ0 ρ1
                mt <- anf_tail ρ2 main
                return (IL_Prog nps mt)

--- Performs type checking (including information-flow security)
epp :: ILProgram -> BLProgram
epp ilp = error "XXX epp"

emit_js :: BLProgram -> Doc ann
emit_js blp = error "XXX emit_js"

--- XXX The consensus block can know all of the variables to do hash storage.
emit_sol :: BLProgram -> Doc ann
emit_sol blp = error "XXX emit_sol"

--- XXX What does rely / guarantee mean?
--- ;; The Z3 program should verify the follows claims:
--- ;; - guarantee! in participant is true
--- ;; - guarantee! in contract is true
--- ;; - rely! in contract is true with honest participants
--- ;;   (i.e. participants rely!'s are trusted)
--- ;; NOTE: rely! is checked at run-time, but trusted in Z3
--- ;; - balance of CTC is 0 on halt
emit_z3 :: BLProgram -> Z3 String
emit_z3 blp = error "XXX emit_z3"

compile :: FilePath -> IO ()
compile srcp = do
  xlp <- withCurrentDirectory (takeDirectory srcp) (readXLProgram (takeFileName srcp))
  writeFile (srcp ++ ".xl") (show xlp)
  let ilp = anf xlp
  writeFile (srcp ++ ".il") (show ilp)
  let blp = epp ilp
  writeFile (srcp ++ ".bl") (show blp)
  z3res <- evalZ3 (emit_z3 $! blp)
  -- XXX If this fails, then refuse to continue
  putStrLn ("Z3: " ++ z3res)
  writeFile (srcp ++ ".sol") (show (emit_sol $! blp))
  writeFile (srcp ++ ".js") (show (emit_js $! blp))

