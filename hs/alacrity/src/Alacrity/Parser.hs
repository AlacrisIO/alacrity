module Alacrity.Parser
  ( readAlacrityFile
  ) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as M
import System.Directory
import System.FilePath
import qualified Text.Megaparsec as MP
import qualified Alacrity.SExpr as SE

import Alacrity.AST

valid_id :: String -> Bool
valid_id p = not (elem p rsw) && head p /= '#' && (Nothing == (decodePrim p))
  where rsw = ["if", "cond", "else", "assert!", "transfer!", "declassify", "values", "@", "define", "define-values", "require", "CTC"]

decodeXLType :: SE.SExpr -> BaseType
decodeXLType (SE.Atom "uint256") = AT_UInt256
decodeXLType (SE.Atom "bool") = AT_Bool
decodeXLType (SE.Atom "bytes") = AT_Bytes
decodeXLType se = invalid "decodeXLType" se

decodeRole :: SE.SExpr -> Role
decodeRole (SE.Atom "CTC") = RoleContract
decodeRole (SE.Atom p)
  | valid_id p = RolePart p
  | otherwise = error (p ++ " is reserved!")
decodeRole se = invalid "decodeRole" se

decodePart :: SE.SExpr -> Participant
decodePart se =
  case (decodeRole se) of
    RolePart p -> p
    RoleContract -> error "CTC is not a participant!"

decodeXLVar :: SE.SExpr -> XLVar
decodeXLVar (SE.Atom v)
  | valid_id v = v
  | otherwise = error (v ++ " is reserved!")
decodeXLVar se = invalid "decodeXLVar" se

decodeXLVarType :: SE.SExpr -> (XLVar, BaseType)
decodeXLVarType (SE.List [vse,SE.Atom ":", tse]) = ((decodeXLVar vse), (decodeXLType tse))
decodeXLVarType se = invalid "decodeXLVarType" se

decodeXLVars :: [SE.SExpr] -> [XLVar]
decodeXLVars ses = map decodeXLVar ses

decodePrim :: String -> Maybe EP_Prim
decodePrim s =
    case s of
    "+" -> Just (CP ADD)
    "-" -> Just (CP SUB)
    "*" -> Just (CP MUL)
    "/" -> Just (CP DIV)
    "%" -> Just (CP MOD)
    "modulo" -> Just (CP MOD)
    "<" -> Just (CP PLT)
    "<=" -> Just (CP PLE)
    "=" -> Just (CP PEQ)
    ">=" -> Just (CP PGE)
    ">" -> Just (CP PGT)
    "ite" -> Just (CP IF_THEN_ELSE)
    "uint256->bytes" -> Just (CP UINT256_TO_BYTES)
    "digest" -> Just (CP DIGEST)
    "bytes=?" -> Just (CP BYTES_EQ)
    "bytes-length" -> Just (CP BYTES_LEN)
    "msg-cat" -> Just (CP BCAT)
    "msg-left" -> Just (CP BCAT_LEFT)
    "msg-right" -> Just (CP BCAT_RIGHT)
    "DISHONEST" -> Just (CP DISHONEST)
    "random" -> Just RANDOM
    "interact" -> Just INTERACT
    _ -> Nothing

decodeXLOp :: String -> [XLExpr] -> XLExpr
decodeXLOp s =
  case decodePrim s of
    Just p -> XL_PrimApp p
    Nothing ->
      if valid_id s then
        XL_FunApp s
      else
        error $ "Invalid function name: " ++ show s

decodeXLExpr1 :: SE.SExpr -> XLExpr
decodeXLExpr1 (SE.Number i) = XL_Con (Con_I i)
decodeXLExpr1 (SE.Bool t) = XL_Con (Con_B t)
decodeXLExpr1 (SE.String s) = XL_Con (Con_BS (B.pack s))
decodeXLExpr1 (SE.Atom v) = XL_Var v
decodeXLExpr1 (SE.List [SE.Atom "if", ce, te, fe]) =
  XL_If False (decodeXLExpr1 ce) (decodeXLExpr1 te) (decodeXLExpr1 fe)
decodeXLExpr1 (SE.List [SE.Atom "cond", SE.List (SE.Atom "else":answer)]) =
  decodeXLExpr answer
decodeXLExpr1 (SE.List (SE.Atom "cond":SE.List (question:answer):more)) =
  XL_If False (decodeXLExpr1 question) (decodeXLExpr answer) (decodeXLExpr1 (SE.List (SE.Atom "cond" : more)))
decodeXLExpr1 (SE.List [SE.Atom "assert!", arg]) =
  XL_Assert (decodeXLExpr1 arg)
decodeXLExpr1 (SE.List [SE.Atom "transfer!", to, amt]) =
  XL_Transfer (decodePart to) (decodeXLExpr1 amt)
decodeXLExpr1 (SE.List [SE.Atom "declassify", arg]) =
  XL_Declassify (decodeXLExpr1 arg)
decodeXLExpr1 (SE.List (SE.Atom "values":args)) =
  XL_Values (map decodeXLExpr1 args)
decodeXLExpr1 (SE.List (SE.Atom op:args)) =
  (decodeXLOp op) (map decodeXLExpr1 args)
decodeXLExpr1 se = invalid "decodeXLExpr1" se

decodeXLExpr :: [SE.SExpr] -> XLExpr
decodeXLExpr [] = error "Empty expression sequence"
decodeXLExpr [SE.List (SE.Atom "begin-local" : kse)] =
  XL_FromConsensus (decodeXLExpr kse)
decodeXLExpr ((SE.List (SE.Atom "@" : fromse :
                        SE.List (SE.Atom "publish!" : inse) :
                        SE.List [SE.Atom "pay!", payse] :
                        bse)):kse) =
  XL_ToConsensus p ins pay body
  where RolePart p = decodeRole fromse
        ins = decodeXLVars inse
        pay = decodeXLExpr1 payse
        bse' = bse ++ [(SE.List (SE.Atom "begin-local" : kse))]
        body = decodeXLExpr bse'
--- define
decodeXLExpr ((SE.List [SE.Atom "@", rs, (SE.List [SE.Atom "define", vse, ese])]):kse) =
  XL_LetValues (Just p) (Just [(decodeXLVar vse)]) (decodeXLExpr1 ese) (decodeXLExpr kse)
  where RolePart p = (decodeRole rs)
decodeXLExpr ((SE.List [SE.Atom "define", vse, ese]):kse) =
  XL_LetValues Nothing (Just [(decodeXLVar vse)]) (decodeXLExpr1 ese) (decodeXLExpr kse)
--- define-values
decodeXLExpr ((SE.List [SE.Atom "@", rs, (SE.List [SE.Atom "define-values", SE.List vses, ese])]):kse) =
  XL_LetValues (Just p) (Just (decodeXLVars vses)) (decodeXLExpr1 ese) (decodeXLExpr kse)
  where RolePart p = (decodeRole rs)
decodeXLExpr ((SE.List [SE.Atom "define-values", SE.List vses, ese]):kse) =
  XL_LetValues Nothing (Just (decodeXLVars vses)) (decodeXLExpr1 ese) (decodeXLExpr kse)
--- expressions
decodeXLExpr ((SE.List [SE.Atom "@", rs, e]):kse)
 = XL_LetValues (Just p) Nothing (decodeXLExpr1 e) (decodeXLExpr kse)
  where RolePart p = (decodeRole rs)
decodeXLExpr (e:kse)
 = case kse of
     [] -> (decodeXLExpr1 e)
     _ -> XL_LetValues Nothing Nothing (decodeXLExpr1 e) (decodeXLExpr kse)

decodeXLParts :: [SE.SExpr] -> (XLPartInfo, [SE.SExpr])
decodeXLParts (SE.Atom "#:main":more) = (M.empty, more)
decodeXLParts ((SE.List ((SE.Atom "define-participant":SE.Atom p:vses))):more) =
  (M.insert p vs ps, more1)
  where
    vs = map decodeXLVarType vses
    (ps, more1) = (decodeXLParts more)
decodeXLParts ses = invalids "decodeXLParts" ses

decodeXLMain :: [SE.SExpr] -> (XLPartInfo, XLExpr)
decodeXLMain (SE.Atom "#:participants":more0) = (ps, me)
  where (ps, more1) = decodeXLParts more0
        me = decodeXLExpr more1
decodeXLMain ses = invalids "decodeXLMain" ses

decodeXLDefs :: [SE.SExpr] -> IO ([XLDef], [SE.SExpr])
decodeXLDefs [] = return ([], [])
decodeXLDefs ((SE.List [SE.Atom "require", SE.String fp]):more) = do
  defs1 <- readXLLibrary fp
  (defs2, m) <- decodeXLDefs more
  return (defs1 ++ defs2, m)
decodeXLDefs ((SE.List (SE.Atom "define":SE.List (fse:argse):ese)):more) = do
  let f = decodeXLVar fse
      args = decodeXLVars argse
      e = case ese of
        (SE.Atom ":" : predse : ese') ->
          (XL_LetValues Nothing (Just ["result"]) (decodeXLExpr ese') (XL_LetValues Nothing Nothing (XL_Assert (XL_FunApp (decodeXLVar predse) [XL_Var "result"])) (XL_Var "result")))
        _ -> decodeXLExpr ese
  (defs2, m) <- decodeXLDefs more
  return ((XL_DefineFun f args e) : defs2, m)
decodeXLDefs ((SE.List [SE.Atom "define", SE.Atom vse, ese]):more) = do
  let v = decodeXLVar (SE.Atom vse)
      e = decodeXLExpr1 ese
  (defs2, m) <- decodeXLDefs more
  return ((XL_DefineValues [v] e) : defs2, m)
decodeXLDefs ((SE.List [SE.Atom "define-values", SE.List vses, ese]):more) = do
  let vs = decodeXLVars vses
      e = decodeXLExpr1 ese
  (defs2, m) <- decodeXLDefs more
  return ((XL_DefineValues vs e) : defs2, m)
decodeXLDefs ses = return ([], ses)

decodeXLProgram :: SE.SExpr -> IO XLProgram
decodeXLProgram (SE.List (SE.Atom "#lang":SE.Atom "alacrity/exe":body)) = do
  (defs, more) <- decodeXLDefs body
  let (ps, be) = decodeXLMain more
  return (XL_Prog defs ps be)
decodeXLProgram se = invalid "decodeXLProgram" se

decodeXLLibrary :: SE.SExpr -> IO [XLDef]
decodeXLLibrary (SE.List (SE.Atom "#lang":SE.Atom "alacrity/lib":body)) = do
  (defs, []) <- decodeXLDefs body
  return defs
decodeXLLibrary se = invalid "decodeXLLibrary" se

invalid :: String -> SE.SExpr -> a
invalid f se = error ("Invalid " ++ f ++ " se: " ++ show se)

invalids :: String -> [SE.SExpr] -> a
invalids f ses = invalid f (SE.List ses)

readSExpr :: FilePath -> IO SE.SExpr
readSExpr srcp = do
  input <- readFile srcp
  let input' = ("(\n" ++ input ++ "\n)")
  case MP.parse SE.parseSExpr srcp input' of
    Left bundle -> do
      putStr (MP.errorBundlePretty bundle)
      error ("Failed to parse " ++ srcp)
    Right se -> return se

readXLProgram :: FilePath -> IO XLProgram
readXLProgram fp = readSExpr fp >>= decodeXLProgram

readXLLibrary :: FilePath -> IO [XLDef]
readXLLibrary fp = readSExpr fp >>= decodeXLLibrary

readAlacrityFile :: FilePath -> IO XLProgram
readAlacrityFile srcp =
  withCurrentDirectory (takeDirectory srcp) (readXLProgram (takeFileName srcp))
