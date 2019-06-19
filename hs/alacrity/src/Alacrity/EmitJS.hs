module Alacrity.EmitJS where

import qualified Data.Map.Strict as M
import Data.List (intersperse)
import Data.Text.Prettyprint.Doc

import Alacrity.AST
import Alacrity.EmitSol (solMsg_evt, solMsg_fun, solType)

{- Compilation to Javascript

   I'm imagining the type of the JS export is:

   Network -> Participant -> NetworkArgs x Args x (Result -> A) -> A or doesn't

   We have some standard way of interacting with the network (so we
   don't depend in the compiler on whether we're using rinkydink or
   whatever or what the name is.) Then, you can look up the code for
   one of the participants (by name). Then you provide extra arguments
   for the network (such as the contract / game id) and the
   participant's initial knowledge, then a continuation for what to do
   with the result.
  -}

jsString :: String -> Doc a
jsString s = dquotes $ pretty s

jsVar :: BLVar -> Doc a
jsVar (n, _, _) = pretty $ "v" ++ show n

jsVar' :: BLVar -> Doc a
jsVar' (n, _, _) = pretty $ "p" ++ show n

jsVarType :: BLVar -> Doc a
jsVarType (_, _, bt) = jsString $ solType bt

jsCon :: Constant -> Doc a
jsCon (Con_I i) = pretty i
jsCon (Con_B True) = pretty "true"
jsCon (Con_B False) = pretty "false"
jsCon (Con_BS s) = jsString $ show s

jsArg :: BLArg -> Doc a
jsArg (BL_Var v) = jsVar v
jsArg (BL_Con c) = jsCon c

jsPartVar :: Participant -> Doc a
jsPartVar p = pretty $ "p" ++ p

jsVarDecl :: BLVar -> Doc a
jsVarDecl bv = pretty "const" <+> jsVar bv

jsBraces :: Doc a -> Doc a
jsBraces body = braces (nest 2 $ hardline <> body <> space)

jsArray :: [Doc a] -> Doc a
jsArray elems = brackets $ hcat $ intersperse (comma <> space) elems

jsApply :: String -> [Doc a] -> Doc a
jsApply f args = pretty f <> parens (hcat $ intersperse (comma <> space) args)

jsFunction :: String -> [Doc a] -> Doc a -> Doc a
jsFunction name args body =
  pretty "function" <+> jsApply name args <+> jsBraces body

jsLambda :: [Doc a] -> Doc a -> Doc a
jsLambda args body = jsApply "" args <+> pretty "=>" <+> jsBraces body

jsReturn :: Doc a -> Doc a
jsReturn a = pretty "return" <+> a <> semi

jsObject :: [(String, Doc a)] -> Doc a
jsObject kvs = jsBraces $ vsep $ (intersperse comma) $ map jsObjField kvs
  where jsObjField (k, v) = pretty (k ++ ":") <> hardline <> v

jsBinOp :: String -> Doc a -> Doc a -> Doc a
jsBinOp o l r = l <+> pretty o <+> r

jsPrimApply :: EP_Prim -> [Doc a] -> Doc a
jsPrimApply pr args =
  case pr of
    CP ADD -> binOp "+"
    CP SUB -> binOp "-"
    CP MUL -> binOp "*"
    CP DIV -> binOp "/"
    CP MOD -> binOp "%"
    CP PLT -> binOp "<"
    CP PLE -> binOp "<="
    CP PEQ -> binOp "=="
    CP PGE -> binOp ">="
    CP PGT -> binOp ">"
    CP IF_THEN_ELSE -> case args of
                      [ c, t, f ] -> c <+> pretty "?" <+> t <+> pretty ":" <+> f
                      _ -> spa_error ()
    CP UINT256_TO_BYTES -> jsApply "stdlib.uint256_to_bytes" args
    CP DIGEST -> jsApply "stdlib.keccak256" args
    CP BYTES_EQ -> jsApply "stdlib.bytes_eq" args
    CP BYTES_LEN -> jsApply "stdlib.bytes_len" args
    CP BCAT -> jsApply "stdlib.bytes_cat" args
    CP BCAT_LEFT -> jsApply "stdlib.bytes_left" args
    CP BCAT_RIGHT -> jsApply "stdlib.bytes_right" args
    RANDOM -> jsApply "stdlib.random_uint256" args
    INTERACT -> error "interact doesn't use jsPrimApply"
  where binOp op = case args of
          [ l, r ] -> jsBinOp op l r
          _ -> spa_error ()
        spa_error () = error "jsPrimApply"

jsEPExpr :: EPExpr -> Doc a
jsEPExpr (EP_Arg a) = jsArg a
jsEPExpr (EP_PrimApp pr al) = jsPrimApply pr $ map jsArg al

jsAssert :: Doc a -> Doc a
jsAssert a = jsApply "stdlib.assert" [ a ] <> semi

jsEPStmt :: EPStmt -> Doc a -> Doc a
jsEPStmt (EP_Claim CT_Possible _) kp = kp
jsEPStmt (EP_Claim _ a) kp = vsep [ jsAssert (jsArg a), kp ]
jsEPStmt (EP_Send i svs msg amt) kp = jsApply "ctc.send" [ jsString (solMsg_fun i), vs, jsArg amt, jsLambda [] kp ]
  where args = svs ++ msg
        vs = jsArray $ map jsVar args

jsEPTail :: EPTail -> Doc a
jsEPTail (EP_Ret al) = (jsApply "kTop" $ map jsArg al) <> semi
jsEPTail (EP_If ca tt ft) =
  pretty "if" <+> parens (jsArg ca) <> bp tt <> hardline <> pretty "else" <> bp ft
  where bp at = jsBraces $ jsEPTail at
jsEPTail (EP_Let v (EP_PrimApp INTERACT al) kt) =
  jsApply "interact" ((map jsArg al) ++ [ kp ])
  where kp = jsLambda [ jsVar v ] $ jsEPTail kt
jsEPTail (EP_Let bv ee kt) = vsep [ jsVarDecl bv <+> pretty "=" <+> jsEPExpr ee <> semi, jsEPTail kt ]
jsEPTail (EP_Do es kt) = jsEPStmt es $ jsEPTail kt
jsEPTail (EP_Recv fromme i _ msg pv kt) = jsApply "ctc.recv" [ jsString (solMsg_evt i), kp ]
  where kp = jsLambda (the_vs ++ [jsVar pv]) ktp
        msg_vs = map jsVar msg
        msg'_vs = map jsVar' msg
        the_vs = if fromme then msg'_vs else msg_vs
        require_and_kt = vsep $ zipWith require_match msg_vs msg'_vs
        require_match mvp mv'p = jsAssert $ jsApply "stdlib.equal" [ mvp, mv'p ]
        ktp = (if fromme then require_and_kt <> hardline else emptyDoc) <> jsEPTail kt

jsPart :: (Participant, EProgram) -> Doc a
jsPart (p, (EP_Prog pargs et)) =
  --- XXX Perhaps use async/await rather than CPS for more idiomatic code?
  pretty "export" <+> jsFunction p ([ pretty "ctc", pretty "interact" ] ++ pargs_vs ++ [ pretty "kTop" ]) bodyp
  where pargs_vs = map jsVar pargs
        bodyp = jsEPTail et

emit_js :: BLProgram -> Doc a
emit_js (BL_Prog pm _) = modp
  where modp = vsep_with_blank ( pretty "import * as stdlib from './alacrity-runtime.mjs';"
                                 : pretty "/* XXX export const ABI = Copy the ABI from the solc output; */"
                                 : pretty "/* XXX export const Bytecode = \"0x Copy the bytecode from the solc output\"; */"
                                 : partsp )
        partsp = map jsPart $ M.toList pm

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l
