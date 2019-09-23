{-# LANGUAGE LambdaCase #-}

module Alacrity.EmitJS (emit_js) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict       as M
import qualified Data.Set              as Set
import Data.List (intersperse)
import Data.Text.Prettyprint.Doc

import Alacrity.AST
import Alacrity.EmitSol
  ( solMsg_evt
  , solMsg_fun
  , solType
  , CompiledSol )


--------------------------------------------------------------------------------

_jsBinOp :: String -> Doc a -> Doc a -> Doc a
_jsBinOp o l r = l <+> pretty o <+> r

_jsVar' :: BLVar -> Doc a
_jsVar' (n, _, _) = pretty $ "p" ++ show n

_jsVarType :: BLVar -> Doc a
_jsVarType (_, _, bt) = jsString $ solType bt

_jsReturn :: Doc a -> Doc a
_jsReturn a = pretty "return" <+> a <> semi

_jsObject :: [(String, Doc a)] -> Doc a
_jsObject kvs = jsBraces $ vsep $ (intersperse comma) $ map jsObjField kvs
  where jsObjField (k, v) = pretty (k ++ ":") <> hardline <> v

_jsPartVar :: Participant -> Doc a
_jsPartVar p = pretty $ "p" ++ p


--------------------------------------------------------------------------------

jsString :: String -> Doc a
jsString s = dquotes $ pretty s

jsVar :: BLVar -> Doc a
jsVar (n, _, _) = pretty $ "v" ++ show n

jsLoopName :: Int -> String
jsLoopName i = "l" ++ show i

jsCon :: Constant -> Doc a
jsCon (Con_I  i)     = pretty i
jsCon (Con_B  True)  = pretty "true"
jsCon (Con_B  False) = pretty "false"
jsCon (Con_BS s)     = jsString $ B.unpack s

jsArg :: BLArg -> (Doc a, Set.Set BLVar)
jsArg (BL_Var v) = (jsVar v, Set.singleton v)
jsArg (BL_Con c) = (jsCon c, Set.empty)

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

vsep_with_blank :: [Doc a] -> Doc a
vsep_with_blank l = vsep $ intersperse emptyDoc l


jsPrimApply :: EP_Prim -> [Doc a] -> Doc a
jsPrimApply = \case
  CP ADD              -> jsApply "stdlib.add"
  CP SUB              -> jsApply "stdlib.sub"
  CP MUL              -> jsApply "stdlib.mul"
  CP DIV              -> jsApply "stdlib.div"
  CP MOD              -> jsApply "stdlib.mod"
  CP PLT              -> jsApply "stdlib.lt"
  CP PLE              -> jsApply "stdlib.le"
  CP PEQ              -> jsApply "stdlib.eq"
  CP PGE              -> jsApply "stdlib.ge"
  CP PGT              -> jsApply "stdlib.gt"
  CP UINT256_TO_BYTES -> jsApply "stdlib.uint256_to_bytes"
  CP DIGEST           -> jsApply "stdlib.keccak256"
  CP BYTES_EQ         -> jsApply "stdlib.bytes_eq"
  CP BYTES_LEN        -> jsApply "stdlib.bytes_len"
  CP BCAT             -> jsApply "stdlib.bytes_cat"
  CP BCAT_LEFT        -> jsApply "stdlib.bytes_left"
  CP BCAT_RIGHT       -> jsApply "stdlib.bytes_right"
  RANDOM              -> jsApply "stdlib.random_uint256"
  CP BALANCE          -> \_ -> pretty $ "txn.balance"
  CP TXN_VALUE        -> \_ -> pretty $ "txn.value"
  INTERACT            -> error "interact doesn't use jsPrimApply"

  CP IF_THEN_ELSE -> \args -> case args of
    [ c, t, f ] -> c <+> pretty "?" <+> t <+> pretty ":" <+> f
    _           -> error "jsPrimApply" ()


jsEPExpr :: EPExpr -> (Doc a, Set.Set BLVar)
jsEPExpr (EP_Arg     a)     = jsArg a
jsEPExpr (EP_PrimApp pr al) = ((jsPrimApply pr $ map fst alp), (Set.unions $ map snd alp))
  where alp = map jsArg al


jsAssert :: Doc a -> Doc a
jsAssert a = jsApply "stdlib.assert" [ a ] <> semi


jsEPStmt :: EPStmt -> Doc a -> (Doc a, Set.Set BLVar)
jsEPStmt (EP_Claim CT_Possible _) kp = (kp, Set.empty)
jsEPStmt (EP_Claim CT_Assert   _) kp = (kp, Set.empty)
jsEPStmt (EP_Claim _           a) kp = (vsep [ jsAssert ap, kp ], afvs)
                                       where (ap, afvs) = jsArg a
jsEPStmt (EP_Send  _ _ _ _)       _  = error "Impossible"


jsEPTail :: String -> EPTail -> (Doc a, Set.Set BLVar)
jsEPTail _who (EP_Ret al) = (d, s)
  where a = map jsArg al
        d = (jsApply "kTop" $ map fst a) <> semi
        s = Set.unions      $ map snd a

jsEPTail who (EP_If ca tt ft) = (tp, Set.unions [ cafvs, ttfvs, ftfvs ])
  where (ttp, ttfvs) = jsEPTail who tt
        (ftp, ftfvs) = jsEPTail who ft
        (cap, cafvs) = jsArg ca
        tp           = pretty "if"   <+> parens   cap <+> jsBraces ttp <> hardline
                    <> pretty "else" <+> jsBraces ftp

jsEPTail who (EP_Let v (EP_PrimApp INTERACT al) kt) = (tp, tfvs)
  where (ktp, ktfvs) = jsEPTail who kt
        alp          = map jsArg al
        tp           = jsApply "interact" ((map fst alp) ++ [ (jsLambda [ jsVar v ] ktp) ]) <> semi
        tfvs         = Set.union ktfvs $ Set.unions $ map snd alp

jsEPTail who (EP_Let bv ee kt) = (tp, tfvs)
  where used         = elem bv ktfvs
        tfvs'        = Set.difference ktfvs (Set.singleton bv)
        bvdeclp      = jsVarDecl bv <+> pretty "=" <+> eep <> semi
        (eep, eefvs) = jsEPExpr ee
        (ktp, ktfvs) = jsEPTail who kt
        tp           = if used then vsep [ bvdeclp, ktp ] else ktp
        tfvs         = if used then Set.union eefvs tfvs' else tfvs'

jsEPTail who (EP_Do (EP_Send i svs msg amt) (EP_Recv True _ _ _ kt)) = (tp, tfvs)
  where (amtp, amtfvs) = jsArg amt
        (ktp,  kfvs)   = jsEPTail who kt
        vs             = jsArray $ (map jsVar svs) ++ map jsVar msg
        tfvs           = Set.unions [ kfvs, amtfvs, Set.fromList svs, Set.fromList msg ]
        tp             = jsApply "ctc.sendrecv" [ jsString who
                                                , jsString (solMsg_fun i)
                                                , vs
                                                , amtp
                                                , jsString (solMsg_evt i)
                                                , jsLambda [ pretty "txn" ] ktp
                                                ] <> semi

jsEPTail who (EP_Do es kt) = (tp, tfvs)
  where (tp, esfvs) = jsEPStmt es ktp
        tfvs        = Set.union esfvs kfvs
        (ktp, kfvs) = jsEPTail who kt

jsEPTail _ (EP_Recv True _ _ _ _) =
  error "Impossible"

jsEPTail who (EP_Recv False i _ msg kt) = (tp, tfvs)
  where (ktp,  ktfvs) = jsEPTail who kt
        (who', event) = (jsString who, jsString (solMsg_evt i))
        block         = pretty blockdepth
        tcb           = pretty "ctc.timeoutTerminate"
        msg_vs        = map jsVar msg
        cb            = jsLambda (msg_vs ++ [pretty "txn"]) ktp
        tfvs          = Set.unions [Set.fromList msg, ktfvs]

        tp = if use_timeout
          then jsApply "ctc.recvWithin" [ who', event, block, tcb, cb ] <> semi
          else jsApply "ctc.recv"       [ who', event, cb             ] <> semi

jsEPTail who (EP_Loop which loopv inita bt) = (tp, tfvs)
  where (callp, callvs) = jsEPTail who (EP_Continue which inita)
        (bodyp, bodyvs) = jsEPTail who bt
        tp              = vsep [ defp, callp ]
        defp            = jsFunction (jsLoopName which) [ jsVar loopv ] bodyp <> semi
        tfvs            = Set.union callvs bodyvs

jsEPTail _who (EP_Continue which arg) = (tp, argvs)
  where (argp, argvs) = jsArg arg
        tp            = jsApply (jsLoopName which) [ argp ] <> semi


jsPart :: (Participant, EProgram) -> Doc a
jsPart (p, (EP_Prog pargs et)) =
  pretty "export" <+> jsFunction p args (fst $ jsEPTail p et)
  where args = [ pretty "stdlib", pretty "ctc", pretty "txn", pretty "interact" ]
            <> map jsVar pargs
            <> [ pretty "kTop" ]


emit_js :: BLProgram -> CompiledSol -> Doc a
emit_js (BL_Prog pm _) (abi, code) = modp
  where modp   = vsep_with_blank $ partsp ++ [ abip, codep ]
        partsp = map jsPart $ M.toList pm
        abip   = pretty $ "export const ABI = "      <> abi  <> ";"
        codep  = pretty $ "export const Bytecode = " <> code <> ";"
