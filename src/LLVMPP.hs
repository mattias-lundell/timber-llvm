{-# LANGUAGE FlexibleInstances #-}

module LLVMPP (ppLLVMModule) where

import LLVM
import LLVMKindle hiding (char)
import Text.PrettyPrint 
import Data.List
import Data.Char
import Numeric

showLLVMstring :: String -> String
showLLVMstring "" = ""
showLLVMstring (c:cs) 
    | isLetter c = c:showLLVMstring cs
    | otherwise = "\\" ++ sChar c ++ showLLVMstring cs
                  where
                    sChar c 
                        | ord c < 10 = '0' : show (ord c)
                        | ord c < 16 = '0' : (showHex.ord) c ""
                        | otherwise  = (showHex.ord) c ""

ppLLVMModule :: LLVMModule -> String
ppLLVMModule (LLVMModule name td gs fds cs fs) = render file
    where 
      file = vcat $
             [text "; typedefinitions"] ++
             map pp td ++ 
             [newline <> text "; global variables"] ++
             map ppGlobalValue gs ++ 
             [newline <> text "; external functions"] ++
             map pp fds ++ 
             [newline <> text "; toplevel constant"] ++
             map pp cs ++ 
             [newline <> text "; local functions"] ++
             map pp fs

ppParams :: [(String,LLVMType)] -> Doc
ppParams ((name,typ):p:ps) = ppType typ <> text " %" <> text name <> comma <> space <> ppParams (p:ps)
ppParams ((name,typ):[]) = ppType typ <> text " %" <> text name 
ppParams [] = text ""

ppType :: LLVMType -> Doc
ppType (Tstruct sname) = text $ sname ++ "*"
ppType typ = text $ show typ

textConcat :: [String] -> Doc
textConcat xs = text $ concat xs

ppValWtyp :: LLVMValue -> Doc
ppValWtyp r@(LLVMRegister typ _ _) = pp typ <+> ppValWOtyp r
ppValWtyp (LLVMConstant Tvoid _)   = text "void"
ppValWtyp r@(LLVMConstant typ const) = pp typ <+> ppValWOtyp r

ppGlobalValue :: LLVMValue -> Doc  
ppGlobalValue r@(LLVMRegister typ reg (TagGlobal linkage Nothing)) = 
        ppValWOtyp r <+> equals <+> pp linkage <+> 
        pp (if isPtr typ then drop1Ptr typ else typ)
ppGlobalValue r@(LLVMRegister typ reg (TagGlobal linkage (Just init))) = 
        ppValWOtyp r <+> equals <+> pp linkage <+> 
        pp (if isPtr typ then drop1Ptr typ else typ) <+> pp init
       
ppValWOtyp :: LLVMValue -> Doc
ppValWOtyp (LLVMRegister _ reg (TagGlobal _ _)) = text "@" <> text reg
ppValWOtyp (LLVMRegister _ reg TagLocal) = text "%" <> text reg
ppValWOtyp (LLVMConstant typ const) = text (show const)

ppBinInstruction :: String -> LLVMValue -> LLVMValue -> LLVMValue -> Doc
ppBinInstruction name res op1 op2 = 
    ppValWOtyp res <+> equals <+> text name <+> 
    ppValWtyp op1 <> comma <+> ppValWOtyp op2

ppCastInstruction :: String -> LLVMValue -> LLVMValue -> LLVMType -> Doc
ppCastInstruction name res op1 typ = 
    ppValWOtyp res <+> equals <+> text name <+> 
    ppValWtyp op1 <+> text "to" <+> pp typ

ppCommaSepVals :: [LLVMValue] -> Doc
ppCommaSepVals [] = text ""
ppCommaSepVals xs = text $ concat (intersperse ", " [ showWtyp x | x <- xs])

newline :: Doc
newline = text "\n"

at :: Doc
at = char '@'

class PP a where
    pp :: a -> Doc

instance PP IcmpArg where
    pp arg = text (show arg)

instance PP FcmpArg where
    pp arg = text (show arg)

instance PP LLVMType where
    pp typ = text $ show typ

instance PP [LLVMLinkage] where
    pp linkage = hcat $ punctuate (text " ") (map (text.show) linkage)

instance PP LLVMGlobalInitializer where
    pp init = text $ show init

instance PP LLVMGlobal where
    pp (LLVMGlobal val linkage Nothing) = 
        ppValWOtyp val <+> equals <+> pp linkage <+> pp (getTyp val)
    pp (LLVMGlobal val linkage (Just init)) = 
        ppValWOtyp val <+> equals <+> pp linkage <+> pp (getTyp val) <+> pp init

instance PP LLVMFunctionDecl where
    pp (LLVMFunctionDecl linkage fname (Tfun rettyp argtyps)) =
        text "declare" <+> pp rettyp <+> at <> text fname <> parens (text $ concat $ intersperse ", " (map show argtyps))

instance PP LLVMTopLevelConstant where
    pp (LLVMTopLevelConstant reg linkage (LLVMConstant typ (StringConst s))) =
        ppValWOtyp reg <+> equals <+> pp linkage <+> pp typ <+> text "c\"" <> text (showLLVMstring s) <>
        text "\\00\"" <+> text ";" <+> text (filter isPrint s)
    pp (LLVMTopLevelConstant reg linkage val) =
        ppValWOtyp reg <+> equals <+> pp linkage <+> ppValWtyp val

instance PP LLVMFunction where
    pp (LLVMFunction fname (Just cc) rettyp params code) = 
        text "define" <+> text (show cc) <+> pp rettyp <+> at <> text fname <>
        lparen <> ppParams params <> rparen <> space <> lbrace $$
        nest 8 (vcat $ map pp code) $$ rbrace <> newline
    pp (LLVMFunction fname Nothing rettyp params code) = 
        text "define" <+> pp rettyp <+> at <> text fname <>
        lparen <> ppParams params <> rparen <> space <> lbrace $$
        nest 8 (vcat $ map pp code) $$ rbrace <> newline

instance PP LLVMStructDef where
    pp (LLVMStructDef reg typs) = 
        ppValWOtyp reg <+> equals <+> text "type" <+> braces (hcat $ punctuate (text ", ") (map pp typs)) -- (vcat $ map pp typs) <+> rbrace 

instance PP LLVMInstruction where
    -- binary instructions
    pp (Add res op1 op2)  = ppBinInstruction "add"  res op1 op2
    pp (Sub res op1 op2)  = ppBinInstruction "sub"  res op1 op2
    pp (Mul res op1 op2)  = ppBinInstruction "mul"  res op1 op2
    pp (Sdiv res op1 op2) = ppBinInstruction "sdiv" res op1 op2
    pp (Srem res op1 op2) = ppBinInstruction "srem" res op1 op2
    pp (Fadd res op1 op2) = ppBinInstruction "fadd" res op1 op2
    pp (Fsub res op1 op2) = ppBinInstruction "fsub" res op1 op2
    pp (Fmul res op1 op2) = ppBinInstruction "fmul" res op1 op2
    pp (Fdiv res op1 op2) = ppBinInstruction "fdiv" res op1 op2
    pp (Frem res op1 op2) = ppBinInstruction "frem" res op1 op2
    pp (And res op1 op2)  = ppBinInstruction "and"  res op1 op2
    pp (Or res op1 op2)   = ppBinInstruction "or"   res op1 op2
    pp (Xor res op1 op2)  = ppBinInstruction "xor"  res op1 op2
    pp (Shl res op1 op2)  = ppBinInstruction "shl"  res op1 op2
    pp (Lshr res op1 op2) = ppBinInstruction "lshr" res op1 op2
    pp (Ashr res op1 op2) = ppBinInstruction "ashr" res op1 op2
    -- cast instructions
    pp (Trunc res op1 typ)    = ppCastInstruction "trunc"    res op1 typ
    pp (Zext res op1 typ)     = ppCastInstruction "zext"     res op1 typ
    pp (Sext res op1 typ)     = ppCastInstruction "sext"     res op1 typ
    pp (Fptrunc res op1 typ)  = ppCastInstruction "fptrunc"  res op1 typ
    pp (Fpext res op1 typ)    = ppCastInstruction "fpext"    res op1 typ
    pp (Fptoui res op1 typ)   = ppCastInstruction "fptoui"   res op1 typ
    pp (Fptosi res op1 typ)   = ppCastInstruction "fptosi"   res op1 typ
    pp (Uitofp res op1 typ)   = ppCastInstruction "uitofp"   res op1 typ
    pp (Sitofp res op1 typ)   = ppCastInstruction "sitofp"   res op1 typ
    pp (Ptrtoint res op1 typ) = ppCastInstruction "ptrtoint" res op1 typ
    pp (Inttoptr res op1 typ) = ppCastInstruction "inttoptr" res op1 typ
    pp (Bitcast res op1 typ)  = ppCastInstruction "bitcast"  res op1 typ
    -- memory related instructions 
    pp (Alloca res typ) = ppValWOtyp res <+> equals <+> text "alloca" <+> pp typ
    pp (Malloc res typ) = ppValWOtyp res <+> equals <+> text "malloc" <+> pp typ
    pp (Load res op1)   = ppValWOtyp res <+> equals <+> text "load" <+> ppValWtyp op1
    pp (Store res op1)  = text "store" <+> ppValWtyp res <+> comma <+> ppValWtyp op1
    pp (Extractelement res op1 index)     = ppValWOtyp res <+> equals <+> text "extractelement" <+> ppValWtyp op1 <> comma <+> ppValWtyp index
    pp (Insertelement  res op1 op2 index) = ppValWOtyp res <+> equals <+> text "insertelement" <+> ppValWtyp op1 <> comma <+> ppValWtyp op2 <> comma <+> ppValWtyp index
    pp (Shufflevector  res op1 op2 op3)   = ppValWOtyp res <+> equals <+> text "shufflevetor" <+> ppValWtyp op1 <+> comma <> comma <+> ppValWtyp op2 <> comma <+> ppValWtyp op3
    -- getelement
    pp (Getelementptr res op1 offset) = ppValWOtyp res <+> equals <+> text "getelementptr" <+> ppValWtyp op1 <+> comma <+> ppValWtyp (intConst 0) <> comma <+> ppCommaSepVals offset
    -- comapre
    pp (Icmp res cmp op1 op2) = ppValWOtyp res <+> equals <+> text "icmp" <+> pp cmp <+> ppValWtyp op1 <> comma <+> ppValWOtyp op2
    pp (Fcmp res cmp op1 op2) = ppValWOtyp res <+> equals <+> text "fcmp" <+> pp cmp <+> ppValWtyp op1 <> comma <+> ppValWOtyp op2
    -- branch
    pp (Switch op defaultLabel lls) = text "switch" <+> comma <+> text "label %" <> text (show defaultLabel) <+> text "[" <+> ppCommaSepLabels lls <+> text "]" 
    pp (Condbr op l1 l2) = text "br" <+> pp (Tint 1) <+> ppValWOtyp op <> comma <+> text "label %" <> text (show l1) <> comma <+> text "label %" <> text (show l2)
    pp (Uncondbr label)  = text "br" <+> text "label %" <> text (show label)
    -- return
    pp (Ret op1) = text "ret" <+> ppValWtyp op1
    -- call
    pp (Call Nothing Tvoid name args)    = text "call void @" <> text name <> lparen <> ppCommaSepVals args <> rparen
    pp (Call (Just res) typ name args)   = ppValWOtyp res <+> equals <+> text "call" <+> pp typ <+> text "@" <> text name <> lparen <> ppCommaSepVals args <> rparen
    pp (Callhigher res typ funAddr args) = ppValWOtyp res <+> equals <+> text "call" <+> pp typ <+> ppValWOtyp funAddr <> lparen <> ppCommaSepVals args <> rparen
    -- label -}
    pp (Lab l) = nest (-4) (text (show l)) <> colon
    -- unreachable
    pp Unreachable = text "unreachable" 


ppCommaSepLabels [] = text ""    
ppCommaSepLabels lls = text $ concat (intersperse ", " [ showWtyp val ++ ", label %" ++ show lab | (val,lab) <- lls])
