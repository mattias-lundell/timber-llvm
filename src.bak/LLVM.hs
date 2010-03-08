module LLVM where

import Data.List
import Data.Char
--import Data.List.Split
import Numeric

-- a type has a type and a size in bytes (or words)
-- a struct has a total size and all elements have 
-- individual sizes and offsets, maybe store the
-- struct type as 
--
-- Tstruct String [(Type,Integer)]
-- Tstruct name   [(type,offset)]
--
-- functions sizeof, calculate size of a type
--           offset, calculate offset for a parameter in a struct

data LLVMCallingConvention = Ccc
                           | Fastccc
                           | Coldcc
                              
instance Show LLVMCallingConvention where
    show Ccc     = "ccc"
    show Fastccc = "fastccc"
    show Coldcc  = "coldcc"

data LLVMVisibilityStyle = Default
                         | Hidden
                         | Protected

instance Show LLVMVisibilityStyle where
    show Default   = "default"
    show Hidden    = "hidden"
    show Protected = "protected" 

data LLVMLinkage = Private
                 | LinkerPrivate
                 | Internal
                 | AvailableExternally
                 | Linkonce
                 | Weak
                 | Common
                 | Appending
                 | ExternWeak
                 | LinkonceOdr
                 | WeakOdr
                 | External
                 | Global   -- not a real linkage
                 | Constant -- not a real linkage
                   deriving (Eq, Ord)

instance Show LLVMLinkage where
    show Private             = "private"
    show LinkerPrivate       = "linker_private"
    show Internal            = "internal"
    show AvailableExternally = "available_externally"
    show Linkonce            = "linkonce"
    show Weak                = "weak" 
    show Common              = "common"
    show Appending           = "appending"
    show ExternWeak          = "extern_weak"
    show LinkonceOdr         = "linkonce_odr"
    show WeakOdr             = "weak_odr"
    show External            = "external"
    show Global              = "global"   -- not a real linkage
    show Constant            = "constant" -- not a real linkage
                 
data LLVMType = Tint Int
              | Tfloat
              | Tdouble
              | Tptr LLVMType
              | Tarray Int LLVMType
              | Tvoid
              | Tfun LLVMType [LLVMType] -- Tfun returnType [argTypes]
              | Topaque
              | Tstruct String
              | Tvector Int LLVMType
                deriving (Eq, Ord)

instance Show LLVMType where
    show (Tint bits)         = 'i' : show bits
    show Tfloat              = "float"
    show Tdouble             = "double"
    show (Tptr typ)          = show typ ++ "*"
    show (Tarray nelem typ)  = "[" ++ show nelem ++ " x " ++ show typ ++ "]" 
    show Tvoid               = "void"
    show (Tstruct sname)     = '%' : sname
    show Topaque             = "opaque"
    show (Tvector nelem typ) = "<" ++ show nelem ++ " x " ++ show typ ++ ">" 
    show (Tfun rettyp args)  = show rettyp ++ " (" ++ arglist args ++ ")" 
        where
          arglist args = concat (intersperse ", " (map show args))

data LLVMParameterAttribute = Zeroext
                            | Signext
                            | Inreg
                            | Byval
                            | Sret
                            | Noalias
                            | Nocapture
                            | Nest

instance Show LLVMParameterAttribute where                            
    show Zeroext   = "zeroext"
    show Signext   = "signext"
    show Inreg     = "inreg"
    show Byval     = "byval"
    show Sret      = "sret"
    show Noalias   = "noalias"
    show Nocapture = "nocapture"
    show Nest      = "nest"

data LLVMFunctionAttributes = Alwaysinline
                            | Inlinehint
                            | Noinline
                            | Optsize
                            | Noreturn
                            | Nounwind
                            | Readnone
                            | Readonly
                            | Ssp
                            | Sspreq
                            | Noredzone
                            | Noimplicitfloat
                            | Naked

instance Show LLVMFunctionAttributes where
    show Alwaysinline    = "alwaysinline"
    show Inlinehint      = "inlinehint"
    show Noinline        = "noinline"
    show Optsize         = "optsize"
    show Noreturn        = "noreturn"
    show Nounwind        = "nounwind"
    show Readnone        = "readnone"
    show Readonly        = "readonly"
    show Ssp             = "ssp"
    show Sspreq          = "sspreq"
    show Noredzone       = "noredzone"
    show Noimplicitfloat = "noimplicitfloat"
    show Naked           = "naked"

data LLVMGlobalInitializer = Zeroinitializer
                           | Null
                             
instance Show LLVMGlobalInitializer where
    show Zeroinitializer = "zeroinitializer"
    show Null            = "null"

--data LLVMModule = LLVMModule [LLVMGlobal] [LLVMFunctionDecl] [LLVMFunction]
--data LLVMModule = LLVMModule String [LLVMFunctionDecl] [LLVMGlobal] [LLVMType] [LLVMFunction]
--data LLVMGlobal = LLVMGlobal LLVMValue
--data LLVMFunctionDecl = LLVMFunctionDecl [LLVMLinkage] [LLVMVisibilityStyle] [LLVMCallingConvention] [LLVMType] LLVMFunctionName
--type LLVMFunctionName = String
--data LLVMFunction = LLVMFunction [LLVMLinkage] [LLVMVisibilityStyle] [LLVMCallingConvention] [LLVMParameterAttribute] LLVMType LLVMFunctionName [(String,LLVMType)] [LLVMFunctionAttributes] [LLVMBasicBlock]
--data LLVMBasicBlock = LLVMBasicBlock LLVMLabel [LLVMInstruction]
{-
data LLVMModule = LLVMmodule String [LLVMTopLevel]
                  deriving (Eq)
-}
data LLVMModule          = LLVMModule { modName     :: String,
                                        modTypDef   :: [LLVMStructDef],
                                        modGlobal   :: [LLVMGlobal],
                                        modFunDecl  :: [LLVMFunctionDecl],
                                        modTopConst :: [LLVMTopLevelConstant],
                                        modFuns     :: [LLVMFunction] }

data LLVMGlobal           = LLVMGlobal LLVMValue [LLVMLinkage] (Maybe LLVMGlobalInitializer) deriving (Show)
data LLVMFunctionDecl     = LLVMFunctionDecl [LLVMLinkage] String LLVMType deriving (Show)
data LLVMTopLevelConstant = LLVMTopLevelConstant LLVMValue [LLVMLinkage] LLVMValue deriving (Show)
data LLVMFunction         = LLVMFunction String LLVMType [(String,LLVMType)] [LLVMInstruction] deriving (Show)
data LLVMStructDef        = LLVMStructDef LLVMValue [LLVMType] deriving (Show)
{-
data LLVMTopLevel = LLVMfunction String LLVMType [(String,LLVMType)] [LLVMInstruction]
                  | LLVMvaluebind String LLVMType LLVMValue
                  | LLVMstruct String [(String,LLVMType)]
                  | LLVMGlobalVariable String LLVMType [LLVMValue]
                  | LLVMExternalVar String LLVMType
                  | LLVMExternalFun String LLVMType [LLVMType]
                  | LLVMGCArray String LLVMType [LLVMValue]
                  | LLVMString LLVMValue String
                    deriving (Eq)
-}

isStruct :: LLVMType -> Bool
isStruct (Tptr (Tstruct _)) = True
isStruct (Tstruct _) = True
isStruct _ = False

isArray :: LLVMType -> Bool
isArray (Tptr (Tarray _ _)) = True
isArray (Tarray _ _) = True
isArray _            = False

elemTyp :: LLVMType -> LLVMType
elemTyp (Tarray _ typ) = typ

isIntType :: LLVMType -> Bool
isIntType (Tint _) = True
isIntType _        = False

isFloatType :: LLVMType -> Bool
isFloatType Tfloat  = True
isFloatType Tdouble = False

data LLVMLabel = Label Integer
                 deriving (Eq,Ord)

instance Show LLVMLabel where
    show (Label l) = "label" ++ show l

data LLVMValue = LLVMRegister LLVMType RegName
               | LLVMConstant LLVMType ConstValue
                 deriving (Eq,Show)

type RegName = String

getTyp :: LLVMValue -> LLVMType
getTyp (LLVMRegister typ _) = typ
getTyp (LLVMConstant typ _) = typ

showWtyp :: LLVMValue -> String
showWtyp (LLVMRegister typ reg)   = show typ ++ " " ++ reg
showWtyp (LLVMConstant Tvoid _)   = "void"  
showWtyp (LLVMConstant typ const) = show typ ++ " " ++ show const

showWOtyp :: LLVMValue -> String
showWOtyp (LLVMRegister typ reg)   = reg
showWOtyp (LLVMConstant typ const) = show const

data ConstValue = IntConst Int
                | FloatConst Rational
                | CharConst Char
                | BoolConst Bool
                | VoidConst
                | VectorConst [LLVMValue]
                | ArrayConst [LLVMValue]
                | StringConst String
                | NullConst
                  deriving (Eq)

instance Show ConstValue where 
    show (IntConst i)      = show i
    show (FloatConst f)    = "0x" ++ toHex (fromRational f :: Float)
    show (CharConst c)     = show.ord $ c
    show (BoolConst True)  = "1"
    show (BoolConst False) = "0"
    show VoidConst         = "void"
    show (VectorConst vs)  = "<" ++ commaSepVals vs  ++ ">"
    show (StringConst s)   = s
    show (ArrayConst vs)   = "[" ++ commaSepVals vs  ++ "]"
    show NullConst         = "null"

toHex :: Float -> String
toHex 0   = "0000000000000000"
toHex num = map (intToDigit.fromBase 2) $ chunk 4 $ sign ++ exponent ++ mantissa
    where
      sign = if num < 0 then [1] else [0]
      exponent = padz 11 $ digits 2 $ (s-1) + 1023
      mantissa = zpad 52 f
      f = if (head.fst $ tup) == 1 then tail.fst $ tup else fst tup
      s = snd tup
      zpad n xs = take n $ xs ++ repeat 0 
      padz n xs = reverse.zpad n $ reverse xs
      tup = floatToDigits 2 (abs num)
      fromBase b ds = foldl' (\n k -> n * b + k) 0 ds

digits :: Integral n => n -> n -> [n]
digits base = reverse.digitsRev base
              where 
                digitsRev base i = case i of
                                     0 -> []
                                     _ -> lastDigit : digitsRev base rest
                                         where (rest, lastDigit) = quotRem i base

chunk :: Int -> [e] -> [[e]]
chunk _ [] = []
chunk i ls = take i ls : chunk i (drop i ls)

-- =============================================================================
-- COMPARING 
-- =============================================================================

data IcmpArg = IcmpEQ  
             | IcmpNE 
             | IcmpUGT 
             | IcmpUGE 
             | IcmpULT 
             | IcmpULE 
             | IcmpSGT 
             | IcmpSGE 
             | IcmpSLT 
             | IcmpSLE
               deriving (Eq)

instance Show IcmpArg where
    show IcmpEQ  = "eq"
    show IcmpNE  = "ne"
    show IcmpUGT = "ugt"
    show IcmpUGE = "uge"
    show IcmpULT = "ult"
    show IcmpULE = "ule"
    show IcmpSGT = "sgt"
    show IcmpSGE = "sge"
    show IcmpSLT = "slt"
    show IcmpSLE = "sle"

data FcmpArg = FcmpFALSE
             | FcmpOEQ
             | FcmpOGT
             | FcmpOGE
             | FcmpOLT
             | FcmpOLE
             | FcmpONE
             | FcmpORD
             | FcmpUEQ
             | FcmpUGT
             | FcmpUGE
             | FcmpULT
             | FcmpULE
             | FcmpUNE
             | FcmpUNO
             | FcmpTRUE
               deriving (Eq)

instance Show FcmpArg where
    show FcmpFALSE = "false"
    show FcmpOEQ   = "oeq"
    show FcmpOGT   = "ogt"
    show FcmpOGE   = "oge"
    show FcmpOLT   = "olt"
    show FcmpOLE   = "ole"
    show FcmpONE   = "one"
    show FcmpORD   = "ord"
    show FcmpUEQ   = "ueq"
    show FcmpUGT   = "ugt"
    show FcmpUGE   = "uge"
    show FcmpULT   = "ult"
    show FcmpULE   = "ule"
    show FcmpUNE   = "une"
    show FcmpUNO   = "uno"
    show FcmpTRUE  = "true"


-- =============================================================================
-- INSTRUCTIONS
-- =============================================================================
                     
data LLVMInstruction = Add             LLVMValue LLVMValue LLVMValue
                     | Fadd            LLVMValue LLVMValue LLVMValue
                     | Sub             LLVMValue LLVMValue LLVMValue
                     | Fsub            LLVMValue LLVMValue LLVMValue
                     | Mul             LLVMValue LLVMValue LLVMValue
                     | Fmul            LLVMValue LLVMValue LLVMValue
                     | Udiv            LLVMValue LLVMValue LLVMValue
                     | Sdiv            LLVMValue LLVMValue LLVMValue
                     | Fdiv            LLVMValue LLVMValue LLVMValue
                     | Urem            LLVMValue LLVMValue LLVMValue
                     | Srem            LLVMValue LLVMValue LLVMValue
                     | Frem            LLVMValue LLVMValue LLVMValue
                     -- Bitwise Binary Operations
                     | Shl             LLVMValue LLVMValue LLVMValue
                     | Lshr            LLVMValue LLVMValue LLVMValue
                     | Ashr            LLVMValue LLVMValue LLVMValue
                     | And             LLVMValue LLVMValue LLVMValue
                     | Or              LLVMValue LLVMValue LLVMValue
                     | Xor             LLVMValue LLVMValue LLVMValue
                     -- Memory Access and Addressing Operations
                     | Alloca          LLVMValue LLVMType
                     | Malloc          LLVMValue LLVMType
                     | Load            LLVMValue LLVMValue
                     | Store           LLVMValue LLVMValue 
                     | Getelementptr   LLVMValue LLVMValue [LLVMValue]
                     -- Vector Operations
                     | Extractelement  LLVMValue LLVMValue LLVMValue
                     | Insertelement   LLVMValue LLVMValue LLVMValue LLVMValue
                     | Shufflevector   LLVMValue LLVMValue LLVMValue LLVMValue
                     -- Conversion Operations
                     | Trunc           LLVMValue LLVMValue LLVMType
                     | Zext            LLVMValue LLVMValue LLVMType
                     | Sext            LLVMValue LLVMValue LLVMType
                     | Fptrunc         LLVMValue LLVMValue LLVMType
                     | Fpext           LLVMValue LLVMValue LLVMType
                     | Fptoui          LLVMValue LLVMValue LLVMType 
                     | Fptosi          LLVMValue LLVMValue LLVMType                        
                     | Uitofp          LLVMValue LLVMValue LLVMType
                     | Sitofp          LLVMValue LLVMValue LLVMType
                     | Ptrtoint        LLVMValue LLVMValue LLVMType
                     | Inttoptr        LLVMValue LLVMValue LLVMType
                     | Bitcast         LLVMValue LLVMValue LLVMType
                     -- Other Operations
                     | Icmp            LLVMValue IcmpArg LLVMValue LLVMValue
                     | Fcmp            LLVMValue FcmpArg LLVMValue LLVMValue
                     -- Three different kinds of functioncalls
                     | Call            (Maybe LLVMValue) LLVMType String [LLVMValue]
                     | Callhigher      LLVMValue LLVMType  LLVMValue [LLVMValue]
                     -- Terminator Instructions
                     | Switch          LLVMValue LLVMLabel [(LLVMValue,LLVMLabel)]
                     | Ret             LLVMValue
                     | Condbr          LLVMValue LLVMLabel LLVMLabel
                     | Uncondbr        LLVMLabel
                     | Lab             LLVMLabel 
                     | Unreachable
                       deriving (Eq,Show)
{-
binInstructionConcat :: String -> LLVMValue -> LLVMValue -> LLVMValue -> String
binInstructionConcat name res op1 op2 =
    concat [showWOtyp res, " = ", name, " ", showWtyp op1, ", ", showWOtyp op2]

conversionInstructionConcat :: String -> LLVMValue -> LLVMValue -> LLVMType -> String
conversionInstructionConcat name res op1 typ =
    concat [showWOtyp res, " = ", name, " ", showWtyp op1, " to ", show typ]

instance Show LLVMInstruction where
    -- binary instructions
    show (Add res op1 op2)  = binInstructionConcat "add"  res op1 op2
    show (Sub res op1 op2)  = binInstructionConcat "sub"  res op1 op2
    show (Mul res op1 op2)  = binInstructionConcat "mul"  res op1 op2
    show (Sdiv res op1 op2) = binInstructionConcat "sdiv" res op1 op2
    show (Srem res op1 op2) = binInstructionConcat "srem" res op1 op2
    show (Fadd res op1 op2) = binInstructionConcat "fadd" res op1 op2
    show (Fsub res op1 op2) = binInstructionConcat "fsub" res op1 op2
    show (Fmul res op1 op2) = binInstructionConcat "fmul" res op1 op2
    show (Fdiv res op1 op2) = binInstructionConcat "fdiv" res op1 op2
    show (Frem res op1 op2) = binInstructionConcat "frem" res op1 op2
    show (And res op1 op2)  = binInstructionConcat "and"  res op1 op2
    show (Or res op1 op2)   = binInstructionConcat "or"   res op1 op2
    show (Xor res op1 op2)  = binInstructionConcat "xor"  res op1 op2
    show (Shl res op1 op2)  = binInstructionConcat "shl"  res op1 op2
    show (Lshr res op1 op2) = binInstructionConcat "lshr" res op1 op2
    -- cast instructions
    show (Trunc res op1 typ)    = conversionInstructionConcat "trunc"    res op1 typ
    show (Zext res op1 typ)     = conversionInstructionConcat "zext"     res op1 typ
    show (Sext res op1 typ)     = conversionInstructionConcat "sext"     res op1 typ
    show (Fptrunc res op1 typ)  = conversionInstructionConcat "fptrunc"  res op1 typ
    show (Fpext res op1 typ)    = conversionInstructionConcat "fpext"    res op1 typ
    show (Fptoui res op1 typ)   = conversionInstructionConcat "fptoui"   res op1 typ
    show (Fptosi res op1 typ)   = conversionInstructionConcat "fptosi"   res op1 typ
    show (Uitofp res op1 typ)   = conversionInstructionConcat "uitofp"   res op1 typ
    show (Sitofp res op1 typ)   = conversionInstructionConcat "sitofp"   res op1 typ
    show (Ptrtoint res op1 typ) = conversionInstructionConcat "ptrtoint" res op1 typ
    show (Inttoptr res op1 typ) = conversionInstructionConcat "inttoptr" res op1 typ
    show (Bitcast res op1 typ)  = conversionInstructionConcat "bitcast"  res op1 typ
    -- memory related instructions
    show (Alloca res typ) = concat [showWOtyp res, " = alloca ", show typ]
    show (Malloc res typ) = concat [showWOtyp res, " = malloc ", show typ]
    show (Load res op1)   = concat [showWOtyp res, " = load ", showWtyp op1]
    show (Store res op1)  = concat ["store ", showWtyp res, ", ", showWtyp op1]
    -- getelement
    show (Getelementptr res op1 offset) = concat [showWOtyp res, " = getelementptr ", showWtyp op1, ", i32 0, ", commaSepVals offset]
    -- comapre
    show (Icmp res cmp op1 op2) = concat [showWOtyp res, " = icmp ", show cmp, " ", showWtyp op1, ", ", showWOtyp op2]
    show (Fcmp res cmp op1 op2) = concat [showWOtyp res, " = fcmp ", show cmp, " ", showWtyp op1, ", ", showWOtyp op2]
    -- branch
    show (Condbr op l1 l2) = concat ["br i1 ", showWOtyp op, ", label %", show l1, ", label %", show l2 ]
    show (Uncondbr label)  = concat ["br label %", show label]
    -- return
    show (Ret op1) = concat ["ret ", showWtyp op1]
    -- call
    show (Call Nothing Tvoid name args)    = concat ["call void @", name, "(", commaSepVals args, ")"]
    show (Call (Just res) typ name args)   = concat [showWOtyp res, " = call ", show typ, " @", name, "(", commaSepVals args, ")"]
    show (Callhigher res typ funAddr args) = concat [showWOtyp res, " = call ", show typ, " ", showWOtyp funAddr, "(", commaSepVals args, ")"]
    -- label
    show (Lab label) = concat [show label, ":"]
    -- unreachable
    show Unreachable = "unreachable"
-}

commaSepVals :: [LLVMValue] -> String
commaSepVals [] = ""
commaSepVals xs = concat (intersperse ", " [ showWtyp x | x <- xs])






