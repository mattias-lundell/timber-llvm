{-# LANGUAGE GADTs #-}

module LLVM where

import Data.List (intercalate, foldl')
import Data.Char (ord, intToDigit)
import Data.List.Split (chunk)
import Numeric

data LLVMCallingConvention = Cc | Fastcc | Coldcc
                              
instance Show LLVMCallingConvention where
    show Cc     = "cc"
    show Fastcc = "fastcc"
    show Coldcc  = "coldcc"

data LLVMVisibilityStyle = Default | Hidden | Protected

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
              | Tunion [LLVMType]
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
    show (Tunion typs)       = 
        "union {" ++ intercalate ", " (map show typs) ++ "}"
    show (Tfun rettyp args)  = 
        show rettyp ++ " (" ++ intercalate ", " (map show args) ++ ")" 

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

data LLVMGlobalInitializer = Zeroinitializer | Null deriving (Eq)
                             
instance Show LLVMGlobalInitializer where
    show Zeroinitializer = "zeroinitializer"
    show Null            = "null"

data LLVMModule          = LLVMModule { modName     :: String,
                                        modTypDef   :: [LLVMStructDef],
                                        modGlobal   :: [LLVMValue],
                                        modFunDecl  :: [LLVMFunctionDecl],
                                        modTopConst :: [LLVMTopLevelConstant],
                                        modFuns     :: [LLVMFunction] }

data LLVMFunctionDecl     = LLVMFunctionDecl [LLVMLinkage] String LLVMType deriving (Show)
data LLVMTopLevelConstant = LLVMTopLevelConstant LLVMValue [LLVMLinkage] LLVMValue deriving (Show)
data LLVMFunction         = LLVMFunction String (Maybe LLVMCallingConvention) LLVMType [(String,LLVMType)] [LLVMInstruction] deriving (Show)
data LLVMStructDef        = LLVMStructDef LLVMValue [LLVMType] deriving (Show)

data LLVMLabel = Label Integer deriving (Eq,Ord)

instance Show LLVMLabel where
    show (Label l) = "label" ++ show l

data LLVMValue = LLVMRegister LLVMType String LLVMRegisterTag
               | LLVMConstant LLVMType ConstValue
                 deriving (Eq,Show)

data LLVMRegisterTag = TagGlobal [LLVMLinkage] (Maybe LLVMGlobalInitializer)
                     | TagLocal
                       deriving (Eq, Show)

getTyp :: LLVMValue -> LLVMType
getTyp (LLVMRegister typ _ _) = typ
getTyp (LLVMConstant typ _) = typ

showWtyp :: LLVMValue -> String
showWtyp (LLVMRegister typ reg (TagGlobal _ _)) = 
    show typ ++ " @" ++ reg
showWtyp (LLVMRegister typ reg TagLocal) = 
    show typ ++ " %" ++ reg
showWtyp (LLVMConstant Tvoid _)   = "void"  
showWtyp (LLVMConstant typ const) = show typ ++ " " ++ show const

showWOtyp :: LLVMValue -> String
showWOtyp (LLVMRegister typ reg (TagGlobal _ _)) = '@' : reg
showWOtyp (LLVMRegister typ reg TagLocal)        = '%' : reg
showWOtyp (LLVMConstant typ const)               = show const

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
    show (VectorConst vs)  = "<" ++ intercalate ", " (map showWtyp vs)  ++ ">"
    show (StringConst s)   = s
    show (ArrayConst vs)   = "[" ++ intercalate ", " (map showWtyp vs)  ++ "]"
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
     
data LLVMInstruction where
    Add  :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Fadd :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Sub  :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Fsub :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Mul  :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Fmul :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Udiv :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Sdiv :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Fdiv :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Urem :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Srem :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Frem :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    -- Bitwise Binary Operations
    Shl  :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Lshr :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Ashr :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    And  :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Or   :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Xor  :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    -- Memory Access and Addressing Operations
    Alloca        :: LLVMValue -> LLVMType -> LLVMInstruction
    Malloc        :: LLVMValue -> LLVMType -> LLVMInstruction
    Load          :: LLVMValue -> LLVMValue -> LLVMInstruction
    Store         :: LLVMValue -> LLVMValue -> LLVMInstruction
    Getelementptr :: LLVMValue -> LLVMValue -> [LLVMValue] -> LLVMInstruction
    -- Vector Operations
    Extractelement :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Insertelement  :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    Shufflevector  :: LLVMValue -> LLVMValue -> LLVMValue -> LLVMValue -> LLVMInstruction
    -- Conversion Operations
    Trunc    :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction
    Zext     :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction
    Sext     :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction
    Fptrunc  :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction
    Fpext    :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction
    Fptoui   :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction
    Fptosi   :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction    
    Uitofp   :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction
    Sitofp   :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction
    Ptrtoint :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction
    Inttoptr :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction
    Bitcast  :: LLVMValue -> LLVMValue -> LLVMType -> LLVMInstruction
    -- Other Operations
    Icmp :: LLVMValue -> IcmpArg -> LLVMValue -> LLVMValue -> LLVMInstruction
    Fcmp :: LLVMValue -> FcmpArg -> LLVMValue -> LLVMValue -> LLVMInstruction
    -- Three different kinds of functioncalls
    Call       :: Maybe LLVMValue -> LLVMType -> String -> [LLVMValue] -> LLVMInstruction
    Callhigher :: LLVMValue -> LLVMType -> LLVMValue -> [LLVMValue] -> LLVMInstruction
    -- Terminator Instructions
    Switch      :: LLVMValue -> LLVMLabel -> [(LLVMValue,LLVMLabel)] -> LLVMInstruction
    Ret         :: LLVMValue -> LLVMInstruction
    Condbr      :: LLVMValue -> LLVMLabel -> LLVMLabel -> LLVMInstruction
    Uncondbr    :: LLVMLabel -> LLVMInstruction
    Unreachable :: LLVMInstruction
    -- Label are not instructions in llvm
    Lab :: LLVMLabel  -> LLVMInstruction
                     deriving (Eq,Show)
