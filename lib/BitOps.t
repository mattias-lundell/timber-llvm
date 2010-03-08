-- The Timber compiler <timber-lang.org>
--
-- Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 
-- 3. Neither the names of the copyright holder and any identified
--    contributors, nor the names of their affiliations, may be used to 
--    endorse or promote products derived from this software without 
--    specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
-- OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
-- ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

module BitOps where


instance intBITS32 :: IntLiteral BITS32 where
    fromInt = primIntToBITS32

instance intBITS16 :: IntLiteral BITS16 where
    fromInt = primIntToBITS16

instance intBITS8 :: IntLiteral BITS8 where
    fromInt = primIntToBITS8

    
default intInt < intBITS8
default intInt < intBITS16
default intInt < intBITS32

typeclass ToInt a where
    toInt :: a -> Int

instance fromBITS32 :: ToInt BITS32 where
    toInt = primBITS32ToInt

instance fromBITS16 :: ToInt BITS16 where
    toInt = primBITS16ToInt

instance fromBITS8 :: ToInt BITS8 where
    toInt = primBITS8ToInt
    

typeclass BitsOp a where
    band :: a -> a -> a        
    bor  :: a -> a -> a        
    bxor :: a -> a -> a        
    binv :: a -> a             
    bsll :: a -> Int -> a      
    bsrl :: a -> Int -> a      
    bsra :: a -> Int -> a      
    bset :: a -> Int -> a      
    bclr :: a -> Int -> a      
    btst :: a -> Int -> Bool   

a .&. b  = band a b
a .|. b  = bor  a b
a .^. b  = bxor a b  
a .<<. b = bsll a b   
a .>>. b = bsrl a b 
a .|=. b = bset a b   
a .!=. b = bclr a b  
a .?. b  = btst a b         

instance bitsOpBits32 :: BitsOp BITS32 where
    band   a b = primAND32 a b
    bor    a b = primOR32  a b
    bxor   a b = primEXOR32 a b
    binv   a   = primNOT32 a
    bsll   a i = primSHIFTL32 a i
    bsrl   a i = primSHIFTR32 a i
    bsra   a i = primSHIFTRA32 a i
    bset   a i = primSET32 a i  
    bclr   a i = primCLR32 a i
    btst   a i = primTST32 a i

instance bitsOpBits16 :: BitsOp BITS16 where
    band   a b = primAND16 a b
    bor    a b = primOR16  a b
    bxor   a b = primEXOR16 a b
    binv   a   = primNOT16 a
    bsll   a i = primSHIFTL16 a i
    bsrl   a i = primSHIFTR16 a i
    bsra   a i = primSHIFTRA16 a i
    bset   a i = primSET16 a i  
    bclr   a i = primCLR16 a i
    btst   a i = primTST16 a i

instance bitsOpBits8 :: BitsOp BITS8 where
    band   a b = primAND8 a b
    bor    a b = primOR8  a b
    bxor   a b = primEXOR8 a b
    binv   a   = primNOT8 a
    bsll   a i = primSHIFTL8 a i
    bsrl   a i = primSHIFTR8 a i
    bsra   a i = primSHIFTRA8 a i
    bset   a i = primSET8 a i  
    bclr   a i = primCLR8 a i
    btst   a i = primTST8 a i

                
instance eqBits32 :: Eq BITS32 where
    a == b = primBITS32ToInt a == primBITS32ToInt b
    a /= b = primBITS32ToInt a /= primBITS32ToInt b

instance eqBits16 :: Eq BITS16 where
    a == b = primBITS16ToInt a == primBITS16ToInt b
    a /= b = primBITS16ToInt a /= primBITS16ToInt b

instance eqBits8 :: Eq BITS8 where
    a == b = primBITS8ToInt a == primBITS8ToInt b
    a /= b = primBITS8ToInt a /= primBITS8ToInt b


showbits :: a -> Int -> String \\ BitsOp a

showbits a 0 = ""
showbits a n = if btst a n1 then '1' : str else '0' : str
  where n1  = n-1
        str = showbits a n1
                    
instance showBits32 :: Show BITS32 where 
    show a = "0b" ++ showbits a 32

instance showBits16 :: Show BITS16 where 
    show a = "0b" ++ showbits a 16

instance showBits8 :: Show BITS8 where 
    show a = "0b" ++ showbits a 8


hex a = if (a <= 9) then 
            (chr (a + (ord '0'))) 
        else
            (chr ((a -10) + (ord 'A'))) 

    
showh :: BITS32 -> Int -> String    
showh a 0 = ""
showh a n = ( hex (toInt((a `bsrl` (4 * n1)) `band` 0xF)) ) : str 
    where n1 = n - 1
          str = showh a n1

typeclass ShowHex a where
    showhex :: a -> String

instance showHBits32 :: ShowHex BITS32 where 
    showhex a = "0x" ++ showh a 8

instance showHBits16 :: ShowHex BITS16 where 
    showhex a = "0x" ++ showh (fromInt(toInt a)) 4

instance showHBits8 :: ShowHex BITS8 where 
    showhex a = "0x" ++ showh (fromInt(toInt a)) 2
