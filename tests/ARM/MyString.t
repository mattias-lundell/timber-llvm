module MyString where

inttochar 0x0 = '0'
inttochar 0x1 = '1'
inttochar 0x2 = '2'
inttochar 0x3 = '3'
inttochar 0x4 = '4'
inttochar 0x5 = '5'
inttochar 0x6 = '6'
inttochar 0x7 = '7'
inttochar 0x8 = '8'
inttochar 0x9 = '9'
inttochar 0xA = 'A'
inttochar 0xB = 'B'
inttochar 0xC = 'C'
inttochar 0xD = 'D'
inttochar 0xE = 'E'
inttochar 0xF = 'F'

inttostr_r :: Int -> Int -> [Char]
inttostr_r 0 radix = []
inttostr_r i radix = inttochar( ( (i `mod` radix) )) : (inttostr_r (i `div` radix) radix)

inttostr :: Int -> Int -> [Char]
inttostr 0 radix = "0"
inttostr i radix = if (i>0) then (reverse (inttostr_r i radix)) else ('-':(reverse (inttostr_r (-i) radix)))
