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

{-
**************************************************************************
** This file is based on sources distributed as the haskell-src package **
**************************************************************************
The Glasgow Haskell Compiler License

Copyright 2004, The University Court of the University of Glasgow. 
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.
 
- Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.
 
- Neither name of the University nor the names of its contributors may be
used to endorse or promote products derived from this software without
specific prior written permission. 

THIS SOFTWARE IS PROVIDED BY THE UNIVERSITY COURT OF THE UNIVERSITY OF
GLASGOW AND THE CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
UNIVERSITY COURT OF THE UNIVERSITY OF GLASGOW OR THE CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
DAMAGE.
-}

module Lexer (Token(..), lexer, readInteger, readNumber, readRational) where


import ParseMonad
import Char
import Ratio
import Token
import Common


{-

The source location, (y,x), is the coordinates of the previous token.
col is the current column in the source file.  If col is 0, we are
somewhere at the beginning of the line before the first token.

Setting col to 0 is used in two places: just after emitting a virtual
close brace due to layout, so that next time through we check whether
we also need to emit a semi-colon, and at the beginning of the file,
to kick off the lexer.

-}
lexer :: (Token -> PM a) -> PM a
lexer contPM =
    PM $ \input (y,x) col state ->
    if col == 0 then tab y x   True  input  col state
                else tab y col False input  col state -- throw away old x
    where
    -- move past whitespace and comments
    tab y x bol []          col state = cont EOF [] (y,x) col state
    tab y x bol ('\t':s)    col state = tab y (nextTab x) bol s col state 
    tab y x bol ('\n':s)    col state = newLine s y col state 
    tab y x bol ('-':'-':s) col state = newLine (drop 1 (dropWhile (/= '\n') s)) y col state
    tab y x bol ('{':'-':s) col state = nestedComment tab y x bol s col state 
    tab y x bol (c:s)       col state 
                 | isSpace c = tab y (x + 1) bol s col state 
                 | otherwise =
                     if bol then
                         lexBOL cont (c:s) (y,x) x state
                     else
                         lexToken cont (c:s) (y,x) x state

    newLine s y col state = tab (y + 1) 1 True s col state 

    cont t = unPM $ contPM t

nextTab x = x + (tab_length - (x - 1) `mod` tab_length)

type P a = String -> (Int,Int) -> Int -> ParseState -> ParseResult a

{-

When we are lexing the first token of a line, check whether we need to
insert virtual semicolons or close braces due to layout.

-}
lexBOL :: (Token -> P a) -> P a
lexBOL cont s loc@(y,x) col state =
    if need_close_curly x state then 
        -- tr' ("layout: inserting '}' at " ++ show loc ++ " " ++ show state ++ "\n") $
        -- Set col to 0, indicating that we're still at the
        -- beginning of the line, in case we need a semi-colon too.
        -- Also pop the context here, so that we don't insert
        -- another close brace before the parser can pop it.
        cont VRightCurly s loc 0 (tail state)
    else if need_semi_colon x state then
        -- tr' ("layout: inserting ';' at " ++ show loc ++ " " ++ show state ++ ", input: " ++ s ++ "\n") $
        cont SemiColon s loc col state
    else
        lexToken cont s loc col state


need_close_curly x []    = False
need_close_curly x (Layout n:Layout m:_)
  | n <= m               = True
need_close_curly x (i:_) = case i of
                           NoLayout -> False
                           Layout n -> x < n
                           RecLayout n -> False

need_semi_colon x []     = False
need_semi_colon x (i:_)  = case i of
                           NoLayout -> False
                           Layout n -> x == n
                           RecLayout n -> x == n

lexToken :: (Token -> P a) -> P a
lexToken cont [] loc x state = 
        internalError0 "Lexer.lexToken: empty input stream."
lexToken cont (c:s) loc@(y,x') x state =
        -- tr' ("lexer: y = " ++ show y ++ " x = " ++ show x ++ "   " ++ show state ++ "\n") $
        case c of
        -- First the special symbols
        '(' -> special LeftParen
        ')' -> special RightParen
        ',' -> special Comma
        ';' -> special SemiColon
        '[' -> special LeftSquare
        ']' -> special RightSquare
        '`' -> special BackQuote
        '{' -> special LeftCurly
        '}' -> case state of 
               (_:ctxt) -> cont RightCurly s loc (x+1) ctxt
               []       -> Failed "parse error (possibly incorrect indentation)"

        '\'' -> lexChar cont s loc (x + 1) state
        '\"' -> lexString cont s loc (x + 1) state

        '_' | null s || not (isIdent (head s)) -> special Wildcard
        
        c | isDigit c ->
              case lexInt (c:s) of
              Decimal (n, rest) ->
                  case rest of
                  ('.':c2:rest2) | isDigit c2 ->
                                     case lexFloatRest (c2:rest2) of
                                     Nothing -> Failed "illegal float."
                                     Just (n2,rest3) ->
                                         let f = n ++ ('.':n2) in
                                         forward (length f) (FloatTok f) rest3
                  _ -> forward (length n) (IntTok n) rest
              Octal       (n,rest) -> forward (length n) (IntTok n) rest
              Hexadecimal (n,rest) -> forward (length n) (IntTok n) rest

          | isLower c -> lexVarId "" c s

          | isUpper c -> lexQualName "" c s

          | isSymbol c -> lexSymbol "" c s

          | otherwise ->
              Failed ("illegal character \'" ++ showLitChar c "" ++ "\'\n")

      where
      special t = forward 1 t s 
      forward n t s = cont t s loc (x + n) state
 
      join "" n = n
      join q n  = q ++ '.' : n

      len "" n = length n
      len q n  = length q + length n + 1

      lexFloatRest r = case span isDigit r of
                       (r2, 'e':r3) -> lexFloatExp (r2 ++ "e") r3
                       (r2, 'E':r3) -> lexFloatExp (r2 ++ "e") r3
                       f@(r2,   r3) -> Just f
 
      lexFloatExp r1 ('-':r2) = lexFloatExp2 (r1 ++ "-") r2
      lexFloatExp r1 ('+':r2) = lexFloatExp2 (r1 ++ "+") r2
      lexFloatExp r1      r2  = lexFloatExp2 r1          r2

      lexFloatExp2 r1 r2 = case span isDigit r2 of
                           ("", _ ) -> Nothing
                           (ds, r3) -> Just (r1++ds,r3)
      
      lexVarId q c s = let (vidtail, rest) = span isIdent s
                           vid             = c:vidtail
                           l_vid           = len q vid
              in
                  case lookup vid reserved_ids of
                  Just keyword -> case q of
                                    "" -> forward l_vid keyword rest
                                    _ -> Failed "illegal qualified name"
                  Nothing      -> forward l_vid (VarId (q,vid)) rest

      lexQualName q c s = let (contail, rest) = span isIdent s
                              con             = join q (c:contail)
                              l_con           = length con
                          in case rest of
                               '.': c' : rest'
                                  | isUpper c' -> lexQualName con c' rest'
                                  | isLower c' -> lexVarId con c' rest'
                                  | isSymbol c' -> lexSymbol con c' rest'
                                  | otherwise -> Failed "illegal qualified name"
                               _ -> forward l_con (ConId (q,c:contail)) rest
                  
      lexSymbol q c s = let (symtail, rest) = span isSymbol s
                            sym             = c:symtail
                            l_sym = len q sym
                        in case lookup sym reserved_ops of
                                Just t  -> case q of
                                    "" -> forward l_sym t rest
                                    _ -> Failed "illegal qualified name"
                                Nothing -> case c of
                                 ':' -> forward l_sym (ConSym (q,sym)) rest
                                 _   -> forward l_sym (VarSym (q,sym)) rest


lexInt ('0':o:d:r) | toLower o == 'o' && isOctDigit d
    = let (ds, rs) = span isOctDigit r
      in Octal ('0':'o':d:ds, rs)
lexInt ('0':x:d:r) | toLower x == 'x' && isHexDigit d
    = let (ds, rs) = span isHexDigit r
      in Hexadecimal ('0':'x':d:ds, rs)
lexInt r = Decimal (span isDigit r)


lexChar :: (Token -> P a) -> P a
lexChar cont s loc@(y,_) x state =
        case s of
        '\\':s ->
            let (e, s2, i) =
                  runPM (escapeChar s) "" loc x []
            in
                charEnd e s2 loc (x + i)
        c:s  -> charEnd c s  loc (x + 1)
        []   -> internalError0 "Lexer.lexChar: empty list."

  where
    charEnd c ('\'':s) loc x = cont (Character c) s loc (x + 1) state
    charEnd c s _ _          = Failed "improperly terminated character constant."


lexString :: (Token -> P a) -> P a
lexString cont s loc@(y',_) x state = 
    loop "" s x y'
        where
        loop e s x y =
            case s of
            '\\':'&':s  -> loop e s (x+2) y
            '\\':c:s | isSpace c -> stringGap e s (x + 2) y
                     | otherwise ->
                         let (e', sr, i) =
                               runPM (escapeChar (c:s)) ""  loc x [] 
                         in
                             loop (e':e) sr (x+i) y
            '\"':s{-"-} -> cont (StringTok (reverse e)) s  loc (x + 1) state
            c:s       -> loop (c:e) s (x + 1) y
            []          -> Failed "improperly terminated string."

        stringGap e s x y =
            case s of
                '\n':s -> stringGap e s 1 (y + 1)
                '\\':s -> loop e s (x + 1) y
                c:s' | isSpace c -> stringGap e s' (x + 1) y
                     | otherwise ->
                         Failed "illegal character in string gap."
                []     -> internalError0 "Lexer.stringGap: empty list."


escapeChar :: String -> PM (Char, String, Int)
escapeChar s = case s of
   'a':s         -> return ('\a', s, 2)
   'b':s         -> return ('\b', s, 2)
   'f':s         -> return ('\f', s, 2)
   'n':s         -> return ('\n', s, 2)
   'r':s         -> return ('\r', s, 2)
   't':s         -> return ('\t', s, 2)
   'v':s         -> return ('\v', s, 2)
   '\\':s        -> return ('\\', s, 2)
   '"':s         -> return ('\"', s, 2)      -- "
   '\'':s        -> return ('\'', s, 2)

   '^':x@(c:s)   -> cntrl x
   'N':'U':'L':s -> return ('\NUL', s, 4)
   'S':'O':'H':s -> return ('\SOH', s, 4)
   'S':'T':'X':s -> return ('\STX', s, 4)
   'E':'T':'X':s -> return ('\ETX', s, 4)
   'E':'O':'T':s -> return ('\EOT', s, 4)
   'E':'N':'Q':s -> return ('\ENQ', s, 4)
   'A':'C':'K':s -> return ('\ACK', s, 4)
   'B':'E':'L':s -> return ('\BEL', s, 4)
   'B':'S':s     -> return ('\BS',  s, 3)
   'H':'T':s     -> return ('\HT',  s, 3)
   'L':'F':s     -> return ('\LF',  s, 3)
   'V':'T':s     -> return ('\VT',  s, 3)
   'F':'F':s     -> return ('\FF',  s, 3)
   'C':'R':s     -> return ('\CR',  s, 3)
   'S':'O':s     -> return ('\SO',  s, 3)
   'S':'I':s     -> return ('\SI',  s, 3)
   'D':'L':'E':s -> return ('\DLE', s, 4)
   'D':'C':'1':s -> return ('\DC1', s, 4)
   'D':'C':'2':s -> return ('\DC2', s, 4)
   'D':'C':'3':s -> return ('\DC3', s, 4)
   'D':'C':'4':s -> return ('\DC4', s, 4)
   'N':'A':'K':s -> return ('\NAK', s, 4)
   'S':'Y':'N':s -> return ('\SYN', s, 4)
   'E':'T':'B':s -> return ('\ETB', s, 4)
   'C':'A':'N':s -> return ('\CAN', s, 4)
   'E':'M':s     -> return ('\EM',  s, 3)
   'S':'U':'B':s -> return ('\SUB', s, 4)
   'E':'S':'C':s -> return ('\ESC', s, 4)
   'F':'S':s     -> return ('\FS',  s, 3)
   'G':'S':s     -> return ('\GS',  s, 3)
   'R':'S':s     -> return ('\RS',  s, 3)
   'U':'S':s     -> return ('\US',  s, 3)
   'S':'P':s     -> return ('\SP',  s, 3)
   'D':'E':'L':s -> return ('\DEL', s, 4)


   -- Depending upon the compiler/interpreter's Char type, these yield either
   -- just 8-bit ISO-8859-1 or 2^16 UniCode.

   -- Octal representation of a character
   'o':s           -> let (ds, s') = span isOctDigit s
                          n        = readNumber 8 ds
                      in 
                          numberToChar n s' (length ds + 1)

   -- Hexadecimal representation of a character
   'x':s           -> let (ds, s') = span isHexDigit s
                          n        = readNumber 16 ds
                      in
                          numberToChar n s' (length ds + 1)
 
   -- Base 10 representation of a character
   d:s | isDigit d -> let (ds, s') = span isDigit s
                          n        = readNumber 10 (d:ds)
                      in 
                          numberToChar n s' (length ds + 1)

   _               -> fail $ "illegal escape sequence."

   where numberToChar n s l_n =
             if n < (toInteger $ fromEnum (minBound :: Char)) ||
                n > (toInteger $ fromEnum (maxBound :: Char)) then
                 fail $ "illegal character literal (number out of range)."
             else
                 return (chr $ fromInteger n, s, l_n)
            

cntrl :: String -> PM (Char, String, Int)
cntrl (c   :s) | isUpper c = return (chr (ord c - ord 'A'), s, 2)
cntrl ('@' :s)             = return ('\^@', s, 2)
cntrl ('[' :s)             = return ('\^[', s, 2)
cntrl ('\\':s)             = return ('\^\', s, 2)
cntrl (']' :s)             = return ('\^]', s, 2)
cntrl ('^' :s)             = return ('\^^', s, 2)
cntrl ('_' :s)             = return ('\^_', s, 2)
cntrl _                    = fail "illegal control character"


nestedComment cont y x bol s col state =
    case s of
    '-':'}':s -> cont y (x + 2) bol s col state
    '{':'-':s -> nestedComment (nestedComment cont) y (x + 2) bol s col state
    '\t':s    -> nestedComment cont y (nextTab x) bol s col state
    '\n':s    -> nestedComment cont (y + 1) 1 True s col state
    c:s       -> nestedComment cont y (x + 1) bol s col state
    []        -> Failed "Open comment at end of file"



readInteger :: String -> Integer
readInteger ('0':'o':ds) = readInteger2  8 isOctDigit ds
readInteger ('0':'O':ds) = readInteger2  8 isOctDigit ds
readInteger ('0':'x':ds) = readInteger2 16 isHexDigit ds
readInteger ('0':'X':ds) = readInteger2 16 isHexDigit ds
readInteger          ds  = readInteger2 10 isDigit    ds

readNumber :: Integer -> String -> Integer
readNumber radix ds = readInteger2 radix (const True) ds

readInteger2 :: Integer -> (Char -> Bool) -> String -> Integer
readInteger2 radix isDig ds 
  = foldl1 (\n d -> n * radix + d) (map (fromIntegral . digitToInt) 
                                    (takeWhile isDig ds))

readRational :: String -> Rational
readRational xs
    = (readInteger (i ++ m))%1 * 10^^((case e of
                                       ""       -> 0
                                       ('+':e2) -> read e2
                                       _        -> read e) - length m)
      where (i, r1) = span isDigit xs
            (m, r2) = span isDigit (dropWhile (== '.') r1)
            e       = dropWhile (== 'e') r2

