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

module Token where

import Char


data Token 
    = VarId      (String,String)
    | ConId      (String,String)
    | VarSym     (String,String)
    | ConSym     (String,String)
    | IntTok     String
    | FloatTok   String
    | Character  Char
    | StringTok  String
{-

Symbols

-}
    | LeftParen
    | RightParen
    | SemiColon
    | LeftCurly
    | RightCurly
    | VRightCurly                       -- a virtual close brace
    | LeftSquare
    | RightSquare
    | Comma
    | BackQuote
{-

Reserved operators

-}
    | Assign
    | Dot
    | DotDot
    | DoubleColon
    | Equals
    | Backslash
    | Bar
    | LeftArrow
    | RightArrow
    | Tilde
    | Wildcard
    | Backslash2
{-

Reserved Ids

-}
    | KW_Action
    | KW_After
    | KW_Before
    | KW_Case     
    | KW_Class    
    | KW_Data     
    | KW_Default  
    | KW_Do       
    | KW_Else   
    | KW_Elsif
    | KW_Extern
    | KW_Forall
    | KW_If
    | KW_Import   
    | KW_Instance
    | KW_In       
    | KW_Let
    | KW_Module
    | KW_New   
    | KW_Of    
    | KW_Private   
    | KW_Request
    | KW_Result
    | KW_Struct
    | KW_Then     
    | KW_Type  
    | KW_Typeclass
    | KW_Use   
    | KW_Where
    | EOF
      deriving (Eq, Show)


reserved_ops :: [(String, Token)]
reserved_ops
    = [
        ( ".",  Dot ),    
        ( "..", DotDot ),    
        ( "::", DoubleColon ),
        ( ":=", Assign ),
        ( "=",  Equals ),    
        ( "\\", Backslash ), 
        ( "|",  Bar ),       
        ( "<-", LeftArrow ), 
        ( "->", RightArrow ),
        ( "_",  Wildcard ),
        ( "\\\\", Backslash2 )
      ]


reserved_ids :: [(String, Token)]
reserved_ids
    = [
        ( "action",    KW_Action ),
        ( "after",     KW_After ),
        ( "before",    KW_Before ),
        ( "case",      KW_Case ),     
        ( "class",     KW_Class ),    
        ( "data",      KW_Data ),
        ( "default",   KW_Default),   
        ( "do",        KW_Do ),       
        ( "else",      KW_Else ),
        ( "elsif",     KW_Elsif ),
        ( "extern",    KW_Extern ),     
        ( "forall",    KW_Forall ),
        ( "if",        KW_If ),
        ( "import",    KW_Import ),
        ( "instance",  KW_Instance ),     
        ( "in",        KW_In ),       
        ( "let",       KW_Let ),      
        ( "module",    KW_Module ),   
        ( "new",       KW_New ),
        ( "of",        KW_Of ),
        ( "private",   KW_Private ),
        ( "request",   KW_Request ),
        ( "result",    KW_Result ),
        ( "struct",    KW_Struct ),
        ( "then",      KW_Then ),     
        ( "type",      KW_Type ),  
        ( "typeclass", KW_Typeclass ),  
        ( "use",       KW_Use ),   
        ( "where",     KW_Where )
      ]



tab_length = 8 :: Int

isIdent  c = isAlpha c || isDigit c || c == '\'' || c == '_'
isSymbol c = elem c ":!#$%&*+./<=>?@\\^|-~"

data LexInt =
      Decimal     (String,String)
    | Octal       (String,String)
    | Hexadecimal (String,String)
