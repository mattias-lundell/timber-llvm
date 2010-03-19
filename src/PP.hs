{-# LANGUAGE  FlexibleInstances, TypeSynonymInstances #-}

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

module PP (module PP, module Text.PrettyPrint) where

import Text.PrettyPrint hiding (TextDetails(..))
import Char(showLitChar)


class Pr a where
    pr   :: a -> Doc
    pr x = prn 0 x

    prn  :: Int -> a -> Doc
    prn n x = pr x

    vpr  :: [a] -> Doc
    vpr xs = vcat (map pr xs)

    hpr  :: Char -> [a] -> Doc
    hpr c xs = sep (punctuate (char c) (map pr xs))

    dump :: a -> IO ()
    dump x = putStr (render (pr x))


instance Pr Int where 
  pr = int

-- XXX Is this correct?
-- AJG It is needed by Main, but it looks wrong.

instance Pr (String, String) where
    pr (a, b) = text a <> text b

-- used by llvm backend
instance Pr String where
    pr a = text a

infixl 4 $$$
a $$$ b       = a $$ text " " $$ b

vcat2 xs      = vcat (map ($$ text " ") xs)

backQuotes p  = char '`'  <> p <> char '`'
litChar       = charQuotes . text . lit
litString     = doubleQuotes . text . concat . map lit
lit c         = showLitChar c ""
charQuotes p  = char '\'' <> p <> char '\''
curlies       = braces

commasep f xs = sep (punctuate comma (map f xs))


show' :: Pr a => a -> String
show' = render . pr

vshow :: Pr a => [a] -> String
vshow = render . vpr

showlist :: Pr a => [a] -> String
showlist xs = render (text "(" <> hpr ',' xs <> text ")")

