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

module Fixity where

import Common
import Syntax
import List(sort)

type Precedence         = Int
data Associativity      = LeftAss | RightAss | NonAss deriving (Eq,Show)

data Fixity             = Fixity Associativity Precedence deriving (Eq,Show)

data OpExp              = Nil Exp | Cons OpExp Name Exp 

fixity :: String -> Fixity
fixity op               = case lookup op fixTable of
                            Just f  -> f
                            Nothing -> fixFromChars op
  where fixTable        = [(":",  Fixity RightAss 5),
                           ("++", Fixity RightAss 5),
                           ("+",  Fixity LeftAss  6),
                           ("-",  Fixity LeftAss  6),
                           ("*",  Fixity LeftAss  7),
                           ("/",  Fixity LeftAss  7),
                           ("div",Fixity LeftAss  7),
                           ("mod",Fixity LeftAss  7),
                           ("@",  Fixity RightAss 9),
                           ("^",  Fixity RightAss 8),
                           ("==", Fixity NonAss   4),
                           ("/=", Fixity NonAss   4),
                           ("<",  Fixity NonAss   4),
                           ("<=", Fixity NonAss   4),
                           (">",  Fixity NonAss   4),
                           (">=", Fixity NonAss   4),
                           ("&&", Fixity RightAss 3),
                           ("||", Fixity RightAss 2),
                           (">>", Fixity LeftAss  1),
                           (">>=",Fixity LeftAss  1),
                           ("$",  Fixity RightAss 0)
                          ]
        fixFromChars op = case sort (nub (intersect op "+-*/<>")) of
                            "+"  -> Fixity LeftAss 6
                            "-"  -> Fixity LeftAss 6
                            "+-" -> Fixity LeftAss 6
                            "*"  -> Fixity LeftAss 7
                            "/"  -> Fixity LeftAss 7
                            "*/" -> Fixity LeftAss 7
                            "<"  -> Fixity NonAss  4
                            ">"  -> Fixity NonAss  4
                            "<>" -> Fixity NonAss  4
                            _    -> Fixity LeftAss 9

{-
Transforms a tree of infix expressions as produced by the parser 
(i.e., with all operators treated as left associative and of equal precedence) 
to a new tree reflecting operator associativity and precedence as given by
the function fixity.

Invariant: at each call to push, the second and third arguments have
the same length.
-}

transFix :: OpExp -> Exp
transFix e                          = push e [] []
  where push (Cons l o r) (o':os) es
          | prec==prec' && (ass/=ass' || ass==NonAss)
                                    = errorIds "Operator associativity ambiguity with operators" [o,o']
          | prec<prec' || (prec==prec' && ass==RightAss)
                                    = push (Cons l o (opApp r o' (head es))) os (tail es)
          where Fixity ass  prec    = fixity (show o)
                Fixity ass' prec'   = fixity (show o')
        push (Cons l o r) os es     = push l (o:os) (r:es)
        push (Nil e) os es          = popAll os (e:es)
        opApp l o r                 = EAp (EAp (op2exp o) l) r
         
        popAll (o:os) (e1:e2:es)    = popAll os (opApp e1 o e2:es)
        popAll [] es                = head es
        