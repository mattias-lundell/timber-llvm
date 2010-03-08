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

module ParseMonad where

import Common

data ParseResult a
    = Ok ParseState a
    | Failed String
      deriving Show

type ParseState = [LexContext]

data LexContext
    = NoLayout
    | Layout Int
    | RecLayout Int
      deriving (Eq, Ord, Show)

newtype PM a 
    = PM (   String		    -- input string
          -> (Int,Int)		-- location of last token read (row,col)
	      -> Int		    -- current column
	      -> ParseState   	-- layout info
	      -> ParseResult a)

unPM (PM p) = p

instance Monad PM where
    (>>=)	= thenPM
    return 	= returnPM
    fail  	= failPM

PM m `thenPM` k	= PM $ \i l c s -> 
                    case m i l c s of 
                        Failed s -> Failed s
                        Ok s' a  -> case k a of PM k' -> k' i l c s'
                        
returnPM a	= PM $ \i l c s -> Ok s a
failPM a 	= PM $ \i l c s -> Failed a

runPM (PM p) i l c s =
    case p i l c s of
        Ok _ a -> a
	Failed err -> compileError err

runPM2 (PM p) input =
    case p input (1,1) 0 [] of
        Ok _ result -> return result
        Failed msg  -> fail msg

getSrcLoc :: PM (Int,Int)
getSrcLoc = PM $ \i l c s -> Ok s l

getCurrent :: PM (String,(Int,Int),Int,ParseState)
getCurrent = PM $ \i l c s -> Ok s (i,l,c,s)

pushContext :: LexContext -> PM ()
pushContext ctxt = PM $ \i l c s -> Ok (ctxt:s) ()

popContext :: PM ()
popContext = PM $ \i loc c stk ->
    case stk of
    (_:s) -> Ok s ()
    []    -> Failed $ show loc ++
                      ": parse error (possibly incorrect indentation)"


parseError :: String -> PM a
parseError err =
    PM $ \r (l,c) -> (unPM $ fail $ "Syntax error at line "++show l++", column "++show c++ "\n") r (l,c)
