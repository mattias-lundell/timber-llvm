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

{

module Parser (parser) where

import Common
import Token
import Lexer
import ParseMonad
import Syntax
import Fixity



}

%token
        VARID 	 { VarId $$ }
	CONID	 { ConId $$ }
        '-'	 { VarSym ("","-") }
        '<'      { VarSym ("","<") }
        '>'      { VarSym ("",">") }
        '*'	 { VarSym ("","*") }
	VARSYM	 { VarSym $$ }
	CONSYM	 { ConSym $$ }
	INT	 { IntTok $$ }
	RATIONAL { FloatTok $$ }
 	CHAR	 { Character $$ }
	STRING   { StringTok $$ }
{-

Symbols
-}
	'('	{ LeftParen }
	')'	{ RightParen }
	';'	{ SemiColon }
	'{'	{ LeftCurly }
	'}'	{ RightCurly }
	vccurly { VRightCurly }	      -- a virtual close brace
	'['	{ LeftSquare }
	']'	{ RightSquare }
  	','	{ Comma }
	'`'	{ BackQuote }
        '_'     { Wildcard }

{-

Reserved operators

-}
        '.'     { Dot }
	'..'	{ DotDot }
	'::'	{ DoubleColon }
        ':='    { Assign }
	'='	{ Equals }
	'\\'	{ Backslash }
	'|'	{ Bar }
	'<-'	{ LeftArrow }
	'->'	{ RightArrow }
        '\\\\'  { Backslash2 }
{-

Reserved Ids

-}
        'action'        { KW_Action }
        'after'         { KW_After }
        'before'        { KW_Before }
        'case'		{ KW_Case }
        'class'		{ KW_Class }
        'data'		{ KW_Data }
        'default'       { KW_Default }
        'do'		{ KW_Do }
        'else'		{ KW_Else }
        'elsif'		{ KW_Elsif }
        'extern'        { KW_Extern }
        'forall'	{ KW_Forall }
        'if'		{ KW_If }
        'import'        { KW_Import }
        'instance'      { KW_Instance }
        'in'		{ KW_In }
        'let'		{ KW_Let }
        'module'	{ KW_Module }
        'new'           { KW_New }
        'of'		{ KW_Of }
        'private'       { KW_Private }
        'request'       { KW_Request }
        'result'        { KW_Result }
        'struct'        { KW_Struct }
        'then'		{ KW_Then }
        'type'		{ KW_Type }
        'typeclass'	{ KW_Typeclass }
        'use'           { KW_Use }
        'where'		{ KW_Where }

%monad { PM } { thenPM } { returnPM }
%lexer { lexer } { EOF }
%name parse
%tokentype { Token }
%%

-- Module Header ------------------------------------------------------------

module  :: { Module }
        : 'module' conid 'where' body		{ mkModule $2 $4 }

body    :: { ([Import],[Decl],[Decl]) }
        : '{' layout_off imports topdecls '}' private	{ (reverse $3,reverse $4, $6) }
        |     layout_on  imports topdecls close	private { (reverse $2, reverse $3, $5) }

private :: { [Decl] }
        : 'private' pbody			{ $2 }
        | {- empty -}			        { [] }

pbody    :: { [Decl] }
        : '{' layout_off topdecls '}'	        { reverse $3 }
        |     layout_on  topdecls close	        { reverse $2 }

imports :: { [Import] }
        : imports import ';'                    { $2 : $1 }
        | {- empty -}                           { [] }

import  :: { Import }
        : 'import' conid                        { Import True (modId $2) }
        | 'use' conid                           { Import False (modId $2) }


-- Top-level declarations ---------------------------------------------------

topdecls :: { [Decl] }
        : topdecls ';' topdecl		        { $3 ++ $1 }
        | topdecl				{ $1 }

topdecl :: { [Decl] }
        : conid '::' kind		        	   { [DKSig $1 $3] }
        | 'type' conid tyvars '=' type	                   { [DType $2 (reverse $3) $5] }
        | 'data' conid tyvars optsubs optcs                { [DData $2 (reverse $3) $4 $5] }
        | 'struct' conid tyvars optsups optsigs            { [DRec False $2 (reverse $3) $4 $5] }
        | 'typeclass' conid tyvars sups optsigs            { [DRec True $2 (reverse $3) $4 $5] }
        | 'typeclass' conid tyvars 'where' siglist         { [DRec True $2 (reverse $3) [] $5] }
        | 'typeclass' ids                                  { [DTClass $2] }
        | 'instance' var '::' type                         { [DPSig $2 $4] }  
        | 'instance' var '::' type 'where' bindlist        { [DBind [BEqn (var2lhs $2) 
                                                                          (RExp (EBStruct (Just (type2head $4)) [] $6))],
                                                              DPSig $2 $4] }
        | 'instance' var '::' type rhs                     { [DBind [BEqn (var2lhs $2) $5], DPSig $2 $4] }
        | 'instance' ids                                   { [DInstance $2] }
        | 'default' def                                    { [DDefault (reverse $2)] }
        | 'extern' ext                                     { [DExtern (reverse $2)] }
        | vars '::' type		                   { [DBind [BSig (reverse $1) $3]] }
        | lhs rhs 					   { [DBind [BEqn $1 $2]] }

sups :: { [Type] }
        : '<' types				{ reverse $2 }
        | '<' type				{ [$2] }

optsups :: { [Type] }
        : sups                                  { $1 }
        | {- empty -}				{ [] }

optsubs :: { [Type] }
        : '>' types				{ reverse $2 }
        | '>' type				{ [$2] }
        | {- empty -}				{ [] }

tyvars  :: { [Name] }
        : tyvars varid				{ $2 : $1 }
        | {- empty -}				{ [] }



ids     :: { [Name] }
        : ids ',' id                            { $3 : $1 }
        | id                                    { [$1] }

-- Default declarations ---------------------------------------------------

def     :: { [Default Type] }
def     : '{' layout_off prefs '}'              { $3 }
        | layout_on prefs close                 { $2 }
      
prefs   ::  { [Default Type] }
        : prefs ';' pref                        { $3 : $1 }
        | pref                                  { [$1] }
 

pref     :: { Default Type }
        : var '<' var                           { Default True $1 $3 }
        | var '::' type                         { Derive $1 $3 }  


-- External declarations -----------------------------------------------------

ext :: { [Extern Type] }
ext     : '{' layout_off exts '}'              { $3 }
        | layout_on exts close                 { $2 }

exts   ::  { [Extern Type] }
        : exts ';' extt                        { $3 : $1 }
        | extt                                 { [$1] }
 

extt    :: { Extern Type }
        : var '::' type                         { Extern $1 $3 }  


-- Datatype declarations ---------------------------------------------------

optcs   :: { [Constr] }
        : '=' constrs				{ reverse $2 }
        | {- empty -}				{ [] }
        
constrs :: { [Constr] }
	: constrs '|' constr	                { $3 : $1 }
	| constr			        { [$1] }

constr  :: { Constr }
        : type consym type                      { Constr $2 [$1, $3] [] }
        | type                                  { type2cons $1 }

-- Signatures --------------------------------------------------------------

optsigs :: { [Sig] }
        : 'where' siglist	      		{ $2 }
        | {- empty -}				{ [] }
        
siglist :: { [Sig] }
        : '{' layout_off sigs '}'		{ reverse $3 }
        |     layout_on  sigs close		{ reverse $2 }

sigs	:: { [Sig] }
	: sigs1					{ $1 }
	| {- empty -}				{ [] }
	
sigs1	:: { [Sig] }
        : sigs1 ';' sig				{ $3 : $1 }
        | sig					{ [$1] }

sig	:: { Sig }
        : vars '::' type	                { Sig (reverse $1) $3 }


-- Bindings ----------------------------------------------------------------

bindlist :: { [Bind] }
	: '{' layout_off binds '}'		{ reverse $3 }
	|     layout_on  binds close		{ reverse $2 }

binds   :: { [Bind] }
        : binds ';' bind			{ $3 : $1 }
        | bind					{ [$1] }
        
bind    :: { Bind }
	: vars '::' type	                { BSig (reverse $1) $3 }
        | lhs rhs 				{ BEqn $1 $2 }


recbinds   :: { [EField] }
        : recbinds ',' recbind			{ $3 : $1 }
        | recbind				{ [$1] }
        
recbind    :: { EField }
        : var '=' exp 				{ Field $1  $3 }

vars	:: { [Name] }
	: vars ',' var				{ $3 : $1 }
        | var					{ [$1] }

lhs     :: { Lhs }
        : exp0s                                 { exp2lhs $1 }

rhs	:: { Rhs Exp }
	: '=' exp		    		{ RExp $2 }
	| gdrhss		    		{ RGrd (reverse $1) }
        | rhs 'where' bindlist			{ RWhere $1 $3 }

gdrhss  :: { [GExp Exp] }
	: gdrhss gdrhs		    		{ $2 : $1 }
	| gdrhs			    		{ [$1] }

gdrhs   :: { GExp Exp }
        : '|' quals '=' exp    			{ GExp (reverse $2) $4 }


-- Types ---------------------------------------------------------------------

type    :: { Type }
        : ftype '\\\\' preds                    { TQual $1 $3 }
        | ftype                                 { $1 }

ftype   :: { Type }
        : btypes                                { tFun (reverse (tail $1)) (head $1) }
        | btype '<' btype                       { TSub $1 $3}

btypes  :: { [Type] }
        : btypes '->' btype                     { $3 : $1 }
        | btype                                 { [$1] }

btype   :: { Type }
        : btype  atype                          { TAp $1 $2 }
        | atype                                 { $1 }

atype   :: { Type }
	: con					{ TCon $1 }
	| varid					{ TVar $1 }
        | '_'                                   { TWild }
        | '[' ']'				{ TCon (prim LIST) }
--	| '(' '->' ')'	                	{ TCon (prim ARROW) }
	| '(' commas ')'			{ TCon (tuple ($2+1)) }
        | '(' ')'				{ TCon (tuple 0) }
        | '(' type ')'                          { $2 }
	| '(' types ')'				{ TTup (reverse $2) }
	| '[' type ']'				{ TList $2 }

types   :: { [Type] }
	: types ',' ftype			{ $3 : $1 }
	| ftype  ',' ftype			{ [$3, $1] }

commas  :: { Int }
	: commas ','		     		{ $1 + 1 }
	| ','			     		{ 1 }

-- Predicates -----------------------------------------------------------------

preds	:: { [Pred] }
        : preds ',' pred			{ $3 : $1 }
        | pred					{ [$1] }
        
pred	:: { Pred }
        : ftype                                 { PType $1 }
        | varid '::' kind			{ PKind $1 $3 }

kind	:: { Kind }
        : kind1 '->' kind			{ KFun $1 $3 }
        | kind1					{ $1 }

kind1	:: { Kind }
        : '*'					{ Star }
        | '_'                                   { KWild }
        | '(' kind ')'				{ $2 }
        

-- Expressions -------------------------------------------------------------

exp     :: { Exp }
        : exp0a '::' btype                      { ESig $1 $3 }
        | exp0                                  { $1}
        | 'struct' bindlist                     { EBStruct Nothing [] $2 }

exp0    :: { Exp }
        : exp0a                                 { $1 }
        | exp0b                                 { $1 }

exp0a	:: { Exp }
        : opExpa			        { transFix $1 }
	| exp10a			        { $1 }
	  
exp0b	:: { Exp }
        : opExpb			        { transFix $1 }
	| exp10b			        { $1 }
	  
opExpa  :: { OpExp }
        : opExpa op '-' exp10a	                { Cons $1 $2 (ENeg $4) }
        | opExpa op exp10a	                { Cons $1 $2 $3 }
        | '-' exp10a			        { Nil (ENeg $2) }
        | exp10a op '-' exp10a	                { Cons (Nil $1) $2 (ENeg $4) }
        | exp10a op exp10a		        { Cons (Nil $1) $2 $3 }
	  
opExpb  :: { OpExp }
        : opExpa op '-' exp10b	                { Cons $1 $2 (ENeg $4) }
	| opExpa op exp10b	                { Cons $1 $2 $3 }
	| '-' exp10b		                { Nil (ENeg $2) }
	| exp10a op '-' exp10b	                { Cons (Nil $1) $2 (ENeg $4) }
	| exp10a op exp10b	                { Cons (Nil $1) $2 $3 }
	  
exp10a  :: { Exp }
        : 'case' exp 'of' altslist              { ECase $2 $4 }
        | '{'  layout_off recbinds '}'          { ERec Nothing (reverse $3) }
	| '{'  layout_off  '}'			{ ERec Nothing [] }
        | exp10as                               { $1 }

exp10as :: { Exp }
        : loc 'do' stmtlist                     { EDo Nothing Nothing (checkStmts $3) }
        | loc 'class' stmtlist                  { ETempl Nothing Nothing (checkClass $3) }
        | loc 'action' stmtlist                 { EAct Nothing (checkStmts $3) }
        | loc 'request' stmtlist                { EReq Nothing (checkStmts $3) }
        | 'forall' quals 'do' stmtlist          { EForall (reverse $2) (checkStmts $4) }
        | 'forall' quals 'class' stmtlist       { forallClass (reverse $2) (ETempl Nothing Nothing (checkClass $4)) }
        | con '{'  layout_off recbinds '}'      { ERec (Just ($1,True)) (reverse $4) } 
        | con '{'  layout_off recbinds '..' '}' { ERec (Just ($1,False)) (reverse $4) } 
        | con '{'  layout_off recbinds ',' '..' '}' { ERec (Just ($1,False)) (reverse $4) } 
        | con '{'  layout_off '..' '}'          { ERec (Just ($1,False)) [] } 
	| con '{'  layout_off  '}'		{ ERec (Just ($1,True)) [] }
        | fexp                                  { $1 }

exp10b :: { Exp }
        : 'if' exp 'then' exp 'else' exp        { EIf $2 $4 $6 }
        | exp10bs                               { $1 }

exp10bs :: { Exp }
        : '\\' apats '->' exp                   { ELam (reverse $2) $4 }
        | 'let' bindlist 'in' exp               { ELet $2 $4 }
        | 'after' aexp exp                      { EAfter $2 $3 }
        | 'before' aexp exp                     { EBefore $2 $3 }

fexp    :: { Exp }
        : fexp aexp                             { EAp $1 $2 }
        | aexp                                  { $1 }

aexp    :: { Exp }
        : aexp '.' var                          { ESelect $1 $3 }
        | bexp                                  { $1 }

bexp    :: { Exp }
        : var                                   { EVar $1 }
        | '_'                                   { EWild }
        | con                                   { ECon $1 }
        | lit                                   { ELit $1 }
        | '(' '.' var ')'                       { ESel $3 }
        | '(' exp ')'                           { $2 }
        | '(' exps ')'                          { ETup (reverse $2) } 
        | '[' list ']'                          { $2 }
        | '(' exp10a op ')'                     { ESectR $2 $3 }
        | '(' op0 fexp ')'                      { ESectL $2 $3 }
        | '(' commas ')'                        { ECon (tuple ($2+1)) }
        | '(' ')'                               { ECon (tuple 0) }

lit     :: { Lit }
: loc INT                                   { LInt (Just $1) (readInteger $2) }
| loc RATIONAL                              { LRat (Just $1) (readRational $2) }
| loc CHAR                                  { LChr (Just $1) $2 }
| loc STRING                                { LStr (Just $1) $2 }

-- List expressions -------------------------------------------------------------

list    :: { Exp }
        : {- empty -}                           { EList [] }
        | exp                                   { EList [$1] }
        | exps                                  { EList (reverse $1) }
        | exp '..' exp                          { ESeq $1 Nothing $3 }
        | exp ',' exp '..' exp                  { ESeq $1 (Just $3) $5 }
        | exp '|' quals                         { EComp $1 (reverse $3) }

exps    :: { [Exp] }
        : exps ',' exp                          { $3 : $1 }
        | exp ',' exp                           { [$3,$1] }
 

-- List comprehensions ---------------------------------------------------------

quals   :: { [Qual] }
        : quals ',' qual                        { $3 : $1 }
        | qual                                  { [$1] }

qual    :: { Qual }
        : pat '<-' exp0s                        { QGen $1 $3 }
        | exp0s                                 { QExp $1 }
        | 'let' bindlist                        { QLet $2 }


-- Case alternatives ------------------------------------------------------------

altslist :: { [Alt Exp] }
        : '{' layout_off alts '}'               { reverse $3 }
        |     layout_on  alts close             { reverse $2 }


alts    :: { [Alt Exp] }
        : alts ';' alt                          { $3 : $1 }
        | alt                                   { [$1] }

alt     :: { Alt Exp }
        : pat rhscasealts                       { Alt $1 $2 }

rhscasealts :: { Rhs Exp }
        : '->' exp                              { RExp $2 }
        | gdcaserhss                            { RGrd (reverse $1) }
        | rhscasealts 'where' bindlist          { RWhere $1 $3 }

gdcaserhss :: { [GExp Exp] }
        : gdcaserhss gdcaserhs                  { $2 : $1 }
        | gdcaserhs                             { [$1] }

gdcaserhs :: { GExp Exp }
        : '|' quals  '->' exp                   { GExp (reverse $2) $4 }


-- Case statement alternatives ------------------------------------------------------------

saltslist :: { [Alt Stmts] }
        : '{' layout_off salts  '}'             { reverse $3 }
        |     layout_on  salts close            { reverse $2 }


salts   :: { [Alt Stmts] }
        : salts ';' salt                        { $3 : $1 }
        | salt                                  { [$1] }

salt    :: { Alt Stmts }
        : pat srhscasealts                      { Alt $1 $2 }

srhscasealts :: { Rhs Stmts }
        : '->' stmtlist                         { RExp $2 }
        | sgdcaserhss                           { RGrd (reverse $1) }
        | srhscasealts 'where' bindlist         { RWhere $1 $3 }

sgdcaserhss :: { [GExp Stmts] }
        : sgdcaserhss sgdcaserhs                { $2 : $1 }
        | sgdcaserhs                            { [$1] }

sgdcaserhs :: { GExp Stmts }
        : '|' quals  '->' stmtlist              { GExp (reverse $2) $4 }


-- Statement sequences -----------------------------------------------------------

stmtlist :: { Stmts }
        : '{' layout_off stmts '}'              { Stmts $3 }
        | layout_on stmts close                 { Stmts $2 }

stmts   :: { [Stmt] }
        : stmt ';' stmts                        { $1 : $3 }
	| stmt					{ [$1] }
	| {- empty -}				{ [] }

stmt    :: { Stmt }
        : pat '<-' exp                          { SGen $1 $3 }
        | mexp                                  { SExp $1 }
        | vars '::' type                        { SBind [BSig $1 $3] }
        | lhs rhs                               { SBind [BEqn $1 $2] }
        | lhs '=' 'new' exp                     { SBind [BEqn $1 (RExp (EAp (EVar (prim New)) $4))] }
        | lhs '=' 'forall' quals 'new' exp10as  { SBind [BEqn $1 (RExp (EAp (EVar (prim New)) (forallClass $4 $6)))] }
        | pat ':=' exp                          { SAss $1 $3 }
        | 'result' exp                          { SRet $2 }
        | 'if' exp 'then' stmtlist              { SIf $2 $4 }
        | 'elsif' exp 'then' stmtlist           { SElsif $2 $4 }
        | 'else' stmtlist                       { SElse $2 }
        | 'case' exp 'of' saltslist             { SCase $2 $4 }
     

mexp    :: { Exp }
--      : exp0as '::' type                      { ESig $1 $3 }
        : exp0s                                 { $1}

exp0s    :: { Exp }
        : exp0as                                { $1 }
        | exp0bs                                { $1 }

exp0as	:: { Exp }
        : opExpas			        { transFix $1 }
	| exp10as			        { $1 }
	  
exp0bs	:: { Exp }
        : opExpbs			        { transFix $1 }
	| exp10bs			        { $1 }
	  
opExpas  :: { OpExp }
        : opExpas op '-' exp10as                { Cons $1 $2 (ENeg $4) }
        | opExpas op exp10as	                { Cons $1 $2 $3 }
        | '-' exp10as			        { Nil (ENeg $2) }
        | exp10as op '-' exp10as                { Cons (Nil $1) $2 (ENeg $4) }
        | exp10as op exp10as		        { Cons (Nil $1) $2 $3 }
	  
opExpbs  :: { OpExp }
        : opExpas op '-' exp10bs                { Cons $1 $2 (ENeg $4) }
	| opExpas op exp10bs	                { Cons $1 $2 $3 }
	| '-' exp10bs		                { Nil (ENeg $2) }
	| exp10as op '-' exp10bs                { Cons (Nil $1) $2 (ENeg $4) }
	| exp10as op exp10bs	                { Cons (Nil $1) $2 $3 }



-- Patterns ----------------------------------------------------------------

pat     :: { Pat }
        : exp0s                                 { exp2pat $1 }

apats   :: { [Pat] }
        : apats apat                            { $2 : $1 }
        | apat                                  { [$1] }

apat    :: { Pat }
        : aexp                                  { exp2pat $1 }


-- Variables, Constructors and Operators ------------------------------------

var     :: { Name }
        : varid                                 { $1 }
        | '(' varsym ')'                        { $2 }

con     :: { Name }
        : conid                                 { $1 }
        | '(' consym ')'                        { $2 }

varop   :: { Name }
        : varsym                                { $1 }
        | '`' varid '`'                         { $2 }

conop   :: { Name }
        : consym                                { $1 }
        | '`' conid '`'                         { $2 }

op      :: { Name }
        : varop                                 { $1 }
        | conop                                 { $1 }

id      :: { Name }
        : varid                                 { $1 }
        | conid                                 { $1 }

op0     :: { Name }
        : VARSYM0                               {% do l <- getSrcLoc; return (name l $1) }
        | '`' varid '`'                         { $2 }
        | conop                                 { $1 }


varid   :: { Name }
        : loc VARID                             { name $1 $2 }

conid   :: { Name }
        : loc CONID                             { name $1 $2 }

varsym  :: { Name }
        : VARSYM1                               {% do l <- getSrcLoc; return (name l $1) }

consym  :: { Name }
        : loc CONSYM                            { name $1 $2 }



VARSYM1 :: { (String,String) }
	: VARSYM0				{ $1 }
	| '-'					{ ("","-") }

VARSYM0 :: { (String,String) }
        : VARSYM                                { $1 }
        | '<'                                   { ("","<") }
        | '>'                                   { ("",">") }
        | '*'                                   { ("","*") }
        | '\\\\'				{ ("","\\\\") }
        

-- Layout ---------------------------------------------------------------------

close :: { () }
        : vccurly                               { () } -- context popped in lexer.
        | error                                 {% popContext }

layout_off :: { () }    :                       {% pushContext NoLayout }

layout_on  :: { () }    :                       {% do { (r,c) <- getSrcLoc;
                                                        pushContext (Layout c)
                                                      }
                                                }

loc     :: { (Int,Int) }
         : {- empty -}                          {% getSrcLoc }


-- Error -----------------------------------------------------------------------

{
parser     :: String -> M s Module
parser str = runPM2 parse str

happyError = parseError "parse error"
}
