// The Timber compiler <timber-lang.org>
// 
// Copyright 2008-2009 Johan Nordlander <nordland@csee.ltu.se>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
// 
// 3. Neither the names of the copyright holder and any identified
//    contributors, nor the names of their affiliations, may be used to 
//    endorse or promote products derived from this software without 
//    specific prior written permission.
// 
// THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
// OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
// WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
// ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
// STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
// ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.

#ifndef TIMBER_H_
#define TIMBER_H_
#include <math.h>

struct TUP2;
typedef struct TUP2 *TUP2;
struct TUP3;
typedef struct TUP3 *TUP3;
struct TUP4;
typedef struct TUP4 *TUP4;
struct TUPLE;
typedef struct TUPLE *TUPLE;

struct CLOS1;
typedef struct CLOS1 *CLOS1;
struct CLOS2;
typedef struct CLOS2 *CLOS2;
struct CLOS3;
typedef struct CLOS3 *CLOS3;
struct CLOS;
typedef struct CLOS *CLOS;

struct LIST;
typedef struct LIST *LIST;
struct NIL;
typedef struct NIL *NIL;
struct CONS;
typedef struct CONS *CONS;
struct EITHER;
typedef struct EITHER *EITHER;
struct LEFT;
typedef struct LEFT *LEFT;
struct RIGHT;
typedef struct RIGHT *RIGHT;

struct World;
typedef struct World *World;

struct TUP2 {
    WORD *GCINFO;
    POLY a;
    POLY b;
};
extern WORD __GC__TUP2[];

struct TUP3 {
    WORD *GCINFO;
    POLY a;
    POLY b;
    POLY c;
};
extern WORD __GC__TUP3[];

struct TUP4 {
    WORD *GCINFO;
    POLY a;
    POLY b;
    POLY c;
    POLY d;
};
extern WORD __GC__TUP4[];

struct TUPLE {
    WORD *GCINFO;
    WORD size;
    POLY elems[];
};
extern WORD __GC__TUPLE[];

struct CLOS1 {
    POLY GCINFO;
    POLY (*Code) (CLOS1, POLY);
};
extern WORD __GC__CLOS1[];

struct CLOS2 {
    POLY GCINFO;
    POLY (*Code) (CLOS2, POLY, POLY);
};
extern WORD __GC__CLOS2[];
 
struct CLOS3 {
    POLY GCINFO;
    POLY (*Code) (CLOS3, POLY, POLY, POLY);
};
extern WORD __GC__CLOS3[];

struct CLOS {
    POLY GCINFO;
    void (*Code) (void);
};
extern WORD __GC__CLOS[];

struct LIST {
    WORD *GCINFO;
};

struct CONS {
  WORD *GCINFO;
  POLY a;
  LIST b;
};
extern WORD __GC__CONS[];

struct EITHER {
  WORD *GCINFO;
  Int Tag;
};

extern WORD __GC__EITHER[];
struct LEFT {
  WORD *GCINFO;
  Int Tag;
  POLY a;
};
extern WORD __GC__LEFT[];

struct RIGHT {
  WORD *GCINFO;
  Int Tag;
  POLY a;
};
extern WORD __GC__RIGHT[];

struct Msg {
  WORD *GCINFO;
  Int (*Code)(Msg);
  AbsTime baseline;
  AbsTime deadline;
  Msg next;
};
extern WORD __GC__Msg[];

struct Array {
  WORD *GCINFO;
  Int size;
  POLY elems[];
};
extern WORD __GC__Array0[];
extern WORD __GC__Array1[];


typedef struct Array *Array;

struct Timer;
typedef struct Timer *TIMERTYPE;


struct Timer {
  WORD *GCINFO;
  UNIT (*reset) (TIMERTYPE, Int);
  Time (*sample) (TIMERTYPE, Int);
};

extern WORD __GC__Timer[];

UNIT ASYNC(Msg, Time, Time);
OID  LOCK(OID);
UNIT UNLOCK(OID);
void RAISE(Int);

POLY     Raise(BITS32, Int);

Array primListArray(BITS32,LIST);
Array primUniArray(BITS32,Int,POLY);
Array EmptyArray(BITS32,Int);
Array CloneArray(BITS32,Array,Int);
Array primUpdateArray(BITS32,Array,Int,POLY);

Array CYCLIC_BEGIN(Int, Int);
void  CYCLIC_UPDATE(Array, Int, ADDR stop);
void  CYCLIC_END(Array, ADDR stop);

POLY primRefl(BITS32,POLY);

TIMERTYPE primTIMERTERM(Int x);
UNIT      ABORT(BITS32,Msg msg,Ref x);

LIST primShowFloat(Float x);
LIST getStr(char *p) ;
Int strEq (LIST s1, LIST s2) ;
#endif
