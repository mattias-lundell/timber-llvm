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

#ifndef RTS_H_
#define RTS_H_

#include <stddef.h>
#include <sys/time.h>
#include <pthread.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include "config.h"

typedef int WORD;
typedef WORD *ADDR;

#define Int int
#define Float float
#define Char char
#define Bool char
#define FALSETYPE char          // alias for singleton type
#define TRUETYPE char           // alias for singleton type
#define TUP0 char

#define UNIT TUP0
#define POLY ADDR

#define OID ADDR
#define BITS8 unsigned char
#define BITS16 unsigned short
#define BITS32 unsigned int

#define DISABLE(mutex)  pthread_mutex_lock(&mutex)
#define ENABLE(mutex)   pthread_mutex_unlock(&mutex)
#define TIMERGET(x)     gettimeofday(&x, NULL)



#define primSHIFTRA8(a,b)  ((signed char)(a) >> (b))
#define primSET8(a,b)      ((a) | (1 << (b)))
#define primCLR8(a,b)      ((a) & ~(1 << (b)))
#define primTST8(a,b)      (((a) & (1 << (b))) != 0)

#define primSHIFTRA16(a,b) ((signed short)(a) >> (b))
#define primSET16(a,b)     ((a) | (1 << (b)))
#define primCLR16(a,b)     ((a) & ~(1 << (b)))
#define primTST16(a,b)     (((a) & (1 << (b))) != 0)

#define primSHIFTRA32(a,b) ((signed int)(a) >> (b))
#define primSET32(a,b)     ((a) | (1 << (b)))
#define primCLR32(a,b)     ((a) & ~(1 << (b)))
#define primTST32(a,b)     (((a) & (1 << (b))) != 0)


union FloatCast {
    float f;
    POLY p;
};

#define POLY2Float(x)   ((union FloatCast )(x)).f
#define Float2POLY(x)   ((union FloatCast )((float)x)).p

#define ZEROBITS        0
#define ORBITS(a,b)     (a) | (b)
#define SETBIT(n)       (1 << n)
#define COPYBIT(x,m,n)  (((x >> m) & 1) << n)

#define INF             0x7fffffff

typedef struct timeval AbsTime;

typedef struct Ref *Ref;

struct Ref {
    WORD *GCINFO;
    pthread_mutex_t mut;
    POLY STATE;
};

typedef struct Thread *Thread;

typedef struct Msg *Msg;

struct Thread {
   Thread next;            // for use in linked lists
   Msg msg;                // message under execution
   int prio;
   pthread_t id;
   int index;
   pthread_cond_t trigger;
   int placeholders;       // for use during cyclic data construction
};

#define STATEOF(ref)    (((ADDR)(ref))+WORDS(sizeof(struct Ref)))

void INITREF(Ref);

extern WORD __GC__Ref[];

pthread_mutex_t rts;
pthread_mutexattr_t glob_mutexattr;


#define SIGSELECT SIGUSR1


struct Time {
  WORD *GCINFO;
  Int sec;
  Int usec;
};

typedef struct Time *Time;


extern WORD __GG__Time[] ;


#define WORDS(bytes)            (((bytes)+sizeof(WORD)-1)/sizeof(WORD))
#define BYTES(words)            ((words)*sizeof(WORD))


#if defined(HAVE_OSX_ATOMICS)
#include <libkern/OSAtomic.h>
#define CAS(old,new,mem)        OSAtomicCompareAndSwap32((WORD)(old),(WORD)(new),(ADDR)(mem))
#else
#if defined(HAVE_BUILTIN_ATOMIC) 
#define CAS(old,new,mem)        __sync_bool_compare_and_swap((mem),(WORD)(old),(WORD)(new))
#else
#error "Can not define CAS on your architecture."
#endif
#endif
/*
#define NEW(t,addr,words)       { ADDR top,stop; \
                                  do { addr = (t)hp; stop = lim; top = ((ADDR)addr)+(words); } \
                                  while (!CAS(addr,top,&hp)); \
                                  if (top>=stop) { addr = (t)force((words),(ADDR)addr<stop?(ADDR)addr:0);} }
*/

#define NEW(t,addr,words)       { ADDR tmp; \
                                  new(&tmp,words); \
                                  addr = (t)tmp; } 

// Note: soundness of the spin-loop above depends on the invariant that lim is never changed unless hp also changes.

#define CURRENT()               ((Thread)pthread_getspecific(current_key))


extern ADDR hp, lim;
extern pthread_key_t current_key;

ADDR force(WORD, ADDR);

Time sec(Int c);
Time millisec(Int x);
Time microsec(Int x);
Int secOf(Time t);
Int microsecOf (Time t);

#define Inherit ((Time)0)
#define Infinity ((Time)1)

#define INHERIT 0
#define TIME_INFINITY 1

Time primTimeMin(Time t1, Time t2);
Time primTimePlus(Time t1, Time t2);
Time primTimeMinus(Time t1, Time t2);

Bool primTimeEQ(Time t1, Time t2);
Bool primTimeNE(Time t1, Time t2);
Bool primTimeLT(Time t1, Time t2);
Bool primTimeLE(Time t1, Time t2);
Bool primTimeGT(Time t1, Time t2);
Bool primTimeGE(Time t1, Time t2);

struct FunList;
typedef struct FunList *FunList;

struct FunList {
  void (*f) (); 
  FunList next;
};

void addRootScanner(FunList ls);
int getArgc();
char **getArgv();
Thread newThread(Msg m, int prio, void *(*fun)(void *), int stacksize) ;

// new does the same thing as the macro NEW but as a callable function
void new(ADDR* addr, size_t bytes);
//void new(ADDR* addr, size_t bytes) __attribute__((always_inline));
#endif
