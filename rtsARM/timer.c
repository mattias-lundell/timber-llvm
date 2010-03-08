// The Timber compiler <timber-lang.org>
// 
// Copyright 2008 Johan Nordlander <nordland@csee.ltu.se>
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


#include "timber.h"

struct S_Timer {
    WORD *GCINFO;
    AbsTime start;
};

typedef struct S_Timer *S_Timer;

struct T_Timer;
typedef struct T_Timer *T_Timer;

struct T_Timer {
    WORD *GCINFO;
    UNITTYPE (*reset) (T_Timer, Int);
    Time (*sample) (T_Timer, Int);
    Ref self;
};


static WORD __GC__T_Timer[] = {WORDS(sizeof(struct T_Timer)), GC_STD, WORDS(offsetof(struct T_Timer,self)), 0};
   
static WORD __GC__S_Timer[] = {WORDS(sizeof(struct S_Timer)), GC_STD, 0};

static WORD __GC__Time[]    = {WORDS(sizeof(struct Time)), GC_STD, 0};


Time sec(Int c) {
    Time res;
    NEW(Time,res,WORDS(sizeof(struct Time)));
    res->GCINFO = __GC__Time;
    res->time = c*100000;
    return res;
}

Time millisec(Int c) {
    Time res;
    NEW(Time,res,WORDS(sizeof(struct Time)));
    res->GCINFO = __GC__Time;
    res->time = c * 100;
    return res;
}

Time microsec(Int c) {
    Time res;
    NEW(Time,res,WORDS(sizeof(struct Time)));
    res->GCINFO = __GC__Time;
    res->time = c / 10;
    return res;
}

Int secOf(Time t) {
    switch ((Int)t) {
        case INHERIT: panic("secOf Inherit");
        case TIME_INFINITY: panic("secOf infinity");
        default: return t->time / 100000;
    }
}

Int microsecOf(Time t) {
    switch ((Int)t) {
        case INHERIT: panic("microsecOf Inherit");
        case TIME_INFINITY: panic("microsecOf infinity");
        default: return t->time * 10;
    }
}



Time primTimePlus(Time t1, Time t2) {
    Time res;
    switch ((Int)t1) {
        case INHERIT: return t2;
        case TIME_INFINITY: return Infinity;
        default: 
            switch ((Int)t2) {
                case INHERIT: return t1;
                case TIME_INFINITY: return Infinity;
                default:
                    NEW(Time,res,WORDS(sizeof(struct Time)));
                    res->GCINFO = __GC__Time;
                    res->time = t1->time + t2->time;
                    return res;
            }
    }
}

Time primTimeMin(Time t1, Time t2) {
    switch ((Int)t1) {
        case INHERIT: 
        case TIME_INFINITY:return t2;
        default:
            switch ((Int)t2) {
                case INHERIT: 
                case TIME_INFINITY: return t1;
                default: 
                    if (t1->time < t2->time )
	                    return t1;
                    else
	                    return t2;
            }
    }
}


Time primTimeMinus(Time t1, Time t2) {
    Time res;
    switch ((Int)t1) {
        case INHERIT: panic("primTimeMinus Inherit");
        case TIME_INFINITY: 
            switch((Int) t2) {
                case INHERIT: panic("primTimeMinus Inherit");
                case TIME_INFINITY: panic("infinity - infinity");
                default: return Infinity;
            }      
        default:
            switch ((Int)t2) {
                case INHERIT: panic("primTimeMinus Inherit");
                case TIME_INFINITY:
                    NEW(Time,res,WORDS(sizeof(struct Time)));
                    res->GCINFO = __GC__Time;
                    res->time = 0;
                    return res;
                default:
                    NEW(Time,res,WORDS(sizeof(struct Time)));
                    res->GCINFO = __GC__Time;
                    res->time = t1->time - t2->time;
                    return res;
            }
    }
}

Bool primTimeEQ(Time t1, Time t2) {
    switch ((Int)t1) {
        case INHERIT: panic("primTimeEQ Inherit");
        case TIME_INFINITY: 
            switch((Int)t2) {
                case INHERIT: panic("primTimeEQ Inherit");
                case TIME_INFINITY: return 1;
                default: return 0;
            }
        default:
            switch ((Int)t2) {
                case INHERIT: panic("primTimeEQ Inherit");
                case TIME_INFINITY: return 0;
                default:
                    return (t1->time == t2->time );
            }
    }
}

Bool primTimeNE(Time t1, Time t2) {
    switch ((Int)t1) {
        case INHERIT: panic("primTimeNE Inherit");
        case TIME_INFINITY: 
            switch((Int)t2) {
                case INHERIT: panic("primTimeNE Inherit");
                case TIME_INFINITY: return 0;
                default: return 1;
            }
        default:
            switch ((Int)t2) {
                case INHERIT: panic("primTimeNE Inherit");
                case TIME_INFINITY: return 1;
                default:
                    return (t1->time != t2->time );
            }
    }
}

Bool primTimeLT(Time t1, Time t2) {
    switch ((Int)t1) {
        case INHERIT: panic("primTimeLT Inherit");
        case TIME_INFINITY: 
            switch((Int)t2) {
                case INHERIT: panic("primTimeLT Inherit");
                default: return 0;
            }
        default:
            switch ((Int)t2) {
                case INHERIT: panic("primTimeLT Inherit");
                case TIME_INFINITY: return 1;
                default:
                    return (t1->time < t2->time );
            }
    }
}

Bool primTimeLE(Time t1, Time t2) {
    switch ((Int)t1) {
        case INHERIT: panic("primTime InheritLE");
        case TIME_INFINITY: 
            switch((Int)t2) {
                case INHERIT: panic("primTime InheritLE");
                default: return 0;
            }
        default:
            switch ((Int)t2) {
                case INHERIT: panic("primTime InheritLE");
                case TIME_INFINITY: return 1;
                default:
                    return (t1 < t2 );
            }
    }
}

Bool primTimeGT(Time t1, Time t2) {
    return primTimeLT(t2,t1);
}

Bool primTimeGE(Time t1, Time t2) {
    return primTimeLE(t2,t1);
}

static UNITTYPE reset_fun(Ref self, Int x) {
    self = (Ref)LOCK((OID)self);
    ((S_Timer)STATEOF(self))->start = current->msg->baseline;
    UNLOCK((OID)self);
    return (UNITTYPE)0;
}

static Time sample_fun(Ref self, Int x) {
    self = (Ref)LOCK((OID)self);
    AbsTime now;
    now = current->msg->baseline;
    SUB(now,((S_Timer)STATEOF(self))->start);
    UNLOCK((OID)self);
    Time res;
    NEW(Time,res,WORDS(sizeof(struct Time)));
    res->GCINFO = __GC__Time;
    res->time = now;
    return res;
}

static UNITTYPE reset_sel(T_Timer this,Int x) {
    return reset_fun(this->self,x);
}

static Time sample_sel(T_Timer this, Int x) {
    return sample_fun(this->self,x);
}

TIMERTYPE primTIMERTERM(Int x) {
    Ref self;
    NEW(Ref,self,WORDS(sizeof(struct Ref))+WORDS(sizeof(struct S_Timer)));
    INITREF(self);
    ((S_Timer)STATEOF(self))->GCINFO = __GC__S_Timer;
    ((S_Timer)STATEOF(self))->start = current->msg->baseline;
    T_Timer res;
    NEW(T_Timer,res,WORDS(sizeof(struct T_Timer)));
    res->GCINFO = __GC__T_Timer;
    res->reset = reset_sel;
    res->sample = sample_sel;
    res->self = self;
    return (TIMERTYPE)res;
}
