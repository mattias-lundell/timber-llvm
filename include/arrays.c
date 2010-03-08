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

// Array primitives

#include "timber.h"

Array primListArray(BITS32 polytag, LIST xs) {
        Int len = 0;
        Int i = 0;
        LIST ys = xs;
        while ((Int)ys) {len++; ys = ((CONS)ys)->b;}; // not nice to compute the length here ...
        ADDR tmp;
        //NEW(Array,res,WORDS(sizeof(struct Array))+len);
        new(&tmp,WORDS(sizeof(struct Array))+len);
        Array res;
        res = (Array)tmp;
        res->GCINFO = (polytag ? __GC__Array1 : __GC__Array0);
        res->size = len;
        ys = xs;
        while((Int)ys) {res->elems[i] = ((CONS)ys)->a; ys = ((CONS)ys)->b; i++;}
        return res;
}

Array primUniArray(BITS32 polytag, Int len, POLY a) {
        Int i;
        //NEW(Array,res,WORDS(sizeof(struct Array))+len);
        ADDR tmp;
        new(&tmp,WORDS(sizeof(struct Array))+len);
        Array res;
        res = (Array)tmp;
        res->GCINFO = (polytag ? __GC__Array1 : __GC__Array0);
        res->size = len;
        for(i=0; i<len; i++) res->elems[i] = a;
        return res;
}

Array EmptyArray(BITS32 polytag, Int len) {
        //NEW(Array,res,WORDS(sizeof(struct Array))+len);
        ADDR tmp;
        new(&tmp,WORDS(sizeof(struct Array))+len);
        Array res; 
        res = (Array)tmp;
        res->GCINFO = (polytag ? __GC__Array1 : __GC__Array0);
        res->size = len;
        return res;
}

Array CloneArray(BITS32 polytag, Array a, Int lev) {
        if (!lev)
                return a;
        Int i; 
        //NEW(Array,res,WORDS(sizeof(struct Array))+a->size);
        ADDR tmp;
        new(&tmp,WORDS(sizeof(struct Array))+a->size);
        Array res; 
        res = (Array)tmp;
        res->size = a->size;
        lev--;
        if (lev) {
                res->GCINFO = __GC__Array0;
                for (i=0; i < a->size; i++) 
                        res->elems[i] = (ADDR)CloneArray(polytag, (Array)a->elems[i], lev);
        } else {
                res->GCINFO = (polytag ? __GC__Array1 : __GC__Array0);
                for (i=0; i < a->size; i++) 
                        res->elems[i] = a->elems[i];    
        }
        return res;
}

Array primUpdateArray(BITS32 polytag, Array a, Int i, POLY v) {
        a = CloneArray(polytag, a, 1);
        a->elems[i] = v;
        return a;
}
