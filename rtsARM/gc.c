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

#include <stdlib.h>
#if defined(__APPLE__)
#include <mach-o/getsect.h>
#endif

#define NEW2(addr,words,info)   { int status = ISPROTECTED(); \
                                  PROTECT(1); \
	                              addr = hp2; \
	                              hp2 = (ADDR)addr+(words); \
                                  if (hp2 >= lim2) force2(words,(ADDR)addr,info); \
                                  addr[0] = (WORD)(info); \
		                          PROTECT(status); \
                                }


#define IND0(obj)               (ADDR)((ADDR)obj)[0]
#define GC_PROLOGUE(obj)        { if (ISFORWARD(IND0(obj))) obj = (Ref)IND0(obj); }             // read barrier
#define GC_EPILOGUE(obj)        { if (ISBLACK((ADDR)obj)) { ADDR a; NEW2(a,1,(ADDR)obj); } \
                                  else if ((ADDR)obj == gcobj) gcobj = 0; }                     // write barrier

#define GC_STD                  0
#define GC_ARRAY                1
#define GC_TUPLE                2
#define GC_BIG                  3
#define GC_MUT                  4

#define POLYTAGS(width)         (((width)+31) % 32)

#define GC_TYPE(info)           (info[1])
#define STATIC_SIZE(info)       (info[0])

#define allocwords(size)        (ADDR)malloc(BYTES(size))
#define HEAPSIZE                0x100000  //  in words

#define ISWHITE(a)              INSIDE(base,a,hp)
#define ISBLACK(a)              hp2 && INSIDE(base2,a,scanp)
#define ISFORWARD(info)         INSIDE(theheap,info,&theheap[2*HEAPSIZE])

#define INSIDE(base,a,lim)      (base <= (a) && (a) < lim)

WORD heapsize;                          // actual size (after static heap has been cut off)
ADDR base, lim, hp;                     // start, end, and current pos of latest "fromspace" (normal space)
ADDR base2, lim2, hp2;                  // start, end and current pos of latest "tospace" (only used during gc)
ADDR scanp;                             // current pos during scanning (only used during gc)
ADDR gcobj;                             // currently copied mutable object, or 0

char emergency = 0;                     // flag signalling heap overflow during gc

void scanEnvRoots(void);
void scanTimerQ(void);
extern int envRootsDirty;
extern int timerQdirty;

WORD theheap[2*HEAPSIZE];

ADDR heap1, heap2;

void pruneStaticHeap() {
    ADDR obj = base + 1;
    while (obj != hp) {
        ADDR info = IND0(obj);
        obj[0] = 0;                     // Mark as static
        WORD size = STATIC_SIZE(info);
        switch (GC_TYPE(info)) {
            case GC_ARRAY:  size += obj[1]; break;
            case GC_TUPLE:  size += obj[1] + POLYTAGS(obj[1]);
        }
        obj = obj + size;
    }

    heap1 = hp;
    heapsize = (HEAPSIZE*2 - (heap1 - theheap)) / 2;
    heap2 = &theheap[heapsize];
    base = heap1;
    base[0] = 0;
    hp = base + 1;
    lim = base + heapsize - 1;
    // printf("Pruned static heap (was: %x (words), is: %x (words))\n", HEAPSIZE, heapsize);
}

ADDR force(WORD size, ADDR last) {                      // Overflow in fromspace
    panic("Heap overflow");
    return 0;
}

ADDR force2(WORD size, ADDR last, ADDR info) {          // Overflow in tospace
    panic("Heap overflow (tospace)");
    return 0;
}

ADDR copystateful(ADDR obj, ADDR info) {
    WORD i = STATIC_SIZE(info);
    ADDR dest, datainfo = IND0(obj+i);              // actual mutable struct follows right after the Ref struct
    WORD size = i + STATIC_SIZE(datainfo);          // dataobj must be a GC_STD or a GC_BIG
    NEW2(dest,size,info);
    INITREF((Ref)dest);
    while (1) {
        gcobj = obj;
        for (i = STATIC_SIZE(info); i < size; i++)
            dest[i] = obj[i];
        if (info[2]) {
            // object has mutable arrays...  !!!!!!!!
        }
        PROTECT(1);
        if (gcobj == obj)
            break;
        PROTECT(0);
    }
    obj[0] = (WORD)dest;
    gcobj = 0;
    PROTECT(0);
    return dest;
}

ADDR copy(ADDR obj) {
        if (!ISWHITE(obj))                              // don't copy if obj is a tospace address or a low range constant
                return obj;
        ADDR dest, info = IND0(obj);
        if (!info)                                      // don't copy if obj is in static heap
                return obj;
        if (ISFORWARD(info))                            // gcinfo should point to static data;
                return info;                            // if not, we have a forward ptr
        WORD i, size = STATIC_SIZE(info);
        switch (GC_TYPE(info)) {                              
                case GC_ARRAY:  size += obj[1]; break;
                case GC_TUPLE:  size += obj[1] + POLYTAGS(obj[1]); break;
                case GC_MUT:    return copystateful(obj,info);
                default:        break;
        }
        NEW2(dest,size,info);                           // allocate in tospace and initialize with gcinfo
        for (i=0; i<size; i++)
                dest[i] = obj[i];
        obj[0] = (WORD)dest;                            // mark fromspace object as forwarded
        return dest;
}


ADDR scan(ADDR obj) {
        ADDR info = IND0(obj);
        if (!info)                                      // if gcinfo is null we have reached the end of a tospace segment
                return (ADDR)0;                         
        if (ISFORWARD(info)) {                          // gcinfo should point to static data;
                scan(info);                             // if not, we have a write barrier (rescan request)
                return obj + 1;
        }
        switch (GC_TYPE(info)) {
                case GC_STD: {
                        WORD size = STATIC_SIZE(info), i = 2, offset = info[i];
                        while (offset) {
                                obj[offset] = (WORD)copy((ADDR)obj[offset]);
                                offset = info[++i];
                        }
                        return obj + size;
                }
                case GC_ARRAY: {
                        WORD size = STATIC_SIZE(info) + obj[1], offset = 2;     // find size of dynamic part in second slot of obj, add static size
                        if (info[2])
                                return obj + size;                              // return immediately if array contains only scalars
                        while (offset<size) {
                                obj[offset] = (WORD)copy((ADDR)obj[offset]);
                                offset++;
                        }
                        return obj + size;
                }
                case GC_TUPLE: {
                        WORD width = obj[1], offset = 1 + POLYTAGS(width), i = 1, j, tags;
                        while (width > 32) {
                                for (j = 0, tags = obj[i++]; j < 32; j++, offset++, tags = tags >> 1)
                                        if (!(tags & 1))
                                                obj[offset] = (WORD)copy((ADDR)obj[offset]);
                                width -= 32;
                        }
                        for (tags = obj[i]; width > 0; width--, offset++, tags = tags >> 1) 
                                if (!(tags & 1))
                                        obj[offset] = (WORD)copy((ADDR)obj[offset]);
                        return obj + STATIC_SIZE(info) + width + POLYTAGS(width);
                }
                case GC_BIG: {
                        WORD size = STATIC_SIZE(info), i = 2, offset = info[i];
                        while (offset) {                                        // scan all statically known pointer fields
                                obj[offset] = (WORD)copy((ADDR)obj[offset]);
                                offset = info[++i];
                        }
                        offset = info[++i];
                        while (offset) {                                        // scan dynamically identified pointers
                                WORD tagword = info[++i];
                                WORD bitno = info[++i];
                                if ((tagword & (1 << bitno)) == 0)
                                        obj[offset] = (WORD)copy((ADDR)obj[offset]);
                                offset = info[++i];
                        }
                        return obj + size;
                }
                case GC_MUT: {
                        return scan(obj + STATIC_SIZE(info));
                }
        }
        return (ADDR)0;                 // Not reached
}

void gc() {
        // debug("$");
        PROTECT(1);
        base2 = (base==heap1 ? heap2 : heap1);          // allocate tospace
        base2[0] = 0;
        lim2 = base2 + heapsize - 1;                    // leave room for a one wor node at the end
        hp2 = base2 + 1;
        scanp = hp2;
        PROTECT(0);
        
        envRootsDirty = 1;
        timerQdirty = 1;

        while (1) {
                if (envRootsDirty)
                        scanEnvRoots();
                if (timerQdirty)
                        scanTimerQ();

                while (1) {
                        if (scanp == hp2)
                                break;                  // break loop when we seem to be done
                        scanp = scan(scanp);
                }

                PROTECT(1);
                if ((scanp == hp2) && (envRootsDirty+timerQdirty == 0)) // still done?
                        break;                                          // Continue with exclusive rts access
                PROTECT(0);
        }
        
        base = base2;
        lim = lim2;
        hp = hp2;
        // fprintf(stderr, "\n!!!Heap switched:  base=%x lim=%x (hp=%x)\n", (int)base, (int)lim, (int)hp);

        base2 = lim2 = hp2 = (ADDR)0;
        scanp = (ADDR)0;
        PROTECT(0);
}

int heapLevel(int steps) {
    int status = ISPROTECTED();
    PROTECT(1);
    int diff = hp - base;
    PROTECT(status);
    return diff / (heapsize/steps);
}


void gcInit() {
    base2 = lim2 = hp2 = (ADDR)0;                   // no active tospace
    heap1 = theheap;
    heapsize = HEAPSIZE;
    heap2 = &theheap[HEAPSIZE];
    base = heap1;
    base[0] = 0;
    hp = base + 1;
    lim = base + heapsize - 1;
    gcobj = 0;
}

