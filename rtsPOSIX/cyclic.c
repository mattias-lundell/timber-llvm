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


#define BIT0(val)               (((WORD)(val)) & 0x01)
#define CLR0(val)               ((WORD)(val) & (~0x01))
#define TOGGLE0(val)            ((WORD)(val) ^ 0x01)

#define ISPLACEHOLDER(obj)      (BIT0(obj) && ((int)(obj) < 0))
#define INDEXOF(obj)            (((-(int)(obj)) >> 1) - current_thread->placeholders)
#define PLACEHOLDER(index)      (-((((index) + current_thread->placeholders) << 1) | 0x01))

#define CURRENT_CYCLE(obj,roots)    !INSIDE(heapchain,obj,(ADDR)roots)

#define SUBST(obj,off,roots,lim)      { ADDR obj1 = (ADDR)obj[off]; \
                                        if (ISPLACEHOLDER(obj1)) { \
                                                int index = INDEXOF(obj1); \
                                                if (index >= 0 && index < (lim)) \
                                                        obj[off] = (WORD)roots->elems[index]; \
                                        } \
                                      }

ADDR substObj(ADDR obj, Array roots, int limit, Thread current_thread) {
        ADDR info = IND0(obj);
        if (!info)                                      // if gcinfo is null we have reached the end of a heap segment
                return (ADDR)obj[1];                    // pointer to first object of next segment is found in subsequent slot
        switch (GC_TYPE(info)) {
                case GC_STD: {
                        WORD size = STATIC_SIZE(info), i = 2, offset = info[i];
                        while (offset) {
                                SUBST(obj,offset,roots,limit);
                                offset = info[++i];
                        }
                        return obj + size;
                }
                case GC_ARRAY: {
                        WORD size = STATIC_SIZE(info) + obj[1], offset = 2;     // find size of dynamic part in second slot of obj, add static size
                        if (info[2])
                                return obj + size;                              // return immediately if array contains only scalars
                        while (offset<size) {
                                SUBST(obj,offset,roots,limit);
                                offset++;
                        }
                        return obj + size;
                }
                case GC_TUPLE: {
                        WORD width = obj[1], offset = 1 + POLYTAGS(width), i = 1, j, tags;
                        while (width > 32) {
                                for (j = 0, tags = obj[i++]; j < 32; j++, offset++, tags = tags >> 1)
                                        if (!(tags & 1))
                                                SUBST(obj,offset,roots,limit);
                                width -= 32;
                        }
                        for (tags = obj[i]; width > 0; width--, offset++, tags = tags >> 1) 
                                if (!(tags & 1))
                                        SUBST(obj,offset,roots,limit);
                        return obj + STATIC_SIZE(info) + width + POLYTAGS(width);
                }
                case GC_BIG: {
                        WORD size = STATIC_SIZE(info), i = 2, offset = info[i];
                        while (offset) {                                        // subst all statically known pointer fields
                                SUBST(obj,offset,roots,limit);
                                offset = info[++i];
                        }
                        offset = info[++i];
                        while (offset) {                                        // subst dynamically identified pointers
                                WORD tagword = info[++i];
                                WORD bitno = info[++i];
                                if ((tagword & (1 << bitno)) == 0)
                                        SUBST(obj,offset,roots,limit);
                                offset = info[++i];
                        }
                        return obj + size;
                }
                case GC_MUT: {
                        return substObj(obj + STATIC_SIZE(info), roots, limit, current_thread);
                }
        }
        return (ADDR)0;                 // Not reached
}

void subst(Array roots, int limit, ADDR stop, Thread current_thread) {
        ADDR p = (ADDR)roots + STATIC_SIZE(roots->GCINFO) + roots->size;
        int i;
        for (i = 0; i < limit; i++)
                if (ISPLACEHOLDER(roots->elems[i])) 
                        RAISE(2);
        while (p != stop)
                p = substObj(p, roots, limit, current_thread);
}

Array CYCLIC_BEGIN(Int n, Int updates) {
        Thread current_thread = CURRENT();
        Array roots = EmptyArray(0,n);
        int i;
        for (i = 0; i < n; i++)
                roots->elems[i] = (POLY)PLACEHOLDER(i);
        current_thread->placeholders += n;
        return roots;
}

void CYCLIC_UPDATE(Array roots, Int limit, ADDR stop) {
        Thread current_thread = CURRENT();
        current_thread->placeholders -= roots->size;
        subst(roots, limit, stop, current_thread);
        current_thread->placeholders += roots->size;
}

void CYCLIC_END(Array roots, ADDR stop) {
        Thread current_thread = CURRENT();
        current_thread->placeholders -= roots->size;
        subst(roots, roots->size, stop, current_thread);
}

