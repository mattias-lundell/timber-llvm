/*
 * Copyright (c) 2009, Per Lindgren, Johan Eriksson, Johan Nordlander,
 * Simon Aittamaa.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *     * Neither the name of the Luleå University of Technology nor the
 *       names of its contributors may be used to endorse or promote products
 *       derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "rts.h"
#include "timber.h"

#include <lpc2468_registers.h>

/* ************************************************************************** */

void panic(char *);
void debug(char *);
void debug_hex(unsigned long);

#define ARM7_CONTEXT_SAVE() \
	__asm__ __volatile__ (\
		/* r0 & r1 is used as a temporary register, we need lr in swi mode. */\
		"stmfd	sp!, {r0,r1,lr}\n"\
		/* Get the user mode stack pointer value. */\
		"stmdb	sp, {sp}^\n"\
		"nop\n"\
		"ldmdb	sp, {r0}\n"\
		/* Store the return address at the first/highest address. */\
		"sub	lr, lr, #4\n"\
		"stmfd	r0!, {lr}\n"\
		/* Start using lr and restore old r0. */\
		"mov	lr, r0\n"\
		"ldmfd	sp, {r0}\n"\
		/* Save all user mode registers. */\
		"stmfd	lr, {r0-r14}^\n"\
		"nop\n"\
		"sub	lr, lr, #60\n"\
		/* Save the saved process register. */\
		"mrs	r0, SPSR\n"\
		"stmfd	lr!, {r0}\n"\
		/* Save the stack pointer to the the current variable. */\
		"ldr	r0, =current\n"\
		"ldr	r0, [r0]\n"\
		"str	lr, [r0]\n"\
		/* Check the context cookie. */\
		"ldr	r0, [r0, #4]\n"\
		"ldr	r0, [r0]\n"\
		"ldr	r1, =arm7_context_cookie\n"\
		"ldr	r1, [r1]\n"\
		"cmp	r0, r1\n"\
		"bne	arm7_context_panic\n"\
		/* \
		 * Restore the old r0 _again_, this is so we can use this macro\
		 * in the software interrupt as well. Don't forget the original\
		 * lr as well for interrupt id.\
		 */\
		"ldmfd	sp!, {r0,r1,lr}\n"\
		)

#define ARM7_CONTEXT_RESTORE() \
	__asm__ __volatile__ (\
		/* Load the current context stack pointer. */\
		"ldr	r0, =current\n"\
		"ldr	r0, [r0]\n"\
		"ldr	lr, [r0]\n"\
		/* Check the context cookie. */\
		"ldr	r0, [r0, #4]\n"\
		"ldr	r0, [r0]\n"\
		"ldr	r1, =arm7_context_cookie\n"\
		"ldr	r1, [r1]\n"\
		"cmp	r0, r1\n"\
		"bne	arm7_context_panic\n"\
		/* Restore the saved saved process status. */\
		"ldmfd	lr!, {r0}\n"\
		"msr	SPSR, r0\n"\
		/* Restore the user context. */\
		"ldmfd	lr, {r0-r14}^\n"\
		"nop\n"\
		"add	lr, lr, #60\n"\
		/* Get the return address and return(leaves interrupt..). */\
		"ldmfd	lr, {pc}^\n"\
		)


#define NTHREADS        5
#define STACKSIZE       65536  
#define IDLESTACKSIZE   65536  

#define ENV_STACKSIZE 65536*10
#define ENV_STACKSIZE_IDLE 65536
#define ARM7_STACKSIZE (NTHREADS*ENV_STACKSIZE+ENV_STACKSIZE_IDLE)

#define SLEEP()         /* not yet defined */

#define TDELTA          1
#define TIMERINIT()     { T0TCR = 0; T0TCR = 1; /* stop & reset clock */ \
	                      T0IR = (1<<0);        /* MR0 int en */ \
	                      T0PR = (576 -1);      /* 1tick = 10uS , at MCLK = 57.6Mhz*/ \
	                      T0MCR = (1<<0); \
	                      VICVectAddr4 = (unsigned long)timer0_interrupt; \
	                      VICIntEnable |= (1<<4); \
                          VICSoftInt = 1<<4;    /* Force a timer interrupt, will hot-start the system */ \
                        }
#define TIMERGET(x)     { x = T0TC; }
#define TIMERSET(x,now) { if (x) { \
                             T0MR0 = x->baseline; \
                             if ((T0MR0 < T0TC)) \
                                VICSoftInt = 1<<4; \
                          } \
                        }
#define TIMERACK()      { VICSoftIntClr = (1<<4); T0IR = T0IR; }

#define LESS(a,b)       ( a < b )
#define LESSEQ(a,b)     ( a <= b )
#define ADD(a,t)        { a += t->time; }
#define SUB(a,b)        { a -= b; }


#define INF             0x7fffffff



// Thread management --------------------------------------------------------------------------------

typedef struct {
	unsigned int *sp;
	unsigned int *cookie;
} arm7_context_t;

struct Thread {
    arm7_context_t context;     // machine state
	Thread next;                // for use in linked lists
    struct Msg *msg;            // message under execution
    Ref waitsFor;               // deadlock detection link
    WORD visit_flag;            // for use during cyclic data construction
    int placeholders;           // for use during cyclic data construction
};

struct Msg msg0         = { NULL, 0,  0 ,  INF , NULL };

struct Thread threads[NTHREADS];
struct Thread threadI;         // the idle process

Msg savedMsg            = NULL;
Msg msgQ                = NULL;
Msg timerQ              = NULL;

Thread threadPool       = &threads[1];
Thread activeStack      = threads;
Thread current          = &threadI;

unsigned int arm7_stack[ARM7_STACKSIZE/4];

const unsigned int arm7_context_cookie = 0x55aa55aa;

void CONTEXT_INIT(arm7_context_t *context, int stacksize, void* function)
{
	int i;
	static unsigned int arm7_stack_offset = ARM7_STACKSIZE-ENV_STACKSIZE_IDLE;

	/* Check for alignment. */
	if (stacksize & 0x3)
		panic("CONTEXT_INIT(): Missalligned stacksize requested.\n");
	
	/* Make sure we have enough stack and assign some to the context. */
	if (arm7_stack_offset < stacksize)
		panic("CONTEXT_INIT(): Out of memory.\n");
	context->sp = (unsigned int *)&arm7_stack[arm7_stack_offset];
	context->cookie = (unsigned int*)&arm7_stack[arm7_stack_offset-stacksize];
	*context->cookie = arm7_context_cookie;

	i = (int)context->sp;

	/* Push the return address onto the stack. */
	*(--context->sp) = (unsigned int)function;

	/* Push a bogus link register onto the stack. */
	*(--context->sp) = (unsigned int)0x00000000;

	/* Push the stack pointer onto the stack. */
	*(--context->sp) = (unsigned int)i;

	/* Push some fake registers onto the stack. */
	for (i=13;i>0;i--)
	{
		*(--context->sp) = i-1;
	}

	/* Push the SPSR onto the stack. */
	*(--context->sp) = 0xdf; /* System mode, all interrupts disabled. */

	/* Save the offset for the next stack. */
	arm7_stack_offset -= stacksize;
}



// Last resort -----------------------------------------------------------------------------------

void debug_char(char c) {
		while (!(U0LSR & (1<<5)));
		U0THR = c;    
		if (c == '\n') {
            while (!(U0LSR & (1<<5)));
            U0THR = '\r';
		}
}

void debug(char *msg) {
	while (*msg)
        debug_char(*msg++);
}

void debug_hex(unsigned long value) {
	static char hex[] = "0123456789abcdef";

	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>28&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>24&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>20&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>16&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>12&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>8&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value>>4&0xf];
	while (!(U0LSR & (1<<5)));

	U0THR = hex[value&0xf];
	while (!(U0LSR & (1<<5)));
}

void panic(char *str) {
	PROTECT(1);
	debug(str);
	for (;;);
}

void arm7_context_panic(void)
{
	panic("Context cookie corrupted.\r\n");
}


// Memory management --------------------------------------------------------------------------------

#include "gc.c"


// Cyclic data handling -----------------------------------------------------------------------------

#include "cyclic.c"


// GCINFO definitions for the built-in types -----------------------------------------------------

#include "timber.c"


// Queue management ------------------------------------------------------------------------------

static void enqueueByDeadline(Msg p, Msg *queue) {
        Msg prev = NULL, q = *queue;
        while (q && LESSEQ(q->deadline, p->deadline)) {
                prev = q;
                q = q->next;
        }
        p->next = q;
        if (prev == NULL)
                *queue = p;
        else
                prev->next = p;
}

static void enqueueByBaseline(Msg p, Msg *queue) {
        Msg prev = NULL, q = *queue;
        while (q && LESSEQ(q->baseline, p->baseline)) {
                prev = q;
                q = q->next;
        }
        p->next = q;
        if (prev == NULL)
                *queue = p;
        else
                prev->next = p;
}

static Msg dequeue(Msg *queue) {
        Msg m = *queue;
        if (m)
                *queue = m->next;
        else
                panic("Empty queue");
        return m;
}

static void push(Thread t, Thread *stack) {
        t->next = *stack;
        *stack = t;
}

static Thread pop(Thread *stack) {
        Thread t = *stack;
        *stack = t->next;
        return t;
}

UNITTYPE ABORT(BITS32 polytag, Msg m, Ref dummy){
    m->Code = NULL;
    ADDR info;
    do {
        info = IND0((ADDR)m);
        if (ISFORWARD(info))
            ((Msg)info)->Code = NULL;
    } while (info != IND0((ADDR)m));
    return (UNITTYPE)0;
}


// Context switching ----------------------------------------------------------------------------

__attribute__((naked)) void dispatch( Thread next ) {   // Note: parameter 'next' is vital!
	asm volatile(
		"swi 0\n"
		"mov pc, lr\n"
	);
}

void idle(void) {
    PROTECT(0);

    while (1) {
        if (heapLevel(16) > 13)
		    gc(0);
        SLEEP();
	}
}

static inline void IRQ_PROLOGUE(void) {
    savedMsg = current->msg;
    current->msg = &msg0;
    TIMERGET(msg0.baseline);
}

static inline void IRQ_EPILOGUE(void) {
    current->msg = savedMsg;
    Msg topMsg = activeStack->msg;
    if (msgQ && threadPool && ((!topMsg) || LESS(msgQ->deadline, topMsg->deadline))) {
        push(pop(&threadPool), &activeStack);
		current = activeStack;
    }
}

static void timer0_interrupt(void) {
    IRQ_PROLOGUE();
    TIMERACK();	
    while (timerQ && LESSEQ(timerQ->baseline, msg0.baseline))
        enqueueByDeadline( dequeue(&timerQ), &msgQ );
    TIMERSET(timerQ, msg0.baseline);
    IRQ_EPILOGUE();
}

static void run(void) {
    while (1) {
        Msg this = current->msg = dequeue(&msgQ);
        PROTECT(0);
        Int (*code)(Msg) = this->Code;
        if (code)
            code(this);
        PROTECT(1);
        current->msg = NULL;
        Msg oldMsg = activeStack->next->msg;
                
        if (!msgQ || (oldMsg && LESS(oldMsg->deadline, msgQ->deadline))) {
			void *tmp = pop(&activeStack);
			if (tmp == &threadI)
			    panic("push_tI_on_aS");


            push(tmp, &threadPool);
            Thread t = activeStack;                     // can't be NULL, may be &threadI
            while (t->waitsFor) 
                t = ((Ref)t->waitsFor)->ownedBy;
            dispatch(t);
        }
    } 
}


int timerQdirty;

// Major primitives ---------------------------------------------------------------------

UNITTYPE ASYNC( Msg m, Time bl, Time dl ) {
    AbsTime now;
    TIMERGET(now);

    m->baseline = current->msg->baseline;
    switch ((Int)bl) {
	    case INHERIT: break;
        case TIME_INFINITY:
	        m->baseline = INF;
	        break;
        default:
            ADD(m->baseline, bl);
            if (LESS(m->baseline, now)) {
                m->baseline = now;
                // debug("^");
            }
    }

    switch((Int)dl) {
	    case INHERIT: 
	        m->deadline = current->msg->deadline;
            break;
	    case TIME_INFINITY:
	        m->deadline = INF;
	        break;
	    default:
	        m->deadline = m->baseline;
            ADD(m->deadline, dl);
	}

    int status = ISPROTECTED();
    PROTECT(1);
    if (LESS(now, m->baseline)) {
	    enqueueByBaseline(m, &timerQ);
	    timerQdirty = 1;
	    if (timerQ == m)
		    TIMERSET(timerQ, now);
    } else {
        enqueueByDeadline(m, &msgQ);
    }
    PROTECT(status);
    return (UNITTYPE)0;
}

void INITREF( Ref obj ) {
	obj->GCINFO = __GC__Ref;
	obj->wantedBy = 0;
	obj->ownedBy = 0;
    obj->STATE = (ADDR)STATEOF(obj);                              // actually unused, but keep it clean
}

OID LOCK( OID to_2 ) {
	int status = ISPROTECTED();
    PROTECT(1);
    
	Ref to = (Ref)to_2;
    GC_PROLOGUE(to);
    Thread t = to->ownedBy;
    if (t) {                                                // "to" is already locked
        if (status)
            panic("Deadlock in interrup handler");
        while (t->waitsFor)
            t = ((Ref)t->waitsFor)->ownedBy;
        if (t == current)                                   // deadlock
            panic("Deadlock");
        if (to->wantedBy)
            to->wantedBy->waitsFor = NULL;
        to->wantedBy = current;
        current->waitsFor = to;
        dispatch(t);
    }
    to->ownedBy = current;

    PROTECT(status);
    return (OID)to;
}

UNITTYPE UNLOCK( OID to_2 ) {
    int status = ISPROTECTED();
    PROTECT(1);

	Ref to = (Ref)to_2;
    to->ownedBy = NULL;
    Thread t = to->wantedBy;
    if (t) {                                                // we have run on someone's behalf
        to->wantedBy = NULL;
        t->waitsFor = NULL;
        dispatch(t);
    }
	GC_EPILOGUE(to);

    PROTECT(status);
    return (UNITTYPE)0;
}

static void init_threads(void) {
    int i;

    for (i=0; i<NTHREADS-1; i++)
        threads[i].next = &threads[i+1];
    threads[NTHREADS-1].next = NULL;
 
    i = NTHREADS;
    {
        threadI.next = NULL;
        threadI.waitsFor = NULL;
        threadI.msg = NULL;
        threadI.visit_flag = 0;
        threadI.placeholders = 0;
                
	    CONTEXT_INIT(
			&threadI.context,
			IDLESTACKSIZE,
			idle
			);                    
    }
   
    for (i=NTHREADS-1; i>=0; i--) {
        threads[i].waitsFor = NULL;
        threads[i].msg = NULL;
        threads[i].visit_flag = 0;
        threads[i].placeholders = 0;

	    CONTEXT_INIT(
		    &threads[i].context,
		    STACKSIZE,
		    run
		    );
    }

    TIMERGET(msg0.baseline);

    activeStack = &threadI;
    threadPool  = &threads[0];
}



// Exception handling ----------------------------------------------------------------------------------

void RAISE(Int err) {
        panic("Unhandled exception");
}

POLY Raise(BITS32 polyTag, Int err) {
    RAISE(err);
    return NULL;
}


// Arrays ---------------------------------------------------------------------------------------------

#include "arrays.c"

// Timer ----------------------------------------------------------------------------------------------

#include "timer.c"

// Environment object ---------------------------------------------------------------------------------

void init_rts(void);

#include "env.c"

    
// Show Float -----------------------------------------------------------------------------------------

int snprintf(char *s, int n, const char *format, ...) {
    char *p = "<float>";
    while (*p)
        *s++ = *p++;
    return 7;
}
     
#include "float.c"

// ----------------------------------------------------------------------------------------------------

void scanTimerQ(void) {
	timerQdirty = 0;
	PROTECT(1);
	if (timerQ) {
		timerQ = (Msg)copy((ADDR)timerQ);
        Msg m = timerQ;
	    PROTECT(0);
	    PROTECT(1);
		Msg next = m->next;
		while (next) {
			m->next = (Msg)copy((ADDR)next);
			m = m->next;
	        PROTECT(0);
	        PROTECT(1);
			next = m->next;
		}
	}
	PROTECT(0);
}


/* ************************************************************************** */

__attribute__((naked)) void swi_handler(void)
{
	// Adjudt the link register so that we can treat this as a regular
	// interrupt ie. use save/restore context macros.
	asm volatile("add	lr, lr, #4\n");
	ARM7_CONTEXT_SAVE();
	asm volatile(
	    "ldr    r4, =current\n"
	    "str    r0, [r4]\n"
	);
	ARM7_CONTEXT_RESTORE();
}

void *_arm7_vic_ivr = (void*)&VICVectAddr;

__attribute__((naked))  void irq_handler(void)
{
	ARM7_CONTEXT_SAVE();
	asm volatile(
		"ldr	r0, =_arm7_vic_ivr\n"
		"ldr	r0, [r0]\n"
		"ldr	r0, [r0]\n"
		"mov	lr, pc\n"
		"bx		r0\n"

		"ldr	r0, =_arm7_vic_ivr\n"
		"ldr	r0, [r0]\n"
		"str	r0, [r0]\n"
	);
	ARM7_CONTEXT_RESTORE();
}

// Initialization -------------------------------------------------------------------------------------

void init_rts(void) {
    PROTECT(1);

    TIMERINIT();
    gcInit();
    init_threads();
}
