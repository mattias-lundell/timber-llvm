#ifndef RTS_H_
#define RTS_H_

#define NULL 0

typedef int WORD;
typedef WORD *ADDR;

#define Int int
#define Float float
#define Char char
#define Bool char
#define FALSE char          // alias for singleton type
#define TRUE char           // alias for singleton type
#define UNITTYPE char
#define UNITTERM char       // alias for singleton type
#define POLY ADDR
#define OID ADDR
#define BITS8 unsigned char
#define BITS16 unsigned short
#define BITS32 unsigned int


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

/*
#define SEC(x)          ((x)*1000000)
#define MILLISEC(x)     ((x)*1000)
#define MICROSEC(x)     (x)
*/

struct Thread;
typedef struct Thread *Thread;

struct Msg;

typedef struct Ref *Ref;
struct Ref {
    WORD *GCINFO;
    Thread ownedBy;
    Thread wantedBy;
    POLY STATE;
};

#define STATEOF(ref)    (((ADDR)(ref))+WORDS(sizeof(struct Ref)))

void INITREF(Ref);

extern WORD __GC__Ref[];

typedef int AbsTime;

struct Time {
  WORD *GCINFO;
  Int time;
};

typedef struct Time *Time;

extern WORD __GG__Time[] ;


#define WORDS(bytes)            (((bytes)+sizeof(WORD)-1)/sizeof(WORD))
#define BYTES(words)            ((words)*sizeof(WORD))

#define offsetof(TYPE, MEMBER) __builtin_offsetof (TYPE, MEMBER)

//ADDR gc_new(int);

//#define NEW(t,addr,words)       addr = (t)gc_new(words)


static inline void PROTECT(int state)
{
	/*
	 * WARNING!!!
	 * 	The use of %0 et al. is _very_ fragile and may break at any time.
	 * 	Honestly I don't have a clue why this miscompiles every now and then...
	 */
	int tmp;
	asm volatile(
		"cmp	%1, #0\n"
		"mrs	%0, CPSR\n"
		"orrne	%0, %0, #0x80|0x40\n"
		"biceq	%0, %0, #0x80|0x40\n"
		"msr	CPSR_c, %0\n"
		: "=r" (tmp)
		: "r" (state)
		);
}

static inline int ISPROTECTED(void)
{
	/*
	 * WARNING!!!
	 * 	The use of %0 et al. is _very_ fragile and may break at any time.
	 * 	Honestly I don't have a clue why this miscompiles every now and then...
	 */
	int tmp;
	asm volatile(
			"mrs	%0, CPSR\n"
			"and	%0, %0, #0x80|0x40\n"
			: "=r" (tmp)
			);
	return tmp;
}



#define NEW(t,addr,words)       { int status = ISPROTECTED(); \
                                  PROTECT(1); \
	                              addr = (t)hp; \
	                              hp = (ADDR)addr+(words); \
                                  if (hp >= lim) force(words,(ADDR)addr); \
		                          PROTECT(status); \
                                }


#define CURRENT() current


extern ADDR hp, lim;

ADDR force(WORD, ADDR);
void pruneStaticHeap();

void init_rts(void);

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

#endif
