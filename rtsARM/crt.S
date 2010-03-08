/*
 * Copyright (c) 2007, Per Lindgren, Johan Eriksson, Johan Nordlander,
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


/*
 * Various ARM modes, makes life easier.
 */
.set ARM_USER,			0x10
.set ARM_FIQ,			0x11
.set ARM_IRQ,			0x12
.set ARM_SUPERVISOR,	0x13
.set ARM_ABORT,			0x17
.set ARM_UNDEF,			0x1B
.set ARM_SYSTEM,		0x1F

.set MEMMAP, 0xE01FC040

/*
 * The stacksizes for the various modes. The modes not listed
 * here is USER and SYSTEM. This is simply because they share
 * the same stack and we will be using the remaining stack
 * memory for them.
 */
.set ARM_FIQ_STACK_SIZE,		256
.set ARM_IRQ_STACK_SIZE,		256
.set ARM_SUPERVISOR_STACK_SIZE,	256
.set ARM_UNDEF_STACK_SIZE,		32
.set ARM_ABORT_STACK_SIZE,		32

/*
 * Interrupt flags, fast an normal interrupt request.
 */
.set I_BIT, 0x80
.set F_BIT, 0x40

/* 
 * External functions that we rely upon. This is mostly the C interrupt
 * routeines. 
 */
.extern main
.extern swi_handler
.extern irq_handler


/*
 * The external functions of this, should only be the entry point ie.
 * the reset vector.
 */
.global __vec_reset
.global __vec_default

.arm
.text
.align

/* 
 * The Interrupt vectors.
 *
 * IMPORTANT:
 *
 *	The reason we do not simply branch to __init is that we want to be running
 *	from the "real" address of the rom, not the adress mapped at 0. This is
 *	simply because the compiler generates code that isn't position independant.
 *	Yes I know this is a ugly hack but it's easier this way ;) Don't mess with
 *	the offset unless you know what you are doing... 
 *	
 *	The only interrupt vector that will be running code from the flash is the
 *	reset vector and that is because we do not really need the speed when
 *	doing the initialization.
 */
.pushsection .vectors,"ax"
__vec_reset:			ldr	pc, =__init
__vec_undef:			b __vec_undef
__vec_swi:				ldr	pc, =swi_handler
__vec_code_abort:		b __vec_code_abort
__vec_data_abort:		b __vec_data_abort
__vec_reserved:			nop
__vec_irq:				ldr	pc,	=irq_handler
__vec_fiq:				b __vec_fiq
.popsection

/*
 * This code is always run upon reset. It should land the CPU in
 * a somewhat usable state.
 *
 * It should do the following:
 *
 *	- The vector table is copied to offset 0x00.
 *
 *	- The interrupt stacks are initialized.
 *
 *  - The regular program stack is initialized.
 *	
 *	- Copies any data from __data_start to __data_end into the ram after
 *	  the vector rable.
 *
 *	- The memory region from __bss_start to __bss_end is initialized to 0.
 *
 *	- Branch to main().
 *
 */
__init:

	/* 
	 * Stack always starts at the end of the ram and grows downwards.
	 * __ram_end points to the first byte of the last 4 byte word.
	 */
	ldr		r0, =__ram_end

	/* Undefined instruction mode. */
	msr		CPSR_c, #I_BIT|F_BIT|ARM_UNDEF
	mov		sp, r0
	sub		r0, r0, #ARM_UNDEF_STACK_SIZE

	/* Abort mode. */
	msr		CPSR_c, #I_BIT|F_BIT|ARM_ABORT
	mov		sp, r0
	sub		r0, r0, #ARM_ABORT_STACK_SIZE

	/* Fast interrupt mode. */
	msr		CPSR_c, #I_BIT|F_BIT|ARM_FIQ
	mov		sp, r0
	sub		r0, r0, #ARM_FIQ_STACK_SIZE

	/* Interrrupt mode. */
	msr		CPSR_c, #I_BIT|F_BIT|ARM_IRQ
	mov		sp, r0
	sub		r0, r0, #ARM_IRQ_STACK_SIZE

	/* Supervisor mode. */
	msr		CPSR_c, #I_BIT|F_BIT|ARM_SUPERVISOR
	mov		sp, r0
	sub		r0, r0, #ARM_SUPERVISOR_STACK_SIZE

	/* System mode, this stack is shared with the user mode. */
	msr		CPSR_c, #I_BIT|F_BIT|ARM_SYSTEM
	mov		sp, r0

	/* 
	 * This is just a slight sanity check so we don't mess things up. We can't
	 * really do that much output right now but let's hang the execution so
	 * that we can check this with a debugger at least.
	 */
	ldr		r0, =_end
	cmp		sp, r0
.Lstack_overflow:
	blo		.Lstack_overflow

	/* Zero out .bss. */
	mov		r0, #0
	ldr		r1,	=__bss_start
	ldr		r2, =__bss_end
.Lbss_zero:
	cmp		r1, r2
	strlo	r0, [r1], #4
	blo		.Lbss_zero

        /* Copy any .data into ram. */
        ldr             r1, =__sram_start
        ldr             r2, =__vectors_start
        ldr             r3, =__vectors_end

.Lvectors_copy:
        cmp             r2, r3
        ldrlo   r0, [r2], #4
        strlo   r0, [r1], #4
        blo             .Lvectors_copy

	/*
	 * REMAP INTVECTORS to external memory
	 */
	mov		r0, #0x02
	ldr		r1, =MEMMAP
	str		r0, [r1]

	/*
	 * All done, let's branch to main. The CPU is not yet initialized but this
	 * is the problem of the main() function. It has to setup the PLL and any
	 * peripheral devices.
	 *
	 * NOTE:
	 *	While the right thing would be to enter user mode here we are on a
	 *	small embedded system so let's stay in system mode.
	 */
	ldr		r0, =main
	bx		r0

.Lhang:
	b		.Lhang
