/* Set the entry point, warning if we mess up. */
ENTRY(__vec_reset);

/* Setup the memory layout. */
MEMORY
{
	sram		: 	ORIGIN = 0x40000000, 	LENGTH = 64k
	ram		:	ORIGIN = 0xA0000000,	LENGTH = 32M
}

/* Provide some information about where some areas start and end. */
__ram_start   = 0xA0000000;
__ram_end     = 0xA4000000;
__sram_start = 0x40000000;

/* The sections of the object file. */
SECTIONS
{
	/* Start at flash. */
	. = 0;

	/* Text section. */
	.text :
	{
		/* This is where the data starts. */
		__vectors_start = .;
		*(.vectors);
		__vectors_end = .;
		*(.text);
		*(.rodata);
		*(.rodata*);
		*(.glue_7);
		*(.glue_7t);
		. = ALIGN(4);
	} > ram

	.data :
	{
		*(.data);
		. = ALIGN(4);
	} > ram

	/* 
	 * BSS section, intialized to zero. This doesn't strictly have to be
	 * before the .text section but let's keep the RAM stuff together.
	 */
	.bss :
	{
		__bss_start = .;
		*(.bss);
		. = ALIGN(4);
		*(COMMON);
		. = ALIGN(4);
		__bss_end = .;
	} > ram

	/* Provide and _end variable, usefull for malloc & co. */
	_end = ADDR(.bss) + SIZEOF(.bss);



}
