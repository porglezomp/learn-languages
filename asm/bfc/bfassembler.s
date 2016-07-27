.globl _start
.text

/*

Fixed purpose registers:

fp - Points to the current location in the output code.
     Grows downwards.
sp - Points to the current location in the BF loop stack.
     Grows upwards.

*/

_start:
	sub sp, #4096
	mov fp, sp
_exit:
	mov r0, #0
	mov r7, #1
	svc #0

ehdr:	.byte 0x7f, 'E', 'L', 'F', 1, 1, 1, 0
	.space 8
	.hword 2	// Executable
	.hword 0x28	// Targeting ARM
	.word 1		// Version 1 of ELF
	.word 0x0001088	// Address of the entry point
	.word 0x34	// Offset to program header table
	.word 0		// Section header table
	.word 0
	.hword ehdrsize	// The size of the ELF header
	.hword phdrsize	// The size of the program header
	.hword 1	// The number of entries in the program header
	.hword 0	// These next 3 say no section table
	.hword 0
	.hword 0
ehdrsize = . - ehdr
phdr:	.word 1		// Program Type "LOAD"
	.word 0		// No segment offset
	.word 0x0001000	// Virtual address of the current segment
	.word 0x0001000	// Physical address of the current segment (???)
fsize:	.word 0		// Filesize
msize:	.word 0		// Memsize
	.word 5		// Flags
	.word 0x1000	// Align
phdrsize = . - phdr
fsizeoff = fsize - ehdr
msizeoff = msize - ehdr
