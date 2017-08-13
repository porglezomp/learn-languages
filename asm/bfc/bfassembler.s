.globl _start
.text

SYS_read	= 3
SYS_write	= 4

_start:
copy_elf_header:
	mov	r0, #1
	ldr	r1, =ehdr
	mov	r2, #fullsize
	mov	r7, #SYS_write
	svc	#0

	mov	r0, #1
	ldr	r1, =_exit
	mov	r2, #exitsize
	svc	#0

_exit:
	mov	r0, #0
	mov	r7, #1
	svc	#0
exitsize = . - _exit

	.align	4
ehdr:	.byte	0x7f, 'E', 'L', 'F', 1, 1, 1, 0
	.space	8		// 08
	.hword	2		// 10 Executable
	.hword	0x28		// 12 Targeting ARM
	.word	1		// 14  Version 1 of ELF
	.word	0x00100054	// 18 Address of the entry point
	.word	ehdrsize	// 1C Offset to program header table
	.word	0		// 20 Section header table counts (empty)
	.word	0		// 24
	.hword	ehdrsize	// 28 The size of the ELF header
	.hword	phdrsize	// 2A The size of the program header
	.hword	1		// 2C The number of entries in the program header
	.hword	0		// 2E These next 3 say no section table
	.hword	0		// 30
	.hword	0		// 32
ehdrsize = . - ehdr
phdr:	.word	1		// 34 Program Type "LOAD"
	.word	0		// 38 No segment offset
	.word	0x00100000	// 3C Virtual address of the current segment
	.word	0x00100000	// 40 Physical address of the current segment
fsize:	.word	0x60		// 44 Filesize (TO BE OVERWRITTEN)
msize:	.word	0x60		// 48 Memsize  (TO BE OVERWRITTEN)
	.word	5		// 4C Flags
	.word	0x00100000	// 50 Align
phdrsize = . - phdr
fullsize = . - ehdr
fsizeoff = fsize - ehdr
msizeoff = msize - ehdr
