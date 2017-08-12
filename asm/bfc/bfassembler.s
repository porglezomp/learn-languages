.globl _start
.text

/*

Fixed purpose registers:

fp - Points to the first instruction in the output code, right below
     the loop stack.
ip - Points to the first empty instruction in the output code.
     Grows downwards.
sp - Points to the current location in the BF loop stack.
     Grows upwards.

*/

_start:
	sub sp, #4096
	sub fp, sp, #4
	mov ip, fp

copy_elf_header:
	mov r0, #0
	ldr r2, =ehdr
	mov r3, #88
copy_header_loop:
	ldr r1, [r0, r2]
	str r1, [ip]
	sub ip, #4
	add r0, #4
	cmp r0, r3
	blt copy_header_loop

copy_body:
	mov r0, #0
	ldr r2, =_exit
	mov r3, #12
copy_body_loop:
	ldr r1, [r0, r2]
	str r1, [ip]
	sub ip, #4
	add r0, #4
	cmp r0, r3
	blt copy_body_loop

output_slow:
	mov r1, fp
	mov r2, #4
	mov r7, #4
output_loop:
	mov r0, #1
	svc #0
	sub r1, #4
	cmp r1, ip
	bgt output_loop

_exit:
	mov r0, #0
	mov r7, #1
	svc #0
exitsize = . - _exit

	.align 8
ehdr:	.byte 0x7f, 'E', 'L', 'F', 1, 1, 1, 0
	.space 8
	.hword 2	// Executable
	.hword 0x28	// Targeting ARM
	.word 1		// Version 1 of ELF
	.word 0x0001058	// Address of the entry point
	.word ehdrsize	// Offset to program header table
	.word 0		// Section header table counts (empty)
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
	.word 0x0001000	// Physical address of the current segment
fsize:	.word 100	// Filesize
msize:	.word 100	// Memsize
	.word 5		// Flags
	.word 0x1000	// Align
phdrsize = . - phdr
	.align 8
fullsize = . - ehdr
fsizeoff = fsize - ehdr
msizeoff = msize - ehdr
