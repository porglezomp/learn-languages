.globl _start
.text

FD_stdin	= 0
FD_stdout	= 1

SYS_exit	= 1
SYS_read	= 3
SYS_write	= 4
SYS_lseek	= 19
SYS_fchmod	= 94

SEEK_SET	= 0
SEEK_CUR	= 1
SEEK_END	= 2

INSTR_SIZE	= 4

/*
This program takes a BF program on stdin, and writes an ELF executable to
stdout. (Note that it requires that stdout be seekable.)
*/

_start:
	mov	fp, sp
copy_elf_header:
	adr	r1, prelude
	mov	r2, #prelude_size
	bl	write

get_char:
	// We read characters one-by-one, and pick what code to generate based
	// on that character
	mov	r0, #FD_stdin
	ldr	r1, =buf
	mov	r2, #1
	mov	r7, #SYS_read
	svc	#0
	// Check for EOF
	cmp	r0, #0
	beq	write_exit

	// Mark r1 as un-initialized
	mov	r1, #0
	// Switch on the input character, and set the output buffer and length
	ldr	r0, =buf
	ldrb	r0, [r0]
	cmp	r0, #'>'
	adreq	r1, output_right
	moveq	r2, #output_right_size
	cmp	r0, #'<'
	adreq	r1, output_left
	moveq	r2, #output_left_size
	cmp	r0, #'+'
	adreq	r1, output_plus
	moveq	r2, #output_plus_size
	cmp	r0, #'-'
	adreq	r1, output_minus
	moveq	r2, #output_minus_size
	cmp	r0, #'.'
	adreq	r1, output_write
	moveq	r2, #output_write_size
	cmp	r0, #','
	adreq	r1, output_read
	moveq	r2, #output_read_size

	cmp	r0, #'['
	beq	write_open

	cmp	r0, #']'
	beq	write_close

	// Write the code chunk if it was initialized to a nonzero value
	cmp	r1, #0
	blne	write

	b	get_char

	// Write [ and ] are the most interesting part of this. Since we have
	// to work based on offsets, we aren't able to know where to jump to
	// until we generate later code. This means that we have to backtrack.
	// In order to do this, we tell() the current location in the file, and
	// push that onto the stack. Then, when we are running write_close, we
	// can pop that location off the stack and come back and fix-up the
	// dummy offset that we had originally written.

write_open:
	bl	tell
	push	{r0}

	adr	r1, output_open
	mov	r2, #output_open_size
	bl	write
	svc	#0

	b	get_char

write_close:
	bl	tell
	pop	{r4}
	// If sp > fp, we've popped too many braces and have a syntax error,
	// and also if we don't stop this soon then we can underflow the stack.
	cmp	sp, fp
	bgt	_fail
	// Offset is: (source - target)/4 - 2
	// This is because all branches are word-aligned (4 byte) and have an
	// offset because of decode pipelining, so we have to compensate for
	// that.
	sub	r5, r4, r0
	asr	r5, #2
	sub	r5, #1

	// Write the ] with computed offset
	ldr	r1, =buf
	str	r5, [r1]
	// ea<offset> is the encoding for b
	mov	r6, #0xea
	strb	r6, [r1, #3]
	mov	r2, #INSTR_SIZE
	bl	write

	// Seek to the [ for fixup
	add	r1, r4, #INSTR_SIZE * 2
	mov	r2, #SEEK_SET
	bl	seek

	// (target - source)/4 - 2
	// = -other_offset - 4
	rsb	r5, r5, #0
	sub	r5, #4

	ldr	r1, =buf
	str	r5, [r1]
	// 0a<offset> is the encoding for beq
	mov	r6, #0x0a
	strb	r6, [r1, #3]
	mov	r2, #INSTR_SIZE
	bl	write

	mov	r1, #0
	mov	r2, #SEEK_END
	bl	seek

	b	get_char

write_exit:
	adr	r1, output_exit
	mov	r2, #output_exitsize
	bl	write

adjust_size:
	// Use tell() to get the size of the file
	bl	tell
	ldr	r1, =buf
	str	r0, [r1]

	// Seek to the filesize field of the header
	mov	r1, #fsizeoff
	mov	r2, #SEEK_SET
	bl	seek

	// Write the correct file size twice
	ldr	r1, =buf
	mov	r2, #4
	bl	write
	ldr	r1, =buf
	mov	r2, #4
	bl	write

make_exec:
	mov	r0, #FD_stdout
	mov	r1, #0750
	orr	r1, #0005
	mov	r7, #SYS_fchmod
	svc	#0

_exit:
	// If sp isn't fp, then there are mismatched braces (because there were
	// an unequal number of push/pop calls)
	cmp	sp, fp
	bne	_fail
	// We also copy this exit code into our output programs
output_exit:
	mov	r0, #0
	mov	r7, #SYS_exit
	svc	#0
output_exitsize = . - _exit

_fail:
	// We get here if there's a mismatched brace, so we print an error
	mov	r0, #FD_stdout
	adr	r1, bad_brace_msg
	mov	r2, #bad_brace_msg_size
	mov	r7, #SYS_write
	svc	#0

	mov	r0, #1
	mov	r7, #SYS_exit
	svc	#0


// Syscalls shorthand packing!

write:	mov	r7, #SYS_write
	b	stdout
tell:	// This seeks with no offset, to make lseek return the absolue position
	mov	r1, #0
	mov	r2, #SEEK_CUR
seek:	mov	r7, #SYS_lseek
stdout:
	mov	r0, #FD_stdout
	svc	#0
	mov	pc, lr
	

// NOTE: This code is just to be copied into the output! /////////////////////

output_right:
	add	sp, #1
output_right_size = . - output_right

output_left:
	sub	sp, #1
output_left_size = . - output_left

output_plus:
	ldrsb	r0, [sp]
	add	r0, #1
	strb	r0, [sp]
output_plus_size = . - output_plus

output_minus:
	ldrsb	r0, [sp]
	sub	r0, #1
	strb	r0, [sp]
output_minus_size = . - output_minus

output_read:
	mov	r0, #FD_stdin
	mov	r1, sp
	mov	r2, #1
	mov	r7, #SYS_read
	svc	#0
output_read_size = . - output_read

output_write:
	mov	r0, #FD_stdout
	mov	r1, sp
	mov	r2, #1
	mov	r7, #SYS_write
	svc	#0
output_write_size = . - output_write

output_open:
	ldr	r0, [sp]
	cmp	r0, #0
	beq	0
output_open_size = . - output_open

	.align	4
prelude:
ehdr:	.byte	0x7f, 'E', 'L', 'F', 1, 1, 1, 0
	.space	8		// 08
	.hword	2		// 10 Executable
	.hword	0x28		// 12 Targeting ARM
	.word	1		// 14  Version 1 of ELF
	.word	0x00010054	// 18 Address of the entry point
	.word	ehdrsize	// 1C Offset to program header table
	.word	0		// 20 Section header table counts (empty)
	.word	0		// 24
	.hword	ehdrsize	// 28 The size of the ELF header
	.hword	phdrsize	// 2A The size of the program header
	.hword	1		// 2C Number of entries in the program header
	.hword	0		// 2E These next 3 say no section table
	.hword	0		// 30
	.hword	0		// 32
ehdrsize = . - ehdr
phdr:	.word	1		// 34 Program Type "LOAD"
	.word	0		// 38 No segment offset
	.word	0x00010000	// 3C Virtual address of the current segment
	.word	0x00010000	// 40 Physical address of the current segment
fsize:	.word	0x42424242	// 44 Filesize (TO BE OVERWRITTEN)
msize:	.word	0x42424242	// 48 Memsize  (TO BE OVERWRITTEN)
	.word	5		// 4C Flags
	.word	0x00010000	// 50 Align
phdrsize = . - phdr
fullsize = . - ehdr
fsizeoff = fsize - ehdr
msizeoff = msize - ehdr

output_prelude:
	mov	r4, #0
	// The value 30,000
	sub	sp, #0x7500
	sub	sp, #0x0030
zero_loop:
	// Zero-initialize the whole tape
	push	{r4}
	subs	r5, #4
	bpl	zero_loop
output_prelude_size = . - output_prelude
prelude_size = . - prelude

bad_brace_msg:	.ascii "Bad []s\n"
bad_brace_msg_size = . - bad_brace_msg

.data
buf:	.word	0x0
