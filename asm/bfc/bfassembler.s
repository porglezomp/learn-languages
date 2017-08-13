.globl _start
.text

FD_stdin	= 0
FD_stdout	= 1

SYS_exit	= 1
SYS_read	= 3
SYS_write	= 4
SYS_lseek	= 19

SEEK_SET	= 0
SEEK_CUR	= 1
SEEK_END	= 2

INSTR_SIZE	= 4

_start:
	mov	fp, sp
copy_elf_header:
	mov	r0, #FD_stdout
	adr	r1, ehdr
	mov	r2, #fullsize
	mov	r7, #SYS_write
	svc	#0

get_char:
	// Read a single character
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
	// Switch on the input character
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

	// Write the code if
	cmp	r1, #0
	movne	r0, #FD_stdout
	movne	r7, #SYS_write
	svcne	#0

	b	get_char

write_open:
	bl	tell
	push	{r0}

	mov	r0, #FD_stdout
	adr	r1, output_open
	mov	r2, #output_open_size
	mov	r7, #SYS_write
	svc	#0

	b	get_char

write_close:
	bl	tell
	pop	{r4}
	cmp	sp, fp
	bgt	_fail
	// (source - target - 8) / 4
	sub	r5, r4, r0
	asr	r5, #2
	sub	r5, #1

	// Write the ] with computed offset
	mov	r0, #FD_stdout
	ldr	r1, =buf
	str	r5, [r1]
	// ea<offset> is the encoding for b
	mov	r6, #0xea
	strb	r6, [r1, #3]
	mov	r2, #INSTR_SIZE
	mov	r7, #SYS_write
	svc	#0

	// Seek to the [ for fixup
	mov	r0, #FD_stdout
	add	r1, r4, #INSTR_SIZE * 2
	mov	r2, #SEEK_SET
	mov	r7, #SYS_lseek
	svc	#0

	// (target - source - 8) / 4
	rsb	r5, r5, #0
	sub	r5, #4

	mov	r0, #FD_stdout
	ldr	r1, =buf
	str	r5, [r1]
	// 0a<offset> is the encoding for beq
	mov	r6, #0x0a
	strb	r6, [r1, #3]
	mov	r2, #INSTR_SIZE
	mov	r7, #SYS_write
	svc	#0

	mov	r0, #FD_stdout
	mov	r1, #0
	mov	r2, #SEEK_END
	mov	r7, #SYS_lseek
	svc	#0

	b	get_char

write_exit:
	mov	r0, #FD_stdout
	adr	r1, output_exit
	mov	r2, #output_exitsize
	mov	r7, #SYS_write
	svc	#0

adjust_size:
	// Use lseek to get the size of the file
	bl	tell
	ldr	r1, =buf
	str	r0, [r1]

	// Seek to the filesize field of the header
	mov	r0, #FD_stdout
	mov	r1, #fsizeoff
	mov	r2, #SEEK_SET
	mov	r7, #SYS_lseek
	svc	#0

	// Write the correct file size twice
	mov	r0, #FD_stdout
	ldr	r1, =buf
	mov	r2, #4
	mov	r7, #SYS_write
	svc	#0
	mov	r0, #FD_stdout
	ldr	r1, =buf
	mov	r2, #4
	svc	#0

_exit:
	cmp	sp, fp
	bne	_fail
output_exit:
	mov	r0, #0
	mov	r7, #SYS_exit
	svc	#0
output_exitsize = . - _exit

_fail:
	mov	r0, #FD_stdout
	adr	r1, bad_brace_msg
	mov	r2, #bad_brace_msg_size
	mov	r7, #SYS_write
	svc	#0

	mov	r0, #1
	mov	r7, #SYS_exit
	svc	#0

tell:
	mov	r0, #FD_stdout
	mov	r1, #0
	mov	r2, #SEEK_CUR
	mov	r7, #SYS_lseek
	svc	#0
	mov	pc, lr
	

// NOTE: This code is just to be copied into the output! /////////////////////

output_header:
	// Reserve 30,000 bytes
	sub	sp, #0x7500
	sub	sp, #0x0030
output_header_size = . - output_header

output_right:
	add	sp, #1
output_right_size = . - output_right

output_left:
	sub	sp, #0
output_left_size = . - output_left

output_plus:
	ldrb	r0, [sp]
	add	r0, #1
	strb	r0, [sp]
output_plus_size = . - output_plus

output_minus:
	ldrb	r0, [sp]
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

bad_brace_msg:	.ascii "Bad []s\n"
bad_brace_msg_size = . - bad_brace_msg

.data
buf:	.word	0x0
