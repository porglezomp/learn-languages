	.text
	.globl print_nibble
	.globl print_byte
	.globl print_half
	.globl print_word
	.globl newline
	
print_nibble:
	mov r3, r7 		@ We can save with registers!
				@ Calling convention requires saving r4+
	and r0, r0, #15		@ Only print the first 4 bits
	ldr r1, =hexdigits
	add r1, r0, r1

	mov r0, #0 		@ ??? What does this do?
	mov r2, #1
	mov r7, #4
	swi #0

	mov r7, r3
	bx  lr

print_byte:
	push {r4, lr}
	mov r4, r0
	
	lsr r0, r4, #4		@ Byte 2
	bl print_nibble
	mov r0, r4
	bl print_nibble
	
	pop {r4, pc}

print_half:	
	push {r4, lr}
	mov r4, r0

	lsr r0, r4, #12		@ Byte 1
	bl print_nibble
	lsr r0, r4, #8
	bl print_nibble
	lsr r0, r4, #4		@ Byte 2
	bl print_nibble
	mov r0, r4
	bl print_nibble

	pop {r4, pc}
	
print_word:
	push {r4, lr}
	mov r4, r0

	lsr r0, r4, #28		@ Byte 1
	bl print_nibble
	lsr r0, r4, #24
	bl print_nibble
	lsr r0, r4, #20		@ Byte 2
	bl print_nibble
	lsr r0, r4, #16
	bl print_nibble
	lsr r0, r4, #12		@ Byte 3
	bl print_nibble
	lsr r0, r4, #8
	bl print_nibble
	lsr r0, r4, #4		@ Byte 4
	bl print_nibble
	mov r0, r4
	bl print_nibble
	
	pop {r4, pc}
	
newline:
	mov r3, r7
	
	mov r0, #0
	ldr r1, =newline_char
	mov r2, #1
	mov r7, #4
	swi #0

	mov r7, r3
	bx  lr

	.data
hexdigits:
	.ascii "0123456789ABCDEF"
newline_char:
	.ascii "\n"
