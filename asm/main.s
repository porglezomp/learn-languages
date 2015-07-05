	.text
	.globl _start

_start:
	ldr r0, =0xdeadbeef
	bl  print_word
	bl  newline
	
	ldr r0, =0xdeadbeef	
	bl  print_half
	bl  newline

	ldr r0, =0xdeadbeef
	bl  print_byte
	bl  newline
	
	ldr r0, =0xdeadbeef
	bl  print_nibble
	bl  newline
	
	b   exit

exit:	
	mov r0, #0
	mov r7, #1
	swi #0
