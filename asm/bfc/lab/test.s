.global _start
_start:	b exit

add:
	add r0, r0, r0
	add r0, r0, r1
	add r0, r1, r2
	add r1, r2, r3
	add r2, r10, r11
	add r0, r1, #1
	add r0, r1, #0xF
sub:
	sub r1, r2, r3
	sub r1, r2, #0xC
	sub r1, #0xC


branches:
	b A
	b A
	b A
	b A
A:	b A
B:	add r0, #0
	b B

	cmp r4, #0
	beq A

loads:
	ldrb r4, [r3]
	add r4, #1
	strb r4, [r3]

exit:
	mov r0, #0
	mov r7, #1
	svc #0
