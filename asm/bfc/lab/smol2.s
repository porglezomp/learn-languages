.globl _start
_start:
	mov	r0, #1
	mov	r1, #1
	add	r2, r0, r1
_end:
	mov	r0, #0
	mov	r7, #1
	svc	#0
