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
