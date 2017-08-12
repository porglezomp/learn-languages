.globl _start
.text

_start:
printhead:
        mov r0, #1 /* stdout */
        mov r1, #header
        mov r2, #headerlen
        mov r7, #4 /* write */
        svc #0

        sub sp, #4096
        mov fp, sp
        mov r8, #0

read:   mov r0, #0 /* stdin */
        mov r1, fp
        mov r2, #4096
        mov r7, #3 /* read */
        svc #0
        mov r3, r0
        mov r4, #0

main:   /* If we read zero bytes, we're at EOF */
        cmp r3, #0
        beq _exit

        /* Load a byte from the text buffer */
        ldrb r5, [fp, r4]
        /* Switch to decide what instruction to emit (if any) */
        ldr r1, =open1
        cmp r5, #'['
        beq emit_open
        cmp r5, #']'
        beq emit_close
	mov r1, #left
        cmp r5, #'<'
        beq l_op
        cmp r5, #'>'
        beq r_op
        cmp r5, #'+'
        beq inc_op
        cmp r5, #'-'
        beq dec_op
        cmp r5, #','
        beq read_op
        cmp r5, #'.'
        beq write_op
next_char:
        add r4, #1
        cmp r3, r4
        beq read /* If we've hit the end of the current buffer, then read a new one */
        b main

/* r1 contains #left for all of these... */
l_op:	mov r2, #leftlen
        b emit

/* Adding the offset means we don't have to waste space on a constant pool for
 * loading values */
r_op:	add r1, #leftlen
        mov r2, #rightlen
        b emit

inc_op: add r1, #(leftlen + rightlen)
        mov r2, #incrlen
        b emit

dec_op: add r1, #(leftlen + rightlen + incrlen)
        mov r2, #decrlen
        b emit

write_op:
        add r1, #(leftlen + rightlen + incrlen + decrlen)
        mov r2, #writeoplen
	b emit

read_op:
	add r1, #(leftlen + rightlen + incrlen + decrlen + writeoplen)
        mov r2, #readoplen
emit:   mov r0, #1
      	mov r7, #4
        svc #0
        b next_char

/* r1 is #open1 here */
emit_close:
        pop {r8}
	mov r5, #1
	mov r6, #3
	add r1, #3
	b emit_brace
emit_open:
	mov r6, #open2len
emit_brace:
	mov r7, #4
        mov r0, #1
        mov r2, #3
        svc #0

        bl printsym

        mov r0, #1
        add r1, #3
        mov r2, r6
        svc #0

        bl printsym

        mov r0, #1
        add r1, #(open2len + 3)
        mov r2, #3
        svc #0

	cmp r5, #1
        pushne {r8}
        addne r8, #1
        b next_char

printsym:
        mov r2, #1
        mov r10, #28
symloop:mov r1, r8
        asr r1, r10
        and r1, #0xF
        add r1, #'A'
        mov r0, #1
        svc #0
        sub r10, #4
        cmp r10, #-4
        bne symloop

        mov pc, lr

_exit:
        mov r0, #1
	// r1 will always contain #left, and #(left - footerlen) = #footer
        sub r1, #footerlen
        mov r2, #footerlen
        mov r7, #4
        svc #0

        mov r0, #0
        mov r7, #1
        svc #0

header: .ascii ".globl _start\n.text\n_start:\nldr r3, =40000\nsub r3, sp, r3\n"
headerlen = . - header
.align 1
footer: .ascii "mov r0, #0\nmov r7, #1\nsvc #0\n"
footerlen = . - footer
// We need alignment so that #left is 8-bits shifted
.align 1
left: .ascii "sub r3, #1\n"
leftlen = . - left
right: .ascii "add r3, #1\n"
rightlen = . - right
incr: .ascii "ldrb r4, [r3]\nadd r4, #1\nstrb r4, [r3]\n"
incrlen = . - incr
decr: .ascii "ldrb r4, [r3]\nsub r4, #1\nstrb r4, [r3]\n"
decrlen = . - decr
writeop: .ascii "mov r0, #1\nmov r1, r3\nmov r2, #1\nmov r7, #4\nsvc #0\n"
writeoplen = . - writeop
readop: .ascii "mov r0, #0\nmov r1, r3\nmov r2, #1\nmov r7, #3\nsvc #0\n"
readoplen = . - readop
close1: .ascii "b b"
open1: .ascii "  b"
close2: .ascii "\ne"
open2: .ascii ":\nldrb r4, [r3]\ncmp r4, #0\nbeq e"
open2len = . - open2
close3: .ascii ":\n"
open3: .ascii "\n "
