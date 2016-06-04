.globl _start
.text

_start:
printhead:
        mov r0, #1 /* stdout */
        ldr r1, =header
        ldr r2, =headerlen
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
        cmp r5, #'['
        beq emit_open
        cmp r5, #']'
        beq emit_close
next_char:
        add r4, #1
        cmp r3, r4
        beq read /* If we've hit the end of the current buffer, then read a new one */
        b main

l_op:   ldr r1, =left
        ldr r2, =leftlen
        b emit

r_op:   ldr r1, =right
        ldr r2, =rightlen
        b emit

inc_op: ldr r1, =incr
        ldr r2, =incrlen
        b emit

dec_op: ldr r1, =decr
        ldr r2, =decrlen
        b emit

read_op:
        ldr r1, =readop
        ldr r2, =readoplen
        b emit

write_op:
        ldr r1, =writeop
        ldr r2, =writeoplen
emit:   mov r0, #1
      	mov r7, #4
        svc #0
        b next_char

emit_open:
	      mov r7, #4
        mov r0, #1
        ldr r1, =open1
        ldr r2, =open1len
        svc #0

        bl printhex

        mov r0, #1
        ldr r1, =open2
        ldr r2, =open2len
        svc #0

        bl printhex

        mov r0, #1
        ldr r1, =open3
        ldr r2, =open3len
        svc #0

        push {r8}
        add r8, #1
        b next_char

emit_close:
        pop {r8}

	      mov r7, #4
        mov r0, #1
        ldr r1, =close1
        ldr r2, =close1len
        svc #0

        bl printhex

        mov r0, #1
        ldr r1, =close2
        ldr r2, =close2len
        svc #0

        bl printhex

        mov r0, #1
        ldr r1, =close3
        ldr r2, =close3len
        svc #0

        b next_char

printhex:
        mov r2, #1
        mov r10, #28
hexloop:mov r9, r8
        asr r9, r10
        and r9, #0xF
        ldr r1, =hexdigits
        add r1, r9
        mov r0, #1
        svc #0
        sub r10, #4
        cmp r10, #-4
        bne hexloop

        mov pc, lr

_exit:
        mov r0, #1
        ldr r1, =footer
        ldr r2, =footerlen
        mov r7, #4
        svc #0

        mov r0, #0
        mov r7, #1
        svc #0

header: .ascii ".globl _start\n.text\n_start:\nldr r3, =tape\n"
headerlen = . - header
footer: .ascii "mov r0, #0\nmov r7, #1\nsvc #0\n.data\ntape: .space 40000\n"
footerlen = . - footer
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
hexdigits: .ascii "0123456789ABCDEF"
open1: .ascii "begin_"
open1len = . - open1
open2: .ascii ":\nldrb r4, [r3]\ncmp r4, #0\nbeq end_"
open2len = . - open2
open3: .ascii "\n"
open3len = . - open3
close1: .ascii "b begin_"
close1len = . - close1
close2: .ascii "\nend_"
close2len = . - close2
close3: .ascii ":\n"
close3len = . - close3
