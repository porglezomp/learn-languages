	.arch armv6
	.eabi_attribute 27, 3
	.eabi_attribute 28, 1
	.fpu vfp
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 2
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4
	.file	"easyfb.c"
	.text

	.align	2
	.global	easyfb_init
	.type	easyfb_init, %function
easyfb_init:
	@ args = 0, pretend = 0, frame = 232
	@ frame_needed = 0, uses_anonymous_args = 0
	stmfd	sp!, {r4, r5, lr}
	mov	r0, #36
	sub	sp, sp, #244	@ reserve stack space
	bl	malloc		
	subs	r4, r0, #0	@ move malloc result and compare == 0
	beq	.L9		@ goto fail
	ldr	r0, .L12
	mov	r1, #2
	bl	open
	cmn	r0, #1
	str	r0, [r4, #16]
	beq	.L4		@ goto free_info
	ldr	r1, .L12+4
	add	r2, sp, #12
	bl	ioctl
	cmp	r0, #0
	blt	.L4		@ goto free_info
	add	r2, sp, #80
	ldr	r0, [r4, #16]
	mov	r1, #17920
	bl	ioctl
	cmp	r0, #0		@ goto free_info
	blt	.L4
	ldr	r0, [sp, #84]
	ldr	lr, [sp, #80]
	ldr	r5, [sp, #56]
	mov	ip, #512
	mov	r3, #320
	mov	r1, #2
	str	r0, [r4, #32]
	str	r1, [r4, #12]
	add	r2, sp, #80
	str	r5, [r4, #8]
	str	lr, [r4, #28]
	str	ip, [r4]
	str	r3, [r4, #4]
	ldr	r0, [r4, #16]
	ldr	r1, .L12+8
	str	ip, [sp, #80]
	str	r3, [sp, #84]
	bl	ioctl
	cmp	r0, #0
	blt	.L4		@ goto free_info
	ldr	r3, [r4, #16]
	ldr	r1, [sp, #32]
	mov	r0, #0
	str	r1, [r4, #24]
	mov	r2, #3
	str	r3, [sp]
	str	r0, [sp, #4]
	mov	r3, #1
	bl	mmap
	cmn	r0, #1
	str	r0, [r4, #20]
	movne	r0, r4
	beq	.L11
	add	sp, sp, #244
	@ sp needed
	ldmfd	sp!, {r4, r5, pc}
.L11:
.L6:
	ldr	r0, [r4, #16]
	bl	close
.L4:
	mov	r0, r4
	bl	free
.L9:
	mov	r0, #0
	add	sp, sp, #244
	@ sp needed
	ldmfd	sp!, {r4, r5, pc}
.L13:
	.align	2
.L12:
	.word	.LC0
	.word	17922
	.word	17921
	.size	easyfb_init, .-easyfb_init

	.align	2
	.global	easyfb_quit
	.type	easyfb_quit, %function
easyfb_quit:
	@ args = 0, pretend = 0, frame = 160
	@ frame_needed = 0, uses_anonymous_args = 0
	stmfd	sp!, {r4, lr}
	mov	r4, r0
	sub	sp, sp, #160
	ldr	r1, [r4, #24]
	ldr	r0, [r0, #20]
	bl	munmap
	mov	r2, sp
	ldr	r0, [r4, #16]
	mov	r1, #17920
	bl	ioctl
	ldr	ip, [r4, #28]
	ldr	r3, [r4, #32]
	mov	r2, sp
	ldr	r1, .L15
	ldr	r0, [r4, #16]
	str	ip, [sp]
	str	r3, [sp, #4]
	bl	ioctl
	ldr	r0, [r4, #16]
	bl	close
	mov	r0, r4
	add	sp, sp, #160
	@ sp needed
	ldmfd	sp!, {r4, lr}
	b	free
.L16:
	.align	2
.L15:
	.word	17921
	.size	easyfb_quit, .-easyfb_quit

	.align	2
	.global	sample_offsets
	.type	sample_offsets, %function
sample_offsets:
	@ args = 0, pretend = 0, frame = 0
	@ frame_needed = 0, uses_anonymous_args = 0
	@ link register save eliminated.
	stmfd	sp!, {r4, r5, r6}
	mov	ip, #3
	mov	r6, #0
	mov	r5, #1
	mov	r4, #2
	mov	r1, #4
	mov	r2, #5
	mov	r3, #6
	str	r6, [r0]	@ width
	str	r5, [r0, #4]	@ height
	str	r4, [r0, #8]	@ stride
	str	ip, [r0, #12]	@ bytes_pp
	str	r1, [r0, #16]	@ fbdev
	str	r2, [r0, #20]	@ buffer
	str	r3, [r0, #24]	@ memsize
	ldmfd	sp!, {r4, r5, r6}
	bx	lr
	.size	sample_offsets, .-sample_offsets
	.section	.rodata.str1.4,"aMS",%progbits,1

	.align	2
.LC0:
	.ascii	"/dev/fb0\000"
	.ident	"GCC: (Raspbian 4.8.2-21~rpi3rpi1) 4.8.2"
	.section	.note.GNU-stack,"",%progbits
