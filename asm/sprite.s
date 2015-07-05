	.text
	.global main
	.syntax unified
	
main:	push {lr}
	bl  easyfb_init
	ldr r1, =fbi_addr
	str r0, [r1]

	bl  clear
	
	ldr r0, =sprite
	mov r1, 32
	mov r2, 32
	bl  draw_sprite

	ldr r0, =sprite_2
	mov r1, 54
	mov r2, 43
	bl  draw_sprite	

wait:	mov r0, 1
	bl  sleep

return:	ldr r0, =fbi_addr
	ldr r0, [r0]
	bl  easyfb_quit

	mov r0, 0		@ return 0
	pop {pc}

clear:
	push {r4, r5, r6}
	ldr r0, =fbi_addr
	ldr r0, [r0]
	screen .req r5
	stride .req r4
	height .req r3
	width .req r2
	ldr screen, [r0, 20]
	ldr stride, [r0, 8]
	ldr height, [r0, 4]
	ldr width, [r0, 0]
	row .req r0
	col .req r1
	mov row, 0
	mov r6, 0
	
clear_loop:
	mov col, 0
	cmp row, height
	bge clear_end
	mul r12, row, stride
clear_row:
	cmp col, width
	addgt row, 1
	bgt clear_loop

	strd r6, [screen, r12]

	@@ add 8 to the byte offset but 4 to the pixel offset
	@@ because pixels are two bytes
	add r12, 8	
	add col, 4
	b   clear_row

clear_end:
	.unreq row
	.unreq col
	.unreq screen
	.unreq stride
	.unreq width
	.unreq height
	pop {r4, r5, r6}
	bx  lr
	
draw_sprite:
	@@ r0 = sprite info address
	@@ r1 = x position
	@@ r2 = y position
	push {r4, r5, r6, r7, r8}
	info .req r0
	xpos .req r1
	ypos .req r2
	pixel .req r3
	texel .req r4
	fbi .req r5
	rx .req r6
	ry .req r7
	stride .req r8
	base .req r12

	ldr fbi, =fbi_addr
	ldr fbi, [fbi]
	ldr base, [fbi, 20]
	ldr stride, [fbi, 8]
	mov ry, 0
loop:
	t0 .req r3
	ldrb t0, [r0, 1]	@ vsiz
	cmp ry, t0		@ ry < t0
	bge loop_end
	mov rx, 0
row:	@@ r3 is scratch for retriving the sprite height
	ldrb t0, [r0]		@ hsiz
	cmp rx, t0
	addge ry, 1		@ if rx >= height
	bge loop		@ increment and next row
	.unreq t0

	@@ Calculate the screen pixel index
	t0 .req r3
	t1 .req r4
	add t0, xpos, rx
	lsl t0, t0, 1		@ double t0 because pixels are 2 bytes wide
	add t1, ypos, ry
	mul t1, stride
	add pixel, t0, t1	@ index = y * stride + x * 2
	.unreq t0
	.unreq t1

	@@ Calculate the sprite pixel index
	t0 .req r4
	sp_stride .req r4
	ldrb sp_stride, [r0, 2]	@ stride
	@@ t0    = ry * sp_stride + rx
	@@ pixel =  y *    stride +  x
	mla t0, ry, sp_stride, rx
	@@ we offset t0 by 3 because that's how much the image data image is
	@@ offset from r0, and we don't want to waste a register adding 3 to r0
	add t0, 3
	ldrb t0, [r0, t0]
	orr texel, t0, t0, LSL 8 @ fill out the pixel to two bytes
	.unreq sp_stride
	.unreq t0
	
	strh texel, [base, pixel]

	add rx, 1
	b   row

loop_end:
	pop {r4, r5, r6, r7, r8}
	bx  lr
	.unreq info
	.unreq xpos
	.unreq ypos
	.unreq pixel
	.unreq texel
	.unreq fbi
	.unreq rx
	.unreq ry
	.unreq stride
	.unreq base

	@@ Image data
	.data
sprite:
	.byte 5			@ hsiz
	.byte 5			@ vsiz
	.byte 5			@ stride

	@ image
	.byte 0x00, 0xFF, 0xFF, 0xFF, 0x00
	.byte 0xFF, 0x00, 0x00, 0x00, 0xFF
	.byte 0xFF, 0x00, 0xFF, 0x00, 0xFF
	.byte 0xFF, 0x00, 0x00, 0x00, 0xFF
	.byte 0x00, 0xFF, 0xFF, 0xFF, 0x00

sprite_2:
	.byte 6			@ hsiz
	.byte 6			@ vsiz
	.byte 6			@ stride

	@ image
	.byte 0xFF, 0x00, 0x00, 0x00, 0x00, 0xFF
	.byte 0x00, 0xFF, 0x00, 0x00, 0xFF, 0x00
	.byte 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00
	.byte 0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00
	.byte 0x00, 0xFF, 0x00, 0x00, 0xFF, 0x00
	.byte 0xFF, 0x00, 0x00, 0x00, 0x00, 0xFF


fbi_addr:
	.word 0

