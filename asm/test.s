	.text
	.global main
main:
	bl  easyfb_init
	@ the address of the framebuffer struct is now in r0
	mov r4, r0

	width .req r8
	height .req r7
	stride .req r6
	buf .req r2
	
	ldr width, [r0]
	lsl width, width, #1
	ldr height, [r0, #4]
	ldr stride, [r0, #8]
	ldr buf, [r0, #20]

	row .req r3
	mov row, #0
.each_line:
	col .req r5
	mov col, #0
	@ Generate the pixel index
	id .req r9
	mul id, r3, r6
.each_pixel:
	and r1, col, #0x00ff
	orr r1, row, lsl #8
	strh r1, [buf, id]

	add id, id, #2
	add col, col, #2
	
	cmp col, width
	blt .each_pixel

	add row, row, #1
	cmp row, height
	blt .each_line

	.unreq row
	.unreq col
	.unreq stride
	.unreq width
	.unreq height
	.unreq id
	
out:	
	mov r0, #3
	bl  sleep
	
	mov r0, r4
	bl  easyfb_quit
	
	mov r0, #0
	mov r7, #1
	swi #0
