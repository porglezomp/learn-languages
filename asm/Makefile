BFC: bfcompiler.s
	gcc -nostdlib bfcompiler.s -o BFC -Os

strip:
	strip -s BFC
	strip -R .ARM.attributes BFC
	strip -R .note.gnu.build-id BFC
