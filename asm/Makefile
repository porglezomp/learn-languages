all: BFC Images

BFC: bfcompiler.s
	gcc -nostdlib bfcompiler.s -o BFC -Os

strip:
	strip -s BFC
	strip -R .ARM.attributes BFC
	strip -R .note.gnu.build-id BFC

Images: images.s
	gcc -nostdlib images.s -o Images

clean:
	rm -f BFC
	rm -f Images
