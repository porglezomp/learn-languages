all: BFC BFA

BFC: bfcompiler.s
	gcc -nostdlib bfcompiler.s -o BFC -Os

BFA: bfassembler.s
	gcc -nostdlib bfassembler.s -o BFA -Os

strip: BFA BFC
	strip -s -R .ARM.attributes -R .note.gnu.build-id BFC
	strip -s -R .ARM.attributes -R .note.gnu.build-id BFA

clean:
	rm -f BFC
	rm -f BFA
