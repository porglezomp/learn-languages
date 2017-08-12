all: BFC BFA

BFC: bfcompiler.s
	gcc -nostdlib bfcompiler.s -o BFC -Os -Wl,--build-id=none

BFA: bfassembler.s
	gcc -nostdlib bfassembler.s -o BFA -Os -Wl,--build-id=none

strip: BFA BFC
	strip -s -R '.ARM.*' BFC
	strip -s -R '.ARM.*' BFA

clean:
	rm -f BFC
	rm -f BFA
