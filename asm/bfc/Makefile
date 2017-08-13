all: BFC BFA

ASM=gcc -nostdlib -Os -Wl,--build-id=none
STRIP=strip -s -R '.ARM.*'

BFC: bfcompiler.s
	$(ASM) bfcompiler.s -o BFC

BFA: bfassembler.s
	$(ASM) bfassembler.s -o BFA

lab: .PHONY
	$(ASM) lab/test.s -o lab/test.out
	$(ASM) lab/smol1.s -o lab/smol1.out
	$(ASM) lab/smol2.s -o lab/smol2.out
	$(STRIP) lab/test.out
	$(STRIP) lab/smol1.out
	$(STRIP) lab/smol2.out

strip: BFA BFC
	$(STRIP) BFC
	$(STRIP) BFA

clean:
	rm -f BFC
	rm -f BFA

.PHONY:
