all:
	../bin/dasm *.asm -f3 -v0 -Lcart.lst -scart.sym -ocart.bin


verbose:
	../bin/dasm *.asm -f3 -v1 -ocart.bin
