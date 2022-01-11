#32X Init 00

Compiling a working 32X rom. This demo sets up three counters, one for each of the SH2 CPUs, and one for the 68000 CPU. They increment values at $06000800, $06000810, and $ff0000 in RAM respectively.

----

#### Compiling SH2 Code With ASMSH ####
asmsh.exe  /i /o #+ /o psh2 /o w+ /p [input].asm, [output].bin

#### Compiling 68K Code With ASM68K ####
asm68k.exe /o op+ /o os+ /o ow+ /o oz+ /o oaq+ /o osq+ /o omq+ /p /o ae- [input].asm, [output].bin

#### Debugging ROM With MAME ####
mame64.exe -verbose -debug -nofilter -waitvsync -window genesis -cart [path/to/compiledROM.bin]