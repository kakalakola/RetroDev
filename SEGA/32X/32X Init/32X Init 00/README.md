#32X Init 00

Compiling a working 32X rom.

----

#### Compiling SH2 Code With ASMSH ####
asmsh.exe  /i /o #+ /o psh2 /o w+ /p [input].asm, [output].bin

#### Compiling 68K Code With ASM68K ####
asm68k.exe /o op+ /o os+ /o ow+ /o oz+ /o oaq+ /o osq+ /o omq+ /p /o ae- [input].asm, [output].bin

#### Debugging ROM With MAME ####
mame64.exe -verbose -debug -nofilter -waitvsync -window genesis -cart [path/to/compiledROM.bin]