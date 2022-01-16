# 32X Init 01 #

Timing things with VBlanks. Nothing too fancy.

This demo sets up seven(!) counters.
  - Master SH2, 68000, and Z80 have a counter incrementing in a loop, and a counter incrementing every VBlank each
  - Slave SH2 has a a counter incrementing in a loop

----

#### Compiling Z80 Code With TNIASM ####
tniasm.exe [input].asm, [output].bin

#### Compiling SH2 Code With ASMSH ####
asmsh.exe  /i /o #+ /o psh2 /o w+ /p [input].asm, [output].bin

#### Compiling 68K Code With ASM68K ####
asm68k.exe /o op+ /o os+ /o ow+ /o oz+ /o oaq+ /o osq+ /o omq+ /p /o ae- [input].asm, [output].bin

#### Debugging ROM With MAME ####
mame64.exe -verbose -debug -nofilter -waitvsync -window 32x -cart [path/to/compiledROM.bin]
