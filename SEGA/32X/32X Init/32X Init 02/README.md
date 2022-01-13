#32X Init 01

Putting a picture on screen.
There are still a few issues to work out though.
  - A few random bits seem to get copied to frame buffer
  - Line table addresses might have a range limitation
    - Using an address of 0x8c00 & higher switches the horizontal resolution to 256 pixels (?)

----

#### Compiling Z80 Code With TNIASM ####
tniasm.exe [input].asm, [output].bin

#### Compiling SH2 Code With ASMSH ####
asmsh.exe  /i /o #+ /o psh2 /o w+ /p [input].asm, [output].bin

#### Compiling 68K Code With ASM68K ####
asm68k.exe /o op+ /o os+ /o ow+ /o oz+ /o oaq+ /o osq+ /o omq+ /p /o ae- [input].asm, [output].bin

#### Debugging ROM With MAME ####
mame64.exe -verbose -debug -nofilter -waitvsync -window 32x -cart [path/to/compiledROM.bin]
