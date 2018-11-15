# Getting Started #

Each of these ASM files are self contained programs that generate working ROMs.

  * genesis_init_00.asm - A working MegaDrive/Genesis ROM
  * genesis_init_01.asm - A properly initialized MD/Gen. ROM with a pair of counters in RAM

#### Compiling Z80 Code With TNIASM ####
tniasm.exe [input].asm [output].z80

#### Compiling 68K Code With ASM68K ####
asm68k.exe /o op+ /o os+ /o ow+ /o oz+ /o oaq+ /o osq+ /o omq+ /p /o ae- [input].asm, [output].bin

#### Running ROM With MAME ####
mame64.exe -verbose -debug -nofilter -waitvsync -window genesis -cart [path/to/compiledROM.bin]
