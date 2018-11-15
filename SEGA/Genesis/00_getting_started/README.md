# Getting Started #

Each of these ASM files are self contained programs that generate working ROMs.

  * genesis_init_00.asm - A working MegaDrive/Genesis ROM
  * genesis_init_01.asm - A properly initialized MD/Gen. ROM with a pair of counters in RAM

## ASM68K Options ##
asm68k.exe /o op+ /o os+ /o ow+ /o oz+ /o oaq+ /o osq+ /o omq+ /p /o ae- [input].asm, [output].bin
