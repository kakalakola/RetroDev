;----------------------------------------------------
;GameBoy ASM Project 0.1 - 2020.10.30
;A proper boot sequence
;by Saad Azim
;----------------------------------------------------

;Tested, and works in MAME and BGB.

;Define variables
_PALETTE_BG:    equ %00000011
                    ;|:|:|:|:
                    ;|:|:|:Color 00 (fill/transparency)
                    ;|:|:Color 01
                    ;|:Color 02
                    ;Color 03

;A place in RAM to store the modified screen scroll values
_SCREEN_SCROLL: equ $c000

cpu gbz80

;Vector table. For this project, they're commented with EXTRA details, so they can double as reference. In latter projects, they will be compacted into something a lot shorter.

  jp rst0                   ;RST (ReSTart) 0                                        $0000-$0002
  ds $05,$00                ;Padding                                                $0003-$0007

  jp rst1                   ;RST 1                                                  $0008-$000F
  ds $05,$00

  jp rst2                   ;RST 2                                                  $0010-$0017
  ds $05,$00

  jp rst3                   ;RST 3                                                  $0018-$001F
  ds $05,$00

  jp rst4                   ;RST 4                                                  $0020-$0027
  ds $05,$00

  jp rst5                   ;RST 5                                                  $0028-$002F
  ds $05,$00

  jp rst6                   ;RST 6                                                  $0030-$0037
  ds $05,$00

  jp rst7                   ;RST 7                                                  $0038-$003F
  ds $05,$00

  jp VBlank                 ;VBlank Interrupt                                       $0040-$0047
  ds $05,$00

  jp LCDStat                ;LCDStat is the GB equivalent to HBlank                 $0048-$004F
                            ;But all the documentation found online refers to it as LCD Stat, so it's less confusing to refer to this as LCD Stat, rather than HBlank
  ds $05,$00

  jp Timer                  ;Timer                                                  $0050-$0057
  ds $05,$00

  jp Serial                 ;Serial link interrupt                                  $0058-$005F
  ds $05,$00

  jp Joypad                 ;Joypad interrupt                                       $0060-$0067
  ;ds $05,$00

  ds $0100-$,$00            ;Padding until $0100                                    $0066-$00FF

  nop                       ;Beginning of code execution                            $0100-$0103
  jp CodeStart

  ;Scrolling Nintendo logo. GB won't start if this is altered                       $0104-$0133
  db $ce,$ed,$66,$66,$cc,$0d,$00,$0b,$03,$73,$00,$83,$00,$0c,$00,$0d
  db $00,$08,$11,$1f,$88,$89,$00,$0e,$dc,$cc,$6e,$e6,$dd,$dd,$d9,$99
  db $bb,$bb,$67,$63,$6e,$0e,$ec,$cc,$dd,$dc,$99,$9f,$bb,$b9,$33,$3e

  db "GB INIT 0.1    "      ;Game Title 15 bytes                                    $0134-$0142

  db $00                    ;GameBoy compatibility, GB only                               $0143
                            ;$00==GameBoy
                            ;$80==GameBoy and GameBoy Color
                            ;$C0==GameBoy Color only

  ;Read as licensee code only if $014b==$33, else $00
  db $00                    ;High nibble of new licensee code                             $0144
  db $00                    ;Low nibble of new licensee code                              $0145

  db $00                    ;Super GameBoy not supported                                  $0146
                            ;$00==No Super GameBoy support
                            ;$03==Super GameBoy support

  db $00                    ;Cartridge type, ROM only                                     $0147
                            ;$00==ROM only
                            ;$01==MBC1 (Memory Bank Controller)
                            ;$02==MBC1+RAM
                            ;$03==MBC1+SRAM
                            ;$05==MBC2
                            ;$06==MBC2+SRAM
                            ;$08==ROM+RAM
                            ;$09==ROM+SRAM
                            ;$0B==MMM01
                            ;$0C==MMM01+RAM
                            ;$0D==MMM01+SRAM
                            ;$0F==MBC3+RTC w/ battery
                            ;$10==MBC3+Timer+SRAM
                            ;$11==MBC3
                            ;$12==MBC3+RAM
                            ;$13==MBC3+SRAM
                            ;$19==MBC5 Officially declared to support GameBoy Color's high-speed mode
                            ;$1A==MBC5+RAM
                            ;$1B==MBC5+SRAM
                            ;$1C==MBC5+Rumble
                            ;$1D==MBC5+Rumble+RAM
                            ;$1E==MBC5+Rumble+SRAM
                            ;$20==MBC6
                            ;$22==MBC7+Sensor+Rumble+SRAM
                            ;$FC==Pocket Camera
                            ;$FD==Bandai TAMA5
                            ;$FE==HuC3
                            ;$FF==HuC1+SRAM

  db $00                    ;ROM size 32kb                                                $0148
                            ;$00==32Kb
                            ;$01==64Kb, 16Kb x 4 banks
                            ;$02==128Kb, 16Kb x 8 banks
                            ;$03==256Kb, 16Kb x 16 banks
                            ;$04==512Kb, 16Kb x 32 banks
                            ;$05==1024Kb, 16Kb x 64 banks
                            ;$06==2048Kb, 16Kb x 128 banks
                            ;$07==4096Kb, 16Kb x 256 banks
                            ;$08==8192Kb, 16Kb x 512 banks
                            ;$52==1152Kb, 16Kb x 72 banks...?
                            ;$53==1280Kb, 16Kb x 80 banks...?
                            ;$54==1536Kb, 16Kb x 96 banks...?

  db $00                    ;RAM size 0kb                                                 $0149
                            ;$00==0Kb
                            ;$01==2Kb
                            ;$02==8Kb
                            ;$03==32Kb, 8Kb x 4 banks
                            ;$04==128Kb, 8Kb x 16 banks
                            ;$05==64Kb, 8Kb x 8 banks (used by Pokémon Crystal [J])

  db $00                    ;Destination code ($00==JP,$01==!JP)                          $014A

  db $01                    ;Old licensee code ($33==Check $0145-$0146)                   $014B
                            ;$01==Nintendo :3

  db $00                    ;Mask ROM version number                                      $014C

  db $00                    ;Header check, check & set with rgbfix                        $014D

  dw $0000                  ;Checksum (H,L), set with rgbfix                        $014E-$014F

;Start of code $0150
CodeStart:                  ;Initialization routines

  di                        ;Disable interrupt

  xor a                     ;Bitwise XOR on A with A, essentially resulting in $00...?
                            ;Resets Carry Negative-carry and Half-carry flag
                            ;Detects Parity/oVerflow flag
                            ;Zero/Sign flags are affected as defined

  ld de,$0000               ;Only DE is cleared manually, because there rest of
                            ;the registers are immediately used :D
  ;**NOTE**
  ;DEC/INC BC/DE/HL doesn't set the zero flag when the 16-bit number hits 0, so INC/DEC BC followed by JP NZ label doesn't work.

  ;Manually clear RAM ($C000-$DFFF)
  ld bc,$2000
  ld hl,$c000
loopClearRAM:               ;Need to use 2 8-bit loops instead of a single 16-bit one -_-
  ldi (hl),a                ;Load the value in A, to the address in HL. Then increment HL by 1
  dec c                     ;DECrement C by 1
  jp nz,loopClearRAM        ;JumP, if the result is Not Zero, to loopClearRAM
  dec b                     ;DECrement B by 1...
  jp nz,loopClearRAM

  ;Manually clear HRAM ($FF80-$FFFE)
  ld c,$80                  ;Using a single register for address AND count :D
loopClearHRAM:
  ldh (c),a                 ;LDH (C),A == LD ($FF00+C),A
                            ;BGB shows the disassembly as LD ($FF00+C),A
                            ;MAME shows the disassembly as LD ($FFnn),A where nn is the value of C
  inc c                     ;INCrement C by 1
  jp nz,loopClearHRAM

  ;The above loop ends up writing 0 to the interrupt register ($FFFF), but it's not an issue since the interrupts are disabled at this point anyway.

  ld sp,$ffff               ;This sets up the stack pointer at $FFFE for ... reasons...?

  ;Loading palette color from a variable now. :D
  ld a,_PALETTE_BG
  ldh ($47),a               ;LDH ($47),A == LD ($FF00+47),A
                            ;BGB shows the disassembly as LD ($FF00+$80),A
                            ;MAME shows the disassembly as LD ($FF80),A

  ;Enable VBlank
  ld a,%00000001
       ;|:|:|:|:
       ;|:|:|:|VBlank
       ;|:|:|:LCDStat
       ;|:|:|Timer Overflow
       ;|:|:End of serial I/O transfer
       ;|:|Transition high>low pins p10-p13 ...?
       ;xxx
  ldh ($ff),a               ;Store a at the address of Interrupt Control. ($FFFF)

  ei                        ;Enable Interrupts (EI automatically sets IM 1...?)

MainLoop:
  xor a                     ;Clear A and all flags
  ld hl,_SCREEN_SCROLL      ;Making address wrangling a little easier with HL.
                            ;Load $c000 into HL

  ld a,(hl)                 ;Load the value stored at $C000 into A
  add a,$01                 ;Add 1 to A
  ld (hl),a                 ;Store the value back to $C000

  inc hl                    ;Increase the value in HL, so it now points to $C001
  inc (hl)                  ;Increase the value stored at the address pointed to by HL
                            ;So the value at $C000 gets increased by 1

  ;Basically it's two different ways of doing the same thing. The first approach keeps track of overflow, and is better for mathematical stuff.
  
  ;The second approach is a simple increment, and comparatively faster if all you're looking to do is increment a value by 1.

  halt                      ;Wait for interrupt, effectively synching the loop to VBlank
  nop                       ;Putting a NOP after HALT because of a potential hardware glitch.

  ;If interrupts are disabled, HALT doesn't stop the CPU. However it DOES skip the instruction after HALT. So if interrupts were disabled for some reason or another, JR MainLoop might not execute, or get interpreted as a different instruction

  jr MainLoop               ;Jump Relative, to MainLoop. JR takes 12 CPU cycles to complete, JP takes 16.

VBlank:
  push af                   ;Back up registers to stack
  push hl

  ld hl,$c000
  ldi a,(hl)                ;LoaD and Increment. The value at (HL) is loaded to A, then HL is increased by 1
  ldh ($42),a               ;Store the value in A at $FF42, the background Y-scroll register

  ld a,(hl)
  ldh ($43),a               ;Store the value in A at $FF43, the background X-scroll register

  pop hl                    ;Restore registers from stack
  pop af

;Unused interrupts. The labels all effectively point RETI below.
LCDStat:
Timer:
Serial:
Joypad:

rst0:
rst1:
rst2:
rst3:
rst4:
rst5:
rst6:
rst7:

  reti                      ;RETurn and enable Interrupts

  ds $8000-$,$00            ;Pad to 32k