;----------------------------------------------------
;GameBoy ASM Project 0.2 - 2020.11.01
;Input handling
;by Saad Azim
;----------------------------------------------------

;Tested, and works in MAME and BGB.

;Variables
_BG_COLOR:      equ %00000011     
_BG_COLOR_ALT:  equ %00000110
                    ;|:|:|:|:
                    ;|:|:|:Color 00 (fill/transparency)
                    ;|:|:Color 01
                    ;|:Color 02
                    ;Color 03

;Using HRAM to store screen scroll values, and input status
_SCREEN_SCROLL: equ $80

_PALETTE_BG:    equ $82

_INPUT_RAW:     equ $8e
_INPUT_HOLD:    equ $8f

cpu gbz80

  ;TNIASM uses "|" as a line separator, which can be used to tidy up the multi-line header code into something more compact. :D

  ;Vector table                                                                     $0000-$00FF
  jp rst0   | ds $05,$00 | jp rst1    | ds $05,$00 | jp rst2  | ds $05,$00 | jp rst3   | ds $05,$00
  jp rst4   | ds $05,$00 | jp rst5    | ds $05,$00 | jp rst6  | ds $05,$00 | jp rst7   | ds $05,$00
  jp VBlank | ds $05,$00 | jp LCDStat | ds $05,$00 | jp Timer | ds $05,$00 | jp Serial | ds $05,$00
  jp Joypad | ds $0100-$,$00

  nop | jp CodeStart        ;Beginning of code execution point                      $0100-$0103

  ;Nintendo logo                                                                    $0104-$0133
  db $ce,$ed,$66,$66,$cc,$0d,$00,$0b,$03,$73,$00,$83,$00,$0c,$00,$0d
  db $00,$08,$11,$1f,$88,$89,$00,$0e,$dc,$cc,$6e,$e6,$dd,$dd,$d9,$99
  db $bb,$bb,$67,$63,$6e,$0e,$ec,$cc,$dd,$dc,$99,$9f,$bb,$b9,$33,$3e

  db "GB INIT 0.2    "      ;Game Title 15 bytes                                    $0134-$0142

  ;GB only, New Licensee Code (H,L), No SGB, ROM only, 32Kb ROM, 0Kb (S)RAM, JP     $0143-$014A
  dw $0000,$0000,$0000,$0000

  db $01                    ;Licensee code: Nintendo :3                                   $014B

  ;Mask ROM version: 0, Header Checksum, ROM Checksum (H,L)                         $014C-$014F
  dw $0000,$0000

CodeStart:                                                                               ;$0150
  di                        ;Disable interrupt
  xor a
  ld de,$0000

  ;Clear RAM ($C000-$DFFF)
  ld bc,$2000
  ld hl,$c000
loopClearRAM:
  ldi (hl),a
  dec c
  jp nz,loopClearRAM
  dec b
  jp nz,loopClearRAM

  ;Clear HRAM ($FF80-$FFFE)
  ld c,$80
loopClearHRAM:
  ldh (c),a
  inc c
  jp nz,loopClearHRAM

  ;Setup stack pointer
  ld sp,$ffff

  ;Set initial input values. The input register returns 0 for a button being pressed, and 1 for a button not being pressed. So $FF means there's no input
  ld a,$ff
  ldh (_INPUT_RAW),a
  ldh (_INPUT_HOLD),a

  ;Save the BG color in HRAM
  ld a,_BG_COLOR
  ldh (_PALETTE_BG),a

  ;Enable VBlank
  ld a,%00000001
       ;|:|:|:|:
       ;|:|:|:|VBlank
       ;|:|:|:LCDStat
       ;|:|:|Timer Overflow
       ;|:|:End of serial I/O transfer
       ;|:|Transition high>low pins p10-p13 ...?
       ;xxx
  ldh ($ff),a

  ei                        ;Enable Interrupts

MainLoop:
  xor a
  ldh a,(_INPUT_RAW)        ;Load input status from HRAM
  ld b,a                    ;Copy input status to B
                            ;Register B will be used to test for various input bits from here on out

  ld a,_BG_COLOR            ;Load _BG_COLOR. If A is not pressed this will be used
  bit 0,b                   ;Check bit 0 in register B, which corresponds to button A
  jr nz,skipPalette         ;If bit 0 in B is Not Zero, then Button A is not pressed,
                            ;so palette change is skipped
  ld a,_BG_COLOR_ALT        ;Otherwise load the alternate palette for background

skipPalette:
  ldh (_PALETTE_BG),a       ;Store whichever palette color was loaded in A to HRAM :D

  ;Since B, Select, and start are not used in this project, it's time to check if right is pressed
  bit 4,b
  jr nz,checkLeft
  ldh a,(_SCREEN_SCROLL+1)
  sub a,1
  ldh (_SCREEN_SCROLL+1),a

checkLeft:
  bit 5,b
  jr nz,checkUp
  ldh a,(_SCREEN_SCROLL+1)
  add a,1
  ldh (_SCREEN_SCROLL+1),a

checkUp:
  bit 6,b
  jr nz,checkDown
  ldh a,(_SCREEN_SCROLL)
  add a,1
  ldh (_SCREEN_SCROLL),a

checkDown:
  bit 7,b
  jr nz,allDone
  ldh a,(_SCREEN_SCROLL)
  sub a,1
  ldh (_SCREEN_SCROLL),a

allDone:
  halt                      ;Wait for interrupt
  nop

  jr MainLoop

VBlank:
  push af
  push bc

  ;Update scroll
  ldh a,(_SCREEN_SCROLL)    ;Y scroll value for background, stored at $80
  ldh ($42),a
  ldh a,(_SCREEN_SCROLL+1)  ;X scroll value for background, stored at $81
  ldh ($43),a

  ;Update BG color palette
  ldh a,(_PALETTE_BG)
  ldh ($47),a

  ;Check button input
  ld a,%00100000
       ;|:|:|:|:
       ;|:|:|:|Right||Button A, read only
       ;|:|:|:Left||Button B, read only
       ;|:|:|Up||Select, read only
       ;|:|:Down||Start, read only
       ;|:Button/Direction select (00==?||01==Direction||10==Button||11==Reset?)
       ;xx
  ldh ($00),a               ;Set port ($FF00) to check for button input
  ldh a,($00)
  ldh a,($00)               ;Read data repeatedly to stabilize input
                            ;GB Programming Manual suggests reading this twice
  and $0f                   ;Mask out unwantded bits (7-4)

  swap a                    ;Swap the nybbles, so the bit order is RLUDxxxx
  ld b,a                    ;Copy A to B

  ld a,%00010000
  ldh ($00),a               ;Set port to check for directional input

  ldh a,($00)
  ldh a,($00)
  ldh a,($00)
  ldh a,($00)
  ldh a,($00)
  ldh a,($00)               ;Read data repeatedly to stabilize input
                            ;GB Programming Manual suggests reading this six times
  and $0f
  or b                      ;Logical OR with B, to merge the data in A and B into A

  ld b,a                    ;Copy A to B
  ldh a,(_INPUT_RAW)        ;Load input status from previous frame
  or d                      ;Get inputs that have changed since last read
                            ;Logical OR only outputs 0 if both of the bits being compared are also 0

  ldh (_INPUT_HOLD),a       ;Store the result in HRAM
  ld a,b                    ;Copy B to A, since B still contains the results of the current input read
  ldh (_INPUT_RAW),a        ;Store the result in HRAM

  ld a,%00110000            ;Reset port
  ldh ($00),a

  pop bc
  pop af

;Unused interrupt pointers
LCDStat: | Timer: | Serial:| Joypad: | rst0: | rst1: | rst2: | rst3: | rst4: | rst5: | rst6: | rst7:

  reti

  ds $8000-$,$00            ;Pad to 32k