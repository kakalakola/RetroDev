;----------------------------------------------------
;GameBoy ASM Project 0.4 - 2020.11.02
;Making some noise
;by Saad Azim
;----------------------------------------------------

;Tested, and works in MAME and BGB.

;Variables
_BG_COLOR:        equ %00000011     
_BG_COLOR_ALT:    equ %00000110
_BG_COLOR_HBLANK: equ %00001000
                      ;|:|:|:|:
                      ;|:|:|:Color 00 (fill/transparency)
                      ;|:|:Color 01
                      ;|:Color 02
                      ;Color 03

;Using HRAM to store screen scroll values, and input status
_SCREEN_SCROLL:   equ $80

_PALETTE_BG:      equ $82

_FLAGS:           equ $8d

_INPUT_RAW:       equ $8e
_INPUT_HOLD:      equ $8f

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

  db "GB INIT 0.4    "      ;Game Title 15 bytes                                    $0134-$0142

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

  ;Initialize audio channel volume
  ld a,%10101111
       ;|:|:|:|:
       ;|:|:|Volume for left channel
       ;|:|:Left channel (0==off||1==on)
       ;|Volume for right channel
       ;Right channel (0==off||1==on)
  ldh ($24),a

  ;Initialize audio channels
  ld a,%10001000
       ;|:|:|:|:
       ;|:|:|:|Enable audio channel 1 R
       ;|:|:|:Enable audio channel 2 R
       ;|:|:|Enable audio channel 3 R
       ;|:|:Enable audio channel 4 R
       ;|:|Enable audio channel 1 L
       ;|:Enable audio channel 2 L
       ;|Enable audio channel 3 L
       ;Enable audio channel 4 L
  ldh ($25),a
  
  ;Channel 4 (noise) configuration
  ;Sound duration
  ld a,%00000001
       ;|:|:|:|:
       ;|:Sound length, lower value == longer sound
       ;xx
  ldh ($20),a

  ;Envelope settings
  ld a,%00101010
       ;|:|:|:|:
       ;|:|:|Number of envelope sweep (0==Stop Operation)
       ;|:|:Envelope mode (0==Atteunuate||1==Amplify)
       ;Initial volume of envelope
  ldh ($21),a

  ;Atteunuation lowers the volume with each envelope sweep. Amplication increases the volume. Number of envelope sweeps determing how long it takes the sound to go from full volume to zero with atteunuation, or zero to full volume with amplification. 

  ;Noise settings
  ld a,%00111001
       ;|:|:|:|:
       ;|:|:|Dividing ratio frequency (roughness)
       ;|:|:Polynomial counter step (0==15 bit||1==7 bit, sounds electronic)
       ;Shift clock frequency of the polynomial counter (pitch), 1110 and 1111 are prohibited
  ldh ($22),a

  ;Sound initialization & counter enable/disable
  ld a,%11000000
       ;|:|:|:|:
       ;|:xxxxxx
       ;|Counter (0==Sound plays forever||1==Sound plays for the duration of time set with $FF20)
       ;Initialize (1==Restart sound)
  ldh ($23),a

  ;Set initial input values
  ld a,$ff
  ldh (_INPUT_RAW),a
  ldh (_INPUT_HOLD),a

  ;Save the BG color in HRAM
  ld a,_BG_COLOR
  ldh (_PALETTE_BG),a

  ld a,$40                  ;Initialize line counter to trigger LCDStat (HBlank) at line $40
  ldh ($44),a

  ;Enable scanline counter
  ld a,%01000000
       ;|:|:|:|:
       ;|:|:|:xx - Mode flag, read only
       ;|:|:|Coincidence Flag (LYC!=LY||LYC==LY), read only
       ;|:|:Mode 0, LCD Stat interrupt check (disable||enable)
       ;|:|Mode 1, VBlank interrupt check (disable||enable)
       ;|:Mode 2, OAM interrupt check (disable||enable)
       ;|LYC=LY Coincidencd (scanline counter) interrupt check (disable||enable)
       ;x
  ldh ($41),a

  ;Enable VBlank
  ld a,%00000011
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

  ;Check _FLAGS
  ldh a,(_FLAGS)
  bit 0,a
  jr nz,MainLoop

  set 0,a
  ldh (_FLAGS),a

  ;Check input
  ldh a,(_INPUT_RAW)
  ld b,a 

  ;Change BG color if A is pressed
  ld a,_BG_COLOR
  bit 0,b
  jr nz,skipPalette
  ld a,_BG_COLOR_ALT

skipPalette:
  ldh (_PALETTE_BG),a

  ;Play a sound clip B button is pressed, and no sound is currently playing
  bit 1,b                   
  jr nz,checkRight

  ;Check to see if the sound is already playing. Without this bit of code, holding down B continuously resets the sound, creating something rather grating... -_-
  ldh a,($26)
  bit 3,a
  jr nz,checkRight

  ;Register $FF26
  ;00000000
  ;|:|:|:|:
  ;|:|:|:|Channel 1 read-only (0==No sound||1==Sound is currently playing)
  ;|:|:|:Channel 2 read-only (0==No sound||1==Sound is currently playing)
  ;|:|:|Channel 3 read-only (0==No sound||1==Sound is currently playing)
  ;|:|:Channel 4 read-only (0==No sound||1==Sound is currently playing)
  ;|xxx
  ;All sound (R/W) (0==off||1==On)

  ;Reset timer and restart noise channel only if the audio has finished playing
  ld a,%00000001
  ldh ($20),a
  ld a,%11000000
  ldh ($23),a

checkRight:
  ;Set BG scroll values if the d-pad was pressed in any direction
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

  jr MainLoop

VBlank:
  push af
  push bc

  ;Update scroll
  ldh a,(_SCREEN_SCROLL)
  ldh ($42),a
  ldh a,(_SCREEN_SCROLL+1)
  ldh ($43),a

  ;Update BG color palette
  ldh a,(_PALETTE_BG)
  ldh ($47),a

  ;Initialize line counter to trigger LCDStat (HBlank)
  ld a,$4c
  ldh ($45),a

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

  and $0f
  swap a
  ld b,a

  ld a,%00010000
  ldh ($00),a               ;Set port to check for directional input

  ldh a,($00)
  ldh a,($00)
  ldh a,($00)
  ldh a,($00)
  ldh a,($00)
  ldh a,($00)               ;Read data repeatedly to stabilize input

  and $0f
  or b

  ;Process input bits before storing
  ld b,a
  ldh a,(_INPUT_RAW)
  or d

  ldh (_INPUT_HOLD),a
  ld a,b
  ldh (_INPUT_RAW),a

  ld a,%00110000            ;Reset port
  ldh ($00),a

  ldh a,(_FLAGS)
  res 0,a                   ;RESet bit 0 in A (setting it to 0)
  ldh (_FLAGS),a

  pop bc
  pop af

  reti

LCDStat:
  push af
  ld a,_BG_COLOR_HBLANK     ;Change background color when HBlank occurs
  ldh ($47),a
  pop af
  reti

;Unused interrupt pointers
Timer: | Serial:| Joypad: | rst0: | rst1: | rst2: | rst3: | rst4: | rst5: | rst6: | rst7:

  reti

  ds $8000-$,$00            ;Pad to 32k