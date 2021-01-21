;----------------------------------------------------
;GameBoy ASM Project 0.9 - 2020.11.07
;Drawing and moving meta sprites
;by Saad Azim
;----------------------------------------------------

;Tested, and works in MAME and BGB.

;Variables
_SPRITE_COLOR_0:     equ %11000000
_SPRITE_COLOR_1:     equ %01001000
_SPRITE_COLOR_1_ALT: equ %00000100

_BG_COLOR:           equ %00110110
_BG_COLOR_ALT:       equ %01001000
_BG_COLOR_HBLANK:    equ %10000101
                        ;|:|:|:|:
                        ;|:|:|:Color 00 (fill/transparency)
                        ;|:|:Color 01
                        ;|:Color 02
                        ;Color 03

;Using an address in RAM as the VBlank & HBlank subroutines. This way, it's no longer a static block of code in ROM
_VBLANK:             equ $c000
_LCD_STAT:           equ $c004

_SPRITE_DATA:        equ $c100 ;A place in RAM to cache sprite data, to be copied to OAM during VBlank
_SPRITE_01_DATA:     equ $c108 ;Start of attributes for meta sprite 01...
_SPRITE_02_DATA:     equ $c110
_SPRITE_03_DATA:     equ $c118

;Using HRAM to store screen scroll values, and input status
_SCREEN_SCROLL:      equ $80

_PALETTE_BG:         equ $82
_PALETTE_SPRITE_0:   equ $83
_PALETTE_SPRITE_1:   equ $84

_FLAGS:              equ $8d

_INPUT_RAW:          equ $8e
_INPUT_HOLD:         equ $8f

_OAM_ROUTINE:        equ $ff90 ;Using OAM DMA to copy sprite data from RAM

cpu gbz80

  ;TNIASM uses "|" as a line separator, which can be used to tidy up the multi-line header code into something more compact. :D

  ;Vector table                                                                     $0000-$00FF
  jp rst0    | ds $05,$00 | jp rst1      | ds $05,$00 | jp rst2  | ds $05,$00 | jp rst3   | ds $05,$00
  jp rst4    | ds $05,$00 | jp rst5      | ds $05,$00 | jp rst6  | ds $05,$00 | jp rst7   | ds $05,$00
  jp _VBLANK | ds $05,$00 | jp _LCD_STAT | ds $05,$00 | jp Timer | ds $05,$00 | jp Serial | ds $05,$00
  jp Joypad  | ds $0100-$,$00

  nop | jp CodeStart        ;Beginning of code execution point                      $0100-$0103

  ;Nintendo logo                                                                    $0104-$0133
  db $ce,$ed,$66,$66,$cc,$0d,$00,$0b,$03,$73,$00,$83,$00,$0c,$00,$0d
  db $00,$08,$11,$1f,$88,$89,$00,$0e,$dc,$cc,$6e,$e6,$dd,$dd,$d9,$99
  db $bb,$bb,$67,$63,$6e,$0e,$ec,$cc,$dd,$dc,$99,$9f,$bb,$b9,$33,$3e

  db "GB INIT 0.9    "      ;Game Title 15 bytes                                    $0134-$0142

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

  ;Copy OAM DMA code to HRAM. The code *has* to be executed from HRAM.
  ld hl,oamDMA              ;Source address
  ld b,oamDMAEnd-oamDMA     ;Size
  ld c,_OAM_ROUTINE and $ff ;Destination address, same as LD C,$90
loopCopyDMARoutine:
  ldi a,(hl)
  ldh (c),a
  inc c
  dec b
  jr nz,loopCopyDMARoutine

  ;Copy sprite data to RAM
  ld hl,sprites
  ld b,$20                  ;8 sprites, 4 bytes per sprite
  ld de,_SPRITE_DATA
loopCopySpriteData:
  ldi a,(hl)
  ld (de),a
  inc de
  dec b
  jr nz,loopCopySpriteData

  ;Setup stack pointer
  ld sp,$ffff

  ;VRAM in Game Boy should only be accessed by the CPU when the LCD is turned off. The LCD should ONLY be turned off during VBlank, otherwise it will PERMANENTLY DAMAGE THE SCREEN ON ACTUAL HADWARE.

  ld hl,_VBLANK             ;Using an alternate VBlank subroutine to disable LCD, and copy tiles & map
  ld  a,$c3                 ;$C3 is the byte code for JP
  ldi (hl),a
  ld a,updateVRAM and $ff
  ldi (hl),a
  ld a,updateVRAM>>8
  ldi (hl),a                ;JP instructions are encoded as JP LOW BYTE HIGH BYTE

  ld a,%00000001            ;Enable VBlank, enable interrupts, wait for VBlank
  ldh ($ff),a
  ei
  halt
  nop

  ;Having copied tile & map data to VRAM, boot should continue as usual

  ;Copy VBlank & LCDStat (HBlank) code to RAM
  ld hl,VBlankJump
  ld b,LCDStatEnd01-VBlankJump
  ld de,_VBLANK
loopCopyInterrupts:
  ldi a,(hl)
  ld (de),a
  inc de
  dec b
  jr nz,loopCopyInterrupts

  ;Initialize registers
  ld hl,bootInit
  ld b,bootInitEnd/2-bootInit/2
loopbootInit:
  ldi a,(hl)
  ld c,(hl)
  ldh (c),a
  inc hl
  dec b
  jr nz,loopbootInit

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
  ;Also, ADD and SUB swapped because it looks like the sprites are moving "backwards" otherwise
  
  bit 4,b
  jr nz,checkLeft
  ldh a,(_SCREEN_SCROLL+1)
  add a,1
  ldh (_SCREEN_SCROLL+1),a

  ;Update sprite X position for left half
  ld a,(_SPRITE_01_DATA+1)
  sub a,1
  ld (_SPRITE_01_DATA+1),a

  ;Update sprite X position for right half
  ld a,(_SPRITE_01_DATA+5)
  sub a,1
  ld (_SPRITE_01_DATA+5),a

  ld a,(_SPRITE_02_DATA+1)
  sub a,1
  ld (_SPRITE_02_DATA+1),a

  ld a,(_SPRITE_02_DATA+5)
  sub a,1
  ld (_SPRITE_02_DATA+5),a

  ld a,(_SPRITE_03_DATA+1)
  sub a,1
  ld (_SPRITE_03_DATA+1),a

  ld a,(_SPRITE_03_DATA+5)
  sub a,1
  ld (_SPRITE_03_DATA+5),a

  ;Update meta sprite 0 X flip
  ld a,(_SPRITE_DATA+3)
  res 5,a
  ld (_SPRITE_DATA+3),a
  ld (_SPRITE_DATA+7),a

  ;Update meta sprite 0 tiles
  ld a,$08
  ld (_SPRITE_DATA+2),a
  ld a,$0a
  ld (_SPRITE_DATA+6),a

checkLeft:
  bit 5,b
  jr nz,checkUp
  ldh a,(_SCREEN_SCROLL+1)
  sub a,1
  ldh (_SCREEN_SCROLL+1),a

  ;Update sprite X position to move with background
  ld a,(_SPRITE_01_DATA+1)
  add a,1
  ld (_SPRITE_01_DATA+1),a

  ld a,(_SPRITE_01_DATA+5)
  add a,1
  ld (_SPRITE_01_DATA+5),a

  ld a,(_SPRITE_02_DATA+1)
  add a,1
  ld (_SPRITE_02_DATA+1),a

  ld a,(_SPRITE_02_DATA+5)
  add a,1
  ld (_SPRITE_02_DATA+5),a

  ld a,(_SPRITE_03_DATA+1)
  add a,1
  ld (_SPRITE_03_DATA+1),a

  ld a,(_SPRITE_03_DATA+5)
  add a,1
  ld (_SPRITE_03_DATA+5),a

  ;Update meta sprite 0 X flip, and tiles
  ld a,(_SPRITE_DATA+3)
  set 5,a
  ld (_SPRITE_DATA+3),a
  ld (_SPRITE_DATA+7),a
  ld a,$0a
  ld (_SPRITE_DATA+2),a
  ld a,$08
  ld (_SPRITE_DATA+6),a

checkUp:
  bit 6,b
  jr nz,checkDown
  ldh a,(_SCREEN_SCROLL)
  sub a,1
  ldh (_SCREEN_SCROLL),a

  ;Update sprite Y position to move with background
  ld a,(_SPRITE_01_DATA)
  add a,1
  ld (_SPRITE_01_DATA),a
  ld (_SPRITE_01_DATA+4),a

  ld a,(_SPRITE_02_DATA)
  add a,1
  ld (_SPRITE_02_DATA),a
  ld (_SPRITE_02_DATA+4),a

  ld a,(_SPRITE_03_DATA)
  add a,1
  ld (_SPRITE_03_DATA),a
  ld (_SPRITE_03_DATA+4),a

checkDown:
  bit 7,b
  jr nz,allDone
  ldh a,(_SCREEN_SCROLL)
  add a,1
  ldh (_SCREEN_SCROLL),a

  ;Update sprite Y position to move with background
  ld a,(_SPRITE_01_DATA)
  sub a,1
  ld (_SPRITE_01_DATA),a
  ld (_SPRITE_01_DATA+4),a

  ld a,(_SPRITE_02_DATA)
  sub a,1
  ld (_SPRITE_02_DATA),a
  ld (_SPRITE_02_DATA+4),a

  ld a,(_SPRITE_03_DATA)
  sub a,1
  ld (_SPRITE_03_DATA),a
  ld (_SPRITE_03_DATA+4),a

allDone:

  jp MainLoop

VBlank:
  push af
  push bc

  ld a,%11110111            ;Enable sprites
  ldh ($40),a

  jp _OAM_ROUTINE
endOAM:

  ;Update scroll
  ldh a,(_SCREEN_SCROLL)
  ldh ($42),a
  ldh a,(_SCREEN_SCROLL+1)
  ldh ($43),a

  ;Update sprite & BG color palettes
  ldh a,(_PALETTE_BG)
  ldh ($47),a
  ldh a,(_PALETTE_SPRITE_0)
  ldh ($48),a
  ldh a,(_PALETTE_SPRITE_1)
  ldh ($49),a

  ;Reset HBlank routine
  ld hl,LCDStat01
  ld b,LCDStatEnd01-LCDStat01
  ld de,_LCD_STAT
loopCopyHBlank:
  ldi a,(hl)
  ld (de),a
  inc de
  dec b
  jr nz,loopCopyHBlank

  ;Initialize line counter to trigger LCDStat01 (HBlank)
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

;Interrupt routines to be copied to to RAM
VBlankJump:
  jp VBlank
VBlankJumpEnd:
  db $00                    ;Padding, so LCDStat01 starts at $c004
LCDStat01:
  ;This section needs to be dealt with ASAP, so only AF is backed up at start
  push af
  ld a,_BG_COLOR_HBLANK     ;Change background color when HBlank occurs
  ldh ($47),a

  ;Having changed the BG colors, it's time to setup the next HBlank
  push hl
  push bc
  push de

  ;Copy routine to run during the next HBlank
  ld hl,LCDStat02
  ld b,LCDStatEnd02-LCDStat02
  ld de,_LCD_STAT
loopCopyHBlank02:
  ldi a,(hl)
  ld (de),a
  inc de
  dec b
  jr nz,loopCopyHBlank02

  ;Modify the line counter, so it looks like the sprites are going "behind" the window. :D
  ld a,$80
  ldh ($45),a

  pop de
  pop bc
  pop hl
  pop af

  reti
LCDStatEnd01:

LCDStat02:
  push af

  ld a,_BG_COLOR            ;Reset BG color
  ldh ($47),a

  ld a,%11110101            ;Disable sprites
  ldh ($40),a

  pop af
  reti
LCDStatEnd02:

;DMA routine, to be copied to HRAM
oamDMA:
  ld a,_SPRITE_DATA>>8
  ldh ($46),a               ;The DMA start register is $FF46. Writing a the higy byte of
                            ;the DMA source address starts the DMA
  ld a,$40                  ;Wait for DMA to finish via a loop
waitDMA:
  dec a
  jr nz,waitDMA
  jp endOAM
oamDMAEnd:

;Unused interrupt pointers
Timer: | Serial:| Joypad: | rst0: | rst1: | rst2: | rst3: | rst4: | rst5: | rst6: | rst7:
  reti

updateVRAM:                 ;There's not much of a point in backing up any registers here

  ;Enable LCD Control Register, disable VBlank
  ld a,%00000010
       ;|:|:|:|:
       ;|:|:|:|vBlank
       ;|:|:|:LCD Control enable
       ;|:|:|Timer Overflow
       ;|:|:End of serial I/O transfer
       ;|:|Transition high>low pins p10-p13 ...?
       ;xxx
  ldh ($ff),a              ;Store a at the address of Interrupt Control

  ;LCD Control - Disable everything
  ld a,%00000000
       ;|:|:|:|:
       ;|:|:|:|Enable BG (and Window) (off||on)
       ;|:|:|:Enable Sprite (off||on)
       ;|:|:|Sprite Size (8x8||8x16)
       ;|:|:BG Tile Map Location ($9800-$9bff||$9c00-$9fff)
       ;|:|BG Tile Location ($8800-$97ff||$8000-$8fff)
       ;|:Enable Window (off||on)
       ;|Window Tile Map Location ($9800-$9bff||$9c00-$9fff)
       ;Enable LCD (off||on)
  ldh ($40),a

  ;LCD Control Status - Disable everything
  ld a,%00000000
       ;|:|:|:|:
       ;|:|:|:xx - Mode flag, read only
       ;|:|:|Coincidence Flag (LYC!=LY||LYC==LY), read only
       ;|:|:Mode 0, HBlank interrupt check (disable||enable)
       ;|:|Mode 1, VBlank interrupt check (disable||enable)
       ;|:Mode 2, OAM interrupt check (disable||enable)
       ;|LYC=LY Coincidencd (scanline counter) interrupt check (disable||enable)
       ;x
  ldh ($41),a

  ;Copy tiles to VRAM
  ld hl,tiles
  ld c,$f0                  ;8-bit loop, for now
  ld de,$8010
loopCopyTile:
  ldi a,(hl)
  ld (de),a
  inc de
  dec c
  jp nz,loopCopyTile

  ;Copy tile and window maps to VRAM
  ld hl,tileMap
  ld bc,$0800
  ld de,$9800
loopCopyTileMap:
  ldi a,(hl)
  ld (de),a
  inc de
  dec c
  jp nz,loopCopyTileMap
  dec b
  jp nz,loopCopyTileMap

  ld a,%11110111            ;Modified to display 8x16 sprites
       ;|:|:|:|:
       ;|:|:|:|Enable BG (and Window) (off||on)
       ;|:|:|:Enable Sprite (off||on)
       ;|:|:|Sprite Size (8x8||8x16)
       ;|:|:BG Tile Map Location ($9800-$9bff||$9c00-$9fff)
       ;|:|BG Tile Location ($8800-$97ff||$8000-$8fff)
       ;|:Enable Window (off||on)
       ;|Window Tile Map Location ($9800-$9bff||$9c00-$9fff)
       ;Enable LCD (off||on)
  ldh ($40),a

  ;Set window Y position
  ld a,$80
  ldh ($4a),a

  ;Set window X position
  ld a,$7                   ;Setting Window X position to 1-6, and 166 ($A6) is unreliable
                            ;Setting Window X positon to 0 causes the causes windows to scroll with screen scroll X%8

  ldh ($4b),a

  ;LCD Control Status
  ld a,%00000000
       ;|:|:|:|:
       ;|:|:|:xx - Mode flag, read only
       ;|:|:|Coincidence Flag (LYC!=LY||LYC==LY), read only
       ;|:|:Mode 0, HBlank interrupt check (disable||enable)
       ;|:|Mode 1, VBlank interrupt check (disable||enable)
       ;|:Mode 2, OAM interrupt check (disable||enable)
       ;|LYC=LY Coincidencd (scanline counter) interrupt check (disable||enable)
       ;x
  ldh ($41),a

  ;Mode flag:
  ; - 00 - HBlank in progress
  ; - 01 - VBlank in progress
  ; - 10 - OAM being accessed by LDC Controller
  ; - 11 - Both OAM and VRAM are being accessed by LCD Controller

  ret                       ;Don't want to enable interrupts yet :D

;Data block (format: value,register)
bootInit:
  db %10101111,$24          ;Initialize audio channel volume
     ;|:|:|:|:
     ;|:|:|Volume for left channel
     ;|:|:Left channel (0==off||1==on)
     ;|Volume for right channel
     ;Right channel (0==off||1==on)

  db %10001000,$25          ;Initialize audio channels
     ;|:|:|:|:
     ;|:|:|:|Enable audio channel 1 R
     ;|:|:|:Enable audio channel 2 R
     ;|:|:|Enable audio channel 3 R
     ;|:|:Enable audio channel 4 R
     ;|:|Enable audio channel 1 L
     ;|:Enable audio channel 2 L
     ;|Enable audio channel 3 L
     ;Enable audio channel 4 L
  
  ;Channel 4 (noise) configuration
  db %00000001,$20          ;Sound duration
     ;|:|:|:|:
     ;|:Sound length, lower value == longer sound
     ;xx

  db %00111010,$21          ;Envelope settings
     ;|:|:|:|:
     ;|:|:|Number of envelope sweep (0==Stop Operation)
     ;|:|:Envelope mode (0==Atteunuate||1==Amplify)
     ;Initial volume of envelope

  ;Atteunuation lowers the volume with each envelope sweep. Amplication increases the volume. Number of envelope sweeps determing how long it takes the sound to go from full volume to zero with atteunuation, or zero to full volume with amplification. 

  db %00101010,$22          ;Noise settings
     ;|:|:|:|:
     ;|:|:|Dividing ratio frequency (roughness)
     ;|:|:Polynomial counter step (0==15 bit||1==7 bit, sounds electronic)
     ;Shift clock frequency of the polynomial counter (pitch), 1110 and 1111 are prohibited

  db %11000000,$23          ;Sound initialization & counter enable/disable
     ;|:|:|:|:
     ;|:xxxxxx
     ;|Counter (0==Sound plays forever||1==Sound plays for the duration of time set with $FF20)
     ;Initialize (1==Restart sound)

  db $ff,_INPUT_RAW         ;Set initial input values
  db $ff,_INPUT_HOLD

  ;Save palettes in HRAM
  db _BG_COLOR,_PALETTE_BG
  db _SPRITE_COLOR_0,_PALETTE_SPRITE_0
  db _SPRITE_COLOR_1,_PALETTE_SPRITE_1

  db $40,$44                ;Initialize line counter

  db %01000000,$41          ;Enable scanline counter
     ;|:|:|:|:
     ;|:|:|:xx - Mode flag, read only
     ;|:|:|Coincidence Flag (LYC!=LY||LYC==LY), read only
     ;|:|:Mode 0, LCD Stat interrupt check (disable||enable)
     ;|:|Mode 1, VBlank interrupt check (disable||enable)
     ;|:Mode 2, OAM interrupt check (disable||enable)
     ;|LYC=LY Coincidencd (scanline counter) interrupt check (disable||enable)
     ;x

  db %00000011,$ff          ;Enable interrupts
     ;|:|:|:|:
     ;|:|:|:|VBlank
     ;|:|:|:LCDStat
     ;|:|:|Timer Overflow
     ;|:|:End of serial I/O transfer
     ;|:|Transition high>low pins p10-p13 ...?
     ;xxx
bootInitEnd:

;Tile data, $F0 bytes
tiles:
  db $7e,$7e,$ff,$ef,$ff,$d7,$ff,$83,$ff,$bb,$ff,$bb,$ff,$bb,$7e,$7e ;A
  db $7e,$7e,$ff,$c7,$ff,$db,$ff,$c3,$ff,$dd,$ff,$dd,$ff,$c3,$7e,$7e ;B
  db $7e,$7e,$ff,$e3,$ff,$df,$ff,$df,$ff,$df,$ff,$df,$ff,$e3,$7e,$7e ;C
  db $7e,$7e,$ff,$c7,$ff,$db,$ff,$dd,$ff,$dd,$ff,$dd,$ff,$c3,$7e,$7e ;D

  db $7e,$ff,$ff,$dd,$ff,$dd,$ff,$d5,$ff,$d5,$ff,$c9,$ff,$dd,$7e,$ff ;W

  db $7e,$7e,$ff,$c3,$ff,$df,$ff,$c7,$ff,$df,$ff,$df,$ff,$c3,$7e,$7e ;E
  db $7e,$7e,$ff,$c3,$ff,$df,$ff,$c7,$ff,$df,$ff,$df,$ff,$df,$7e,$7e ;F

  db $7f,$7f,$c0,$bf,$bf,$ff,$bf,$f0,$bf,$f0,$bf,$f3,$bf,$f3,$bf,$f0 ;16x16 E Tile 0
  db $bf,$f0,$bf,$f3,$bf,$f3,$bf,$f0,$bf,$f0,$bf,$ff,$c0,$bf,$7f,$7f ;16x16 E Tile 1
  db $fe,$fe,$03,$fd,$fd,$ff,$fd,$0f,$fd,$0f,$fd,$ff,$fd,$ff,$fd,$3f ;16x16 E Tile 2
  db $fd,$3f,$fd,$ff,$fd,$ff,$fd,$0f,$fd,$0f,$fd,$ff,$03,$fd,$fe,$fe ;16x16 E Tile 3

  db $7f,$7f,$c0,$bf,$bf,$ff,$bf,$f0,$bf,$f0,$bf,$f3,$bf,$f3,$bf,$f0 ;16x16 F Tile 0
  db $bf,$f0,$bf,$f3,$bf,$f3,$bf,$f3,$bf,$f3,$bf,$ff,$c0,$bf,$7f,$7f ;16x16 F Tile 1
  db $fe,$fe,$03,$fd,$fd,$ff,$fd,$0f,$fd,$0f,$fd,$ff,$fd,$ff,$fd,$3f ;16x16 F Tile 2
  db $fd,$3f,$fd,$ff,$fd,$ff,$fd,$ff,$fd,$ff,$fd,$ff,$03,$fd,$fe,$fe ;16x16 F Tile 3

sprites:
  ;    Y    X  Tile   RYXP     (pRiority, Y flip, X flip, Palette)
  db $50, $50,  $08, %00000000 ;E, left half, using palette 0
  db $50, $58,  $0a, %00000000 ;E, right half, using palette 0
                               ;Bit 0 of tile index is ignored when in 8x16 sprite mode. So setting tile index to 9 has the same effect as setting tile index to 8

  db $43, $37,  $08, %00010000 ;E, using palette 1
  db $43, $3f,  $0a, %00010000 ;E, using palette 1
  db $69, $45,  $0c, %00010000 ;F, using palette 1
  db $69, $4d,  $0e, %00010000 ;F, using palette 1
  db $48, $6a,  $0e, %00110000 ;F, using palette 1, X flip
  db $48, $72,  $0c, %00110000 ;F, using palette 1, X flip

;Tile map, $0400 bytes. The tile map area in Game Boy VRAM 32x32 tiles (256x256 pixels) in size. Though only 160x144 pixels are visible at any given time.
tileMap:
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4
  db 3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4

windowMap:
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5
  db 5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5

  ds $8000-$,$00            ;Pad to 32k